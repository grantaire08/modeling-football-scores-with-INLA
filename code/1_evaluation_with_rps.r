rm(list=ls())

# Import the data set
source("code/10_main.r")

# Load the model list
model.ls = list()
i = 1
for (filename in list.files("code/models", full.names = T)){
  model.ls[[i]] = filename
  i = i+1
}

# Load the function that calculates the precision matrix for 'generic0' model
source(model.ls[[1]])

# Define the function to calculate rps
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Initialize the output
rps.ma = matrix(rep(0,6*10), nrow = 10, ncol = 6)
acc.ma = matrix(rep(0,6*10), nrow = 10, ncol = 6)

for (m in c(1:3,5,6)){
  source(model.ls[[m+1]])
  
  for (n in 1:10) {
    #We create a new dataframe where the matches of the last three rounds (30 games) in each season
    # have NA in their response variables y 
    r = 30
    data=df[c(1:(380*n), (G+1):(G+380*n)),]
    data$goals = data$y
    G.tr = nrow(data)/2L
    data[(G.tr-r+1):G.tr,"y"]<-NA
    data[(2*G.tr-r+1):(2*G.tr),"y"]<-NA   
    
    # Train the model with new data
    mdl = m.pois(data)
    print("CPU used:")
    print(mdl$cpu.used)
    print(paste("Model",m,"Trained with",G.tr,"games,",n,"seasons."))
  
    #We obtain the samples from the linear predictors
    nbsamp = 1000
    samp = inla.posterior.sample(nbsamp, mdl)
    predictors = inla.posterior.sample.eval(function(...) {Predictor}, samp)
    
    
    #We will store the scores.samples from the posterior predictive in a matrix scores.samples
  
    scores.samples=matrix(0,nrow=2*r,ncol=nbsamp)
    
    rates.H=exp(predictors[(G.tr-r+1):G.tr,])
    vector.rates.H=as.vector(rates.H)
    scores.samples[1:r,]=matrix(rpois(length(vector.rates.H),vector.rates.H),nrow=r,ncol=nbsamp)
    
    rates.A=exp(predictors[(2*G.tr-r+1):(2*G.tr),])
    vector.rates.A=as.vector(rates.A)
    scores.samples[(r+1):(2*r),]=matrix(rpois(length(vector.rates.A),vector.rates.A),nrow=r,ncol=nbsamp)
    
    # Get the probability of draw and lose (for fact and sample)
    p2 = rowMeans(scores.samples[1:r,] >  scores.samples[(r+1):(2*r),])
    p1 = rowMeans(scores.samples[1:r,] == scores.samples[(r+1):(2*r),])
    p0 = rowMeans(scores.samples[1:r,] <  scores.samples[(r+1):(2*r),])
    
    a2 = as.numeric(df[(G.tr-r+1):G.tr, "y"] >  df[(2*G.tr-r+1):(2*G.tr),"y"])
    a1 = as.numeric(df[(G.tr-r+1):G.tr, "y"] == df[(2*G.tr-r+1):(2*G.tr),"y"])
    a0 = as.numeric(df[(G.tr-r+1):G.tr, "y"] <  df[(2*G.tr-r+1):(2*G.tr),"y"])
    
    # Pass those probabilities to RPS function to calculate RPS
    rps.ma[n,m] = mean(RPS_fun(p0, p1, a0, a1))
    print(paste("RPS is",rps.ma[n,m],"."))
    
    # Compute the accuracy
    scores.obs = data$goals[is.na(data$y)]
    df.pred = (scores.samples[1:r,] >  scores.samples[(r+1):(2*r),])*2 + (scores.samples[1:r,] == scores.samples[(r+1):(2*r),])
    df.true = matrix(rep((scores.obs[1:r] >  scores.obs[(r+1):(2*r)])*2 + (scores.obs[1:r] == scores.obs[(r+1):(2*r)]),1000), nrow=30, ncol=1000)
    acc.ma[n,m] = mean(df.pred == df.true)
    print(paste("Accuracy is",acc.ma[n,m],"."))
    
    print("=========================================================================")
  }
  
  # Check the statics of the validation rps
  rps.stats = list(mean=mean(rps.ma[,m]),sd=sd(rps.ma[,m]),quantile=quantile(rps.ma[,m]))
  print("RPS statics:")
  print(rps.stats)
  print("=========================================================================")
  print("=========================================================================")
}


# rps.ma
apply(rps.ma, 2, mean)
apply(rps.ma, 2, sd)
# 
# acc.ma
apply(acc.ma, 2, mean)
apply(acc.ma, 2, sd)

