# Define the function to calculate rps
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Import the data set
source("code/10_final.r")

# Load the model list
model.ls = list()
i = 1
for (filename in list.files("code/models", full.names = T)){
  model.ls[[i]] = filename
  i = i+1
}
mdl.ls = list()

# Load the function that calculates the precision matrix for 'generic0' model
source(model.ls[[1]])

r = 30
data=df[c((380*8+1):(380*10), (G+380*8+1):(G+380*10)),]
G.tr = nrow(data)/2L
data$goals = data$y
data[(G.tr-r+1):G.tr,"y"]<-NA
data[(2*G.tr-r+1):(2*G.tr),"y"]<-NA   

n = 2
rps.ls = rep(0,6)
acc.ls = rep(0,6)

for (m in c(1:3,5,6)) {

source(model.ls[[m+1]])

# Train the model with new data
mdl = m.pois(data)
mdl.ls[[m]] = mdl
print("CPU used:")
print(mdl$cpu.used)
print(paste("Model",m,"Trained with",G.tr,"games,",n,"seasons."))

#We obtain the samples from the linear predictors
nbsamp = 1000
samp = inla.posterior.sample(nbsamp, mdl)
predictors = inla.posterior.sample.eval(function(...) {Predictor}, samp)

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
rps.ls[m] = mean(RPS_fun(p0, p1, a0, a1))
print(paste("RPS is",rps.ls[m],"."))

# Compute the accuracy
scores.obs = data$goals[is.na(data$y)]
df.pred = (scores.samples[1:r,] >  scores.samples[(r+1):(2*r),])*2 + (scores.samples[1:r,] == scores.samples[(r+1):(2*r),])
df.true = matrix(rep((scores.obs[1:r] >  scores.obs[(r+1):(2*r)])*2 + (scores.obs[1:r] == scores.obs[(r+1):(2*r)]),1000), nrow=30, ncol=1000)
acc.ls[m] = mean(df.pred == df.true)
print(paste("Accuracy is",acc.ls[m],"."))

print("=========================================================================")
}



# png("Figure/Plot2.png", width = 8, height = 4, units = 'in', res = 300)
# # df %>% select(attack, y, points) %>% group_by(attack) %>%
# #   mutate(TGS = sum(y), TP = sum(as.numeric(points))) %>% ungroup() %>%
# #   distinct(attack, TGS, TP) %>% arrange(desc(TGS)) %>%
# #   mutate(attack = factor(attack, levels = attack)) %>%
# #   ggplot(aes(x=attack, y=TGS, fill=TP)) +
# #   geom_bar(stat = "identity", width=.5) +
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# #   xlab("Team") + ylab("Total goals scored") +
# #   scale_fill_gradient("Total points",low="grey", high="black")
# df %>% mutate(y = as.character(y)) %>%
#   ggplot(aes(y)) +
#   geom_bar(stat="count", width = .5) +
#   theme_bw() +
#   xlab("Scores") + ylab("Count")
# 
# dev.off()
# 
# 



# tm.str = mdl$summary.fixed[2:69,1]
# tm.STR = data.frame(team = Tm.ls, attack.str = c(0,tm.str[1:34]), defense.str=c(0,tm.str[35:68]))
# tm.annot = (tm.STR %>% arrange(desc(attack.str)))[c(1:5,33:35),]
# png("Figure/Plot4.png", width = 6, height = 4, units = 'in', res = 300)
# tm.STR %>% ggplot(aes(x=attack.str, y=defense.str)) +
#   geom_point() +
#   geom_point(aes(x=0,y=0), color="red") +
#   annotate("text", x = +.025, y = -0.01, label = "Arsenal", size=3, color="red") +
#   annotate("text", x = tm.annot$attack.str[-2], y = tm.annot$defense.str[-2]-0.01, label = tm.annot$team[-2], size=3) +
#   annotate("text", x = tm.annot$attack.str[2]+0.02, y = tm.annot$defense.str[2]+0.01, label = tm.annot$team[2], size=3) +
#   theme_bw() +
#   xlim(-0.3,0.4) +
#   xlab("Attack strength") + ylab("Defense strength")
# dev.off()

# png("Figure/Plot5.png", width = 6, height = 4, units = 'in', res = 300)
# goals.obs = data$goals[c((G.tr-29):G.tr,(2*G.tr-29):(2*G.tr))]
# goals.pred = apply(scores.samples,1,mean)
# 
# # goals.obs = data$goals
# # goals.pred = goals.obs
# # goals.pred[c((G.tr-29):G.tr,(2*G.tr-29):(2*G.tr))] = apply(scores.samples,1,mean)
# 
# df.goalsden = data.frame(goals = c(goals.obs,goals.pred), 
#                          label = c(rep("observed",length(goals.pred)), rep("predicted",length(goals.pred))) )
# 
# df.goalsden %>% ggplot(aes(goals, color=label)) +
#   geom_density(adjust=2) +
#   theme_bw() +
#   xlab("Goals") + ylab("Density")
# 
# dev.off()

png("Figure/Plot6.png", width = 6, height = 4, units = 'in', res = 300)
goalsdf.obs = data$goals[(G.tr-29):G.tr]-data$goals[(2*G.tr-29):(2*G.tr)]
goalsdf.pred = apply(scores.samples,1,mean)
goalsdf.pred = goalsdf.pred[1:r] - goalsdf.pred[(r+1):(2*r)]

# goals.obs = data$goals
# goals.pred = goals.obs
# goals.pred[c((G.tr-29):G.tr,(2*G.tr-29):(2*G.tr))] = apply(scores.samples,1,mean)

df.goalsdfden = data.frame(goalsdf = c(goalsdf.obs,goalsdf.pred),
                         label = c(rep("observed",length(goalsdf.pred)), rep("predicted",length(goalsdf.pred))) )

df.goalsdfden %>% ggplot(aes(goalsdf, color=label)) +
  geom_density(adjust=2) +
  theme_bw() +
  xlab("Goals difference") + ylab("Density")

dev.off()

png("Figure/Plot7.png", width = 4, height = 4, units = 'in', res = 300)
goals = df$y
goalsdf = goals[1:G] - goals[(G+1):(2*G)]
goalsdf = data.frame( GDF = goals[1:G] - goals[(G+1):(2*G)])
goalsdf %>% ggplot(aes(GDF)) + geom_bar(stat = "count", size=.5) + 
  theme_bw() + xlab("Goals difference") + ylab("Count")
dev.off()
