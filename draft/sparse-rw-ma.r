# rw1
Q1 <- function(n=n.all_days){
  Q = toeplitz(c(2,-1, rep(0,n-3),-1))
  Q[1,1] = Q[n,n] = 1
  Q[n,1] = Q[1,n] = 0
  
  bQ = Q
  for(i in 1:(Ts-1)) {
    bQ = bdiag(bQ,Q)
  }
  Q = as(bQ, "sparseMatrix")
  return(Q)
}

# rw2
Q2 <- function(n=n.all_days){
  Q = toeplitz(c(6,-4,1, rep(0,n-5),-4,1))
  Q[1,] = c(1,-2,1,rep(0,n-3))
  Q[n,] = c(rep(0,n-3),1,-2,1)
  Q[2,] = c(-2,5,-4,1,rep(0,n-4))
  Q[n-1,] = c(rep(0,n-4),1,-4,5,-2)
  
  bQ = Q
  for(i in 1:(Ts-1)) {
    bQ = bdiag(bQ,Q)
  }
  Q = as(bQ, "sparseMatrix")
  return(Q)
}


# blocked rw1

bQ1 <- function() {
  teamls = levels(data$attack)
  xy = NULL
  bQ = NULL
  
  for (tm in teamls) {
    df = data %>% filter(attack==tm) %>% arrange(date)
    n = length(df$date)
    xy = c(xy,paste(df$date,df$attack))
    Q = toeplitz(c(2,-1, rep(0,n-3),-1))
    Q[1,1] = Q[n,n] = 1
    Q[n,1] = Q[1,n] = 0
    if (is.null(bQ)){
      bQ = Q
    } else{
      bQ = bdiag(bQ,Q) 
    }
  }
  
  Q.a = as(bQ, "sparseMatrix")
  id.a = xy
  z.a = factor(paste(data$date,data$attack), levels = id.a)
  
  teamls = levels(data$defense)
  xy = NULL
  bQ = NULL
  
  for (tm in teamls) {
    df = data %>% filter(defense==tm) %>% arrange(date)
    n = length(df$date)
    xy = c(xy,paste(df$date,df$attack))
    Q = toeplitz(c(2,-1, rep(0,n-3),-1))
    Q[1,1] = Q[n,n] = 1
    Q[n,1] = Q[1,n] = 0
    if (is.null(bQ)){
      bQ = Q
    } else{
      bQ = bdiag(bQ,Q) 
    }
  }
  
  Q.d = as(bQ, "sparseMatrix")
  id.d = xy
  z.d = factor(paste(data$date,data$defense), levels = id.d)
  
  ls = list(Q.a=Q.a, Q.d=Q.d, id.a=id.a, id.d=id.d)
  
  return(ls)
}


# blocked rw2

bQ2 <- function() {
  teamls = levels(data$attack)
  xy = NULL
  bQ = NULL
  
  for (tm in teamls) {
    df = data %>% filter(attack==tm) %>% arrange(date)
    n = length(df$date)
    xy = c(xy,paste(df$date,df$attack))
    Q = toeplitz(c(6,-4,1, rep(0,n-5),-4,1))
    Q[1,] = c(1,-2,1,rep(0,n-3))
    Q[n,] = c(rep(0,n-3),1,-2,1)
    Q[2,] = c(-2,5,-4,1,rep(0,n-4))
    Q[n-1,] = c(rep(0,n-4),1,-4,5,-2)
    if (is.null(bQ)){
      bQ = Q
    } else{
      bQ = bdiag(bQ,Q) 
    }
  }
  
  Q.a = as(bQ, "sparseMatrix")
  id.a = xy
  z.a = factor(paste(data$date,data$attack), levels = id.a)
  
  teamls = levels(data$defense)
  xy = NULL
  bQ = NULL
  
  for (tm in teamls) {
    df = data %>% filter(defense==tm) %>% arrange(date)
    n = length(df$date)
    xy = c(xy,paste(df$date,df$attack))
    Q = toeplitz(c(6,-4,1, rep(0,n-5),-4,1))
    Q[1,] = c(1,-2,1,rep(0,n-3))
    Q[n,] = c(rep(0,n-3),1,-2,1)
    Q[2,] = c(-2,5,-4,1,rep(0,n-4))
    Q[n-1,] = c(rep(0,n-4),1,-4,5,-2)
    if (is.null(bQ)){
      bQ = Q
    } else{
      bQ = bdiag(bQ,Q) 
    }
  }
  
  Q.d = as(bQ, "sparseMatrix")
  id.d = xy
  z.d = factor(paste(data$date,data$defense), levels = id.d)
  
  ls = list(Q.a=Q.a, Q.d=Q.d, id.a=id.a, id.d=id.d)
  
  return(ls)
}
