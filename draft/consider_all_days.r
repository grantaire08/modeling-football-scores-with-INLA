library("INLA")
library(tidyverse)
library(lubridate)

source("proc_data_2.r")
source("sparse-rw-ma.r")


## model 1

Q.a = Q.d = Q1()
# Q.a = Q.d = Q2()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  )


result1 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result1)

Q.a = Q.d = Q2()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  )


result2 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result2)

## model 2

Q.ls = bQ1()
# Q.ls = bQ2()
Q.a = Q.ls$Q.a
Q.d = Q.ls$Q.d
id.a = Q.ls$id.a
id.d = Q.ls$id.d

z.a = factor(paste(data$date,data$attack), levels = id.a)
z.d = factor(paste(data$date,data$defense), levels = id.d)
data =  mutate(data, ID.a = z.a, ID.d = z.d) %>% relocate(ID.a, ID.d, .after = 1)


formula <- y ~ 1 +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005)))
  )


result2 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result2)



# source("proc_data.r")
# 
# for (df in alldata) {
#   Ts = length(unique(df$attack))
#   print(Ts)
# }

# two seas, no ht.score
# rw1
# -2037.64 for all game days
# -2050.39  for all days
# -2027.15 for different game days to different teams
# rw2
# -2139.85 for all game days
# -2513.95 for all days
# -1969.58 for different game days to different teams


## ab=c(100,100)
ab = c(100,100)

Q.a = Q.d = Q1()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result3 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result3)

Q.a = Q.d = Q2()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result4 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result4)


## ab = c(1,100)
ab = c(1,100)

Q.a = Q.d = Q1()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result5 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result5)

Q.a = Q.d = Q2()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result6 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result6)



## ab = c(1,100)
ab = c(100,1e-5)

Q.a = Q.d = Q1()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result7 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result7)

Q.a = Q.d = Q2()
id.a = id.d = id

formula <- y ~ 1 + ht.score +
  f(ID.a, model='generic0', Cmatrix = Q.a, values = id.a,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.d, values = id.d, 
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = ab))
  )


result8 <- inla(formula, data = data, family='poisson',
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))

summary(result8)