# save(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv)

library(tidyverse)
library(lubridate)
library(INLA)

# source("code/0_prep_data.r")
# source("code/5_effect_of_managers.r")
# # 
# df = data_prep(n = 10)
# mg = read.csv("data/PL_managers.csv")
# df = add_manager_info(df, mg)
# save(df, file="data/tidyData.rdata")

load("data/tidyData.rdata")


G = nrow(df)/2L
Tm.ls = sort(unique(df$attack))
Ts = length(Tm.ls)


