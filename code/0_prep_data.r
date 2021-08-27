#' Basic data cleaning are done in this file.
#' 
#' Function `data_cleaning` is to choose the columns we need from the original data, 
#' modify the data type, add new columns we need and etc.
#' Function `data_prep` is to merge different seasons of data we need.

library(tidyverse)
library(lubridate)


data_cleaning <- function(...) {  # Input could be more than one data frame
  df <- bind_rows(...)  # merge all input data frames into one
  
  # The target variable y is score
  # each game has two scores, one for the home team and the other for the away team
  # hence we construct two data frames, one for home team scores and the other for away team scores

  y.h <- df$FTHG # home team score
  y.a <- df$FTAG # away team score

  home <- as.factor(df$HomeTeam)  # home team name
  away <- as.factor(df$AwayTeam)  # away team name
  
  date <- df$Date  # the dates
  
  G = nrow(df)  # the total number of games
  
  # add new column `at.home` to indicate if a team played at home
  at.home.h <- rep(1,G)
  at.home.a <- rep(0,G)
  
  # calculate the points of attack team (0/1/3)
  points.h <- as.factor(3*(y.h > y.a) + 1*(y.h == y.a))
  points.a <- as.factor(3*(y.h < y.a) + 1*(y.h == y.a))
  
  # Output is consist of two data frames
  # df.h.bl records the score of the home team in each game
  # df.a.bl records the score of the away team in each game
  df.h.bl <- data.frame(y=y.h,
                        date,
                        attack=home, 
                        defense=away,
                        at.home=at.home.h,
                        points = points.h
                        )
  df.a.bl <- data.frame(y=y.a,
                        date,
                        attack=away,
                        defense=home, 
                        at.home=at.home.a,
                        points = points.a
                        )
  # return the two data frames in a list
  return(
    list(
      df.h.bl=df.h.bl,
      df.a.bl=df.a.bl
      )
    )
}

data_prep <- function(n = 1){  # n represent the number of seasons we need
  # Load 10 seasons data, 2011/2012 ~ 2020/2021
  data.ls = list.files("data")[1:10]
  
  df = data.frame()
  for (i in 1:10) {
    data = read.csv(paste("data",data.ls[i], sep="/"))
    data = data %>% drop_na(FTHG)
    if (all(nchar(data$Date)==10)) {
      data$Date = as.Date(data$Date,'%d/%m/%Y')  # change date form from 'dd-mm-yyyy' to 'yyyy-mm-dd'
    } else if (all(nchar(data$Date)==8)) {
      data$Date = as.Date(data$Date,'%d/%m/%y')  # change date form from 'dd-mm-yy' to 'yyyy-mm-dd'
    } else {
      stop(paste("Please check the date form of",data.ls[i]))
    }
    data$sea = rep(i, nrow(data))
    df = bind_rows(df,data)
  }
  
  # Clean the data we need
  df = df %>% filter(sea > (10-n))
  ls.df = data_cleaning(df)

  # Merge the two data frames we get after data cleaning by row
  df = bind_rows(ls.df[[1]], ls.df[[2]])
  
  # Add an index column
  df = df %>% mutate(ID = c(1:n()), .before=y) %>% mutate(
    sea = paste(year(date) -  (month(date)<8), year(date) + (month(date)>7), sep = "-"), .before=date
  )
  
  # Add a column to indicate if a game is the last game in a season for each team
  df = df %>% group_by(sea, attack) %>% arrange(sea, attack, date) %>% mutate(
    is.last.game = c(rep(FALSE, n()-1), TRUE)
  ) %>% ungroup() %>% arrange(ID)
  
  # Return the merged dataframe 
  # The top half are scores of home team in each game arranged chronologically
  # The bottom half are scores of away team in each game arranged chronologically
  return(df)
}

if (sys.nframe() == 0) {
  df <- data_prep()
  View(df)
}