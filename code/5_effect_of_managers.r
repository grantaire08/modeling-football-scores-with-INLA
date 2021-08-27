# rm(list = ls())
source("code/0_prep_data.r")


mg_cleaning <- function(mg) {
  # Tidy the data
  mg[mg == "Present*"] = "12-Jul-21"  # Use "2021-07-12" to represent "Present*"
  mg = mg %>% mutate(
    Start = as.Date(Start, "%d-%b-%y"),
    End = as.Date(End, "%d-%b-%y")
  ) %>% arrange(Team,desc(Start))
  
  # Make the team name consistent
  mg.tm = mg$Team
  mg.tm[mg.tm == "Blackburn Rovers"] = "Blackburn"
  mg.tm[mg.tm == "Birmingham City"] = "Birmingham"
  mg.tm[mg.tm == "Brighton & Hove Albion"] = "Brighton"
  mg.tm[mg.tm == "Bolton Wanderers"] = "Bolton"
  mg.tm[mg.tm == "Cardiff City"] = "Cardiff"
  mg.tm[mg.tm == "Huddersfield Town"] = "Huddersfield"
  mg.tm[mg.tm == "Hull City"] = "Hull"
  mg.tm[mg.tm == "Leeds United"] = "Leeds"
  mg.tm[mg.tm == "Leicester City"] = "Leicester"
  mg.tm[mg.tm == "Manchester City"] = "Man City"
  mg.tm[mg.tm == "Manchester United"] = "Man United"
  mg.tm[mg.tm == "Newcastle United"] = "Newcastle"
  mg.tm[mg.tm == "Norwich City"] = "Norwich"
  mg.tm[mg.tm == "Queens Park Rangers"] = "QPR"
  mg.tm[mg.tm == "Queens Park Rangers"] = "QPR"
  mg.tm[mg.tm == "Stoke City"] = "Stoke"
  mg.tm[mg.tm == "Swansea City"] = "Swansea"
  mg.tm[mg.tm == "Tottenham Hotspur"] = "Tottenham"
  mg.tm[mg.tm == "West Bromwich Albion"] = "West Brom"
  mg.tm[mg.tm == "West Ham United"] = "West Ham"
  mg.tm[mg.tm == "Wigan Athletic"] = "Wigan"
  mg.tm[mg.tm == "Wolverhampton Wanderers"] = "Wolves"
  mg$Team = mg.tm
  
  return(mg)
}

# Add manager info to original data
add_manager_info <- function(df, mg) {
  # Clean the data
  mg = mg_cleaning(mg)
  
  # Choose the data in the same time range of our data
  # date.min = min(as.Date(df$date))
  # mg = mg %>% filter(End >= date.min)
  
  # Choose the team existing in our data
  # Add the manager information to our data frame
  tm.ls = sort(unique(df$attack))
  mg = mg %>% filter(Team %in% tm.ls)   # 12 teams out 23 teams
  
  mg.tm.ls = tm.ls[tm.ls %in% sort(unique(mg$Team))]
  G = nrow(df)/2L

  manager.start = rep(NA, 2*G)
  manager.end = rep(NA, 2*G)
  manager = rep(NA, 2*G)
  for (tm in mg.tm.ls) {
    mg.tm = mg %>% filter(Team == tm) %>% select(Name, Start, End)
    data.tm = df %>% filter(attack == tm) %>% mutate(date = as.Date(date))
    for (i in 1:nrow(data.tm)) {
      date = data.tm$date[i]
      id = data.tm$ID[i]
      if (any((date >= mg.tm$Start) & (date <= mg.tm$End))) {
        # Note for some team, there might more than one managers in the same time
        manager[id] = mg.tm$Name[(date >= mg.tm$Start) & (date <= mg.tm$End)][1]
        start.date = mg.tm$Start[(date >= mg.tm$Start) & (date <= mg.tm$End)][1]
        # end.date = mg.tm$End[(date >= mg.tm$Start) & (date <= mg.tm$End)]
        manager.start[id] = as.character(start.date)  # without saving as char, the date will be transformed as number
        # manager.end[id] = as.character(end.date) 
      }
    }
  }
  
  # convert string to date
  manager.start = as.Date(manager.start)
  # manager.end = as.Date(manager.end)
  
  # Is the manager in the honeymoon period (assume one month)
  is.hm = as.integer((df$date - manager.start) <= 30)
  
  df$attack.manager = manager
  df$is.hm.a = is.hm
  # df$attack.manager.start = manager.start
  # df$attack.manager.end = manager.end
  # df$attack.manager.tenure = as.integer(df$date - df$attack.manager.start)
  
  df$defense.manager = c(manager[(G+1):(2*G)],manager[1:G])
  df$is.hm.d = c(is.hm[(G+1):(2*G)],is.hm[1:G])
  # df$defense.manager.start = c(manager.start[(G+1):(2*G)],manager.start[1:G])
  # df$defense.manager.end = c(manager.end[(G+1):(2*G)],manager.end[1:G])
  # df$defense.manager.tenure = as.integer(df$date - df$defense.manager.start)
  
  df = df %>% group_by(attack, attack.manager) %>% arrange(attack, date) %>%
    mutate(is.new.ma.a = c(1,rep(0, n()-1)) , .after="attack.manager") %>% ungroup() %>% arrange(ID)
  df = df %>% group_by(defense, defense.manager) %>% arrange(defense, date) %>%
    mutate(is.new.ma.d = c(1,rep(0, n()-1)) , .after="defense.manager") %>% ungroup() %>% arrange(ID)
  df = df %>% relocate(is.hm.a, .after = is.new.ma.a)
  df = df %>% relocate(is.hm.d, .after = is.new.ma.d)
  
  return(df)
}



if (sys.nframe() == 0) {
  # Read the data
  mg = read.csv("data/PL_managers.csv")
  df = data_prep(n=11)
  
  # Add the information about manager
  df = add_manager_info(df, mg)
  
  # Visualization 1
  
  # tenure>4000 is very rare so we don't consider those data
  bound = 1000
  data = df %>% filter(attack.manager.tenure < bound) %>% filter(defense.manager.tenure < bound)
  
  # The scores vs tenure
  data %>% ggplot() +
    geom_jitter(aes(attack.manager.tenure, y, color=attack),
               alpha=.5, height = .1, width = .1) +
    geom_jitter(aes(defense.manager.tenure, -y, color=defense),
               alpha=.5, height = .1, width = .1) +
    geom_hline(yintercept = 0, color = "red") +
    xlab("Manager Tenure") +
    theme(legend.position = "none")
  
  # The simplified version: only consider the last game in a season for each team
  ggplot() +
    geom_jitter(data = data %>% filter(is.last.game == TRUE),
               aes(attack.manager.tenure, y),
               height = .1, width = .1) +
    geom_jitter(data = data %>% filter(is.last.game == TRUE),
               aes(defense.manager.tenure, -y),
               height = .1, width = .1) +
    geom_hline(yintercept = 0, color = "red") +
    xlab("Manager Tenure")
  
  # Another simplified version: focus on one certain team
  tm = "Chelsea"
  ggplot() +
    geom_point(data = data %>% filter(attack == tm),
               aes(attack.manager.tenure, y, color=attack.manager),
               alpha=.5) +
    geom_point(data = data %>% filter(defense == tm),
               aes(defense.manager.tenure, -y, color=defense.manager),
               alpha=.5) +
    geom_hline(yintercept = 0, color = "red") +
    xlab("Manager Tenure") +
    labs(color="manager")
  
  # Don't consider y<0
  ## All data
  df %>% ggplot() +
    geom_jitter(aes(attack.manager.tenure, y, color = attack.manager), alpha=.5) +
    geom_point(aes(attack.manager.tenure, y), alpha=.5, size=.1) +
    theme(legend.position = "none")
  
  ## Last game in a season for each team
  df %>% filter(is.last.game == TRUE) %>% ggplot() +
    geom_point(aes(attack.manager.tenure, y), alpha=.2, color="red") +
    geom_jitter(aes(attack.manager.tenure, y), position=position_jitter(.2))
  
  
  # Visualization 2
  
  # Don't consider points<0
  ## All data
  df %>% ggplot() +
    geom_jitter(aes(attack.manager.tenure, points, color = attack.manager), alpha=.5) +
    geom_point(aes(attack.manager.tenure, points), alpha=.5, size=.1) +
    theme(legend.position = "none") +
    xlab("Manager Tenure")
  
  ## Last game in a season for each team
  df %>% filter(is.last.game == TRUE) %>% ggplot() +
    geom_point(aes(attack.manager.tenure, points), alpha=.2, color="red") +
    geom_jitter(aes(attack.manager.tenure, points), position=position_jitter(.2)) +
    xlab("Manager Tenure")
  
  # Chelsea
  
  tm = "Chelsea"
  ggplot() +
    geom_jitter(data = data %>% filter(attack == tm),
               aes(attack.manager.tenure, points, color=attack.manager),
               alpha=.5, height = .1, width = .1) +
    geom_hline(yintercept = 0, color = "red") +
    xlab("Manager Tenure") +
    labs(color="manager")
}
