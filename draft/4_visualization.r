#' Visualize the model parameter 


# Suppress the warnings when using alpha for a discrete variable
options(warn=-1)

data = df

# Choose the model trained and the team interested
# result = result.A
tm = "Arsenal"
date1 = as.Date("2019-12-22")  # The day when new manager arrives
date1.ls = (df %>% mutate(is.new.manager.a) %>% filter(attack == tm & is.new.manager.a == 1))$date

# Plot the attack strength changing trend
df.a = (result$summary.random$ID.a) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.a$date)
gamedays = as.Date((data %>% filter(attack == tm))$date)
is.gd = rep(F,length(alldays))

for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gd[i] = TRUE
  }
}

df.a = mutate(df.a,
              is.gameday = is.gd,
              date = alldays
)

ggplot() +
  geom_point(data=df.a, aes(x = date, y = mean, 
                            # alpha=is.gameday
                            ), size=1) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Attack strength of",tm)) +
  geom_vline(xintercept = date1.ls, linetype="dashed", color = "red") + 
  theme(axis.text.x = element_text(angle = 90))

df.d = (result$summary.random$ID.d) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.d$date)
gamedays = as.Date((data %>% filter(defense == tm))$date)
is.gd = rep(F,length(alldays))

for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gd[i] = T
  }
}


# Plot the defense trength changing trend
df.d = mutate(df.d,
              is.gameday = is.gd,
              date = alldays
)

ggplot() +
  geom_point(data=df.d, aes(x = date, y = mean, 
                            # alpha=is.gameday
                            ), size=1) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Defense strength of",tm)) +
  geom_vline(xintercept = date1.ls, linetype="dashed", color = "red") + 
  theme(axis.text.x = element_text(angle = 90))

