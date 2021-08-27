names(result1)
'
[1] "names.fixed"                 "summary.fixed"               "marginals.fixed"            
 [4] "summary.lincomb"             "marginals.lincomb"           "size.lincomb"               
 [7] "summary.lincomb.derived"     "marginals.lincomb.derived"   "size.lincomb.derived"       
[10] "mlik"                        "cpo"                         "po"                         
[13] "waic"                        "model.random"                "summary.random"             
[16] "marginals.random"            "size.random"                 "summary.linear.predictor"   
[19] "marginals.linear.predictor"  "summary.fitted.values"       "marginals.fitted.values"    
[22] "size.linear.predictor"       "summary.hyperpar"            "marginals.hyperpar"         
[25] "internal.summary.hyperpar"   "internal.marginals.hyperpar" "offset.linear.predictor"    
[28] "model.spde2.blc"             "summary.spde2.blc"           "marginals.spde2.blc"        
[31] "size.spde2.blc"              "model.spde3.blc"             "summary.spde3.blc"          
[34] "marginals.spde3.blc"         "size.spde3.blc"              "logfile"                    
[37] "misc"                        "dic"                         "mode"                       
[40] "neffp"                       "joint.hyper"                 "nhyper"                     
[43] "version"                     "Q"                           "graph"                      
[46] "ok"                          "cpu.used"                    "all.hyper"                  
[49] ".args"                       "call"                        "model.matrix" 
'

df = result1$summary.hyperpar
df
names(df)

plot(x=df[,1], y=df[,2], type='l')


tm = "Arsenal"
result = result3
result = result4

## re1

df.a = (result$summary.random$ID.a) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.a$date)
gamedays = as.Date((data %>% filter(attack == tm))$date)
is.gameday = rep(F,length(alldays))
for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gameday[i] = T
  }
}
df.a = mutate(df.a,
  is.gameday,
  date = alldays
  )
ggplot() +
  geom_point(data=df.a, aes(x = date, y = mean, alpha=is.gameday), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Attack strength of",tm))

df.d = (result$summary.random$ID.d) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.d$date)
gamedays = as.Date((data %>% filter(defense == tm))$date)
is.gameday = rep(F,length(alldays))
for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gameday[i] = T
  }
}
df.d = mutate(df.d,
              is.gameday,
              date = alldays
)
ggplot() +
  geom_point(data=df.d, aes(x = date, y = mean, alpha=is.gameday), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Defense strength of",tm))


## re2
result = result2

df.a = (result$summary.random$ID.a) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.a$date)
gamedays = as.Date((data %>% filter(attack == tm))$date)
is.gameday = rep(F,length(alldays))
for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gameday[i] = T
  }
}
df.a = mutate(df.a,
              is.gameday,
              date = alldays
)
ggplot() +
  geom_point(data=df.a, aes(x = date, y = mean, alpha=is.gameday), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Attack strength of",tm))

ggplot() +
  geom_point(data=df.a, aes(x = date, y = mean), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Attack strength of",tm))

df.d = (result$summary.random$ID.d) %>% separate(ID, c("date","team"), sep=11) %>% filter(team == tm)
alldays = as.Date(df.d$date)
gamedays = as.Date((data %>% filter(defense == tm))$date)
is.gameday = rep(F,length(alldays))
for (i in (1:length(alldays))) {
  if (alldays[i] %in% gamedays) {
    is.gameday[i] = T
  }
}
df.d = mutate(df.d,
              is.gameday,
              date = alldays
)
ggplot() +
  geom_point(data=df.d, aes(x = date, y = mean, alpha=is.gameday), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Defense strength of",tm))

ggplot() +
  geom_point(data=df.d, aes(x = date, y = mean), size=2) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title = paste("Defense strength of",tm))


## game scores
df = data.frame(
  date=gamedays,
  score.a=(data %>% filter(attack == tm))$y,
  score.d=-(data %>% filter(defense == tm))$y
  )

ggplot() +
  geom_col(data=df, aes(x=date, y=score.a),fill="cyan3") +
  geom_col(data=df, aes(x=date, y=score.d),fill="tomato") +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  labs(title=paste("Scores of", tm))

bind_rows(df %>% group_by(score.a) %>% count(score.a) %>% ungroup() %>% summarise(score=score.a,n), 
          df %>% group_by(score.d) %>% count(score.d) %>% ungroup() %>% summarise(score=score.d,n=-n)) %>% 
  mutate(is.attack=(n>0)) %>% 
  ggplot() + 
  geom_col(aes(score,n,fill=is.attack))


hist(rgamma(100000, shape=100, rate=5e-5),breaks = 100, probability = T)
# mode = (a-1)/b     
# mean = a/b
# var = a/b^2