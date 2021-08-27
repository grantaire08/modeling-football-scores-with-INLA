library("INLA")
source("code/0_prep_data.r")

## rw1

# df = data_prep(n=2)

data1 = df %>% group_by(attack, attack.manager) %>% arrange(attack, date) %>% 
  mutate(is.new.manager = c(1, rep(0, n()-1)) , .before=attack.manager) %>% ungroup() %>% arrange(ID)
data2 = df %>% group_by(defense, defense.manager) %>% arrange(defense, date) %>% 
  mutate(is.new.manager = c(1,rep(0, n()-1)) , .before=defense.manager) %>% ungroup() %>% arrange(ID)

is.new.manager.a = data1$is.new.manager
is.new.manager.d = data2$is.new.manager

G = nrow(df)/2L  # the number of total games*2
Ts = length(unique(df$attack))

# Generate the G*G precision matrix
ID = 1:(2*G) # The original order in data (time order)

# The order of block matrix
id = within(df %>% arrange(attack, date), id <- paste(date, attack))$id
df = df %>% mutate(
  ID.a = factor(paste(date, attack), levels=id),
  ID.d = factor(paste(date, defense), levels=id)
)

id.a = as.numeric(df$ID.a)
id.d = as.numeric(df$ID.d)

# Add new column `is.last.game` indicate each team's last game in the data set
is.last.game.a = (df %>% group_by(attack) %>% arrange(date) %>% 
  mutate(is.last.game = c(rep(0, n()-1), 1)) %>% ungroup()%>% arrange(ID))$is.last.game
is.last.game.d = (df %>% group_by(defense) %>% arrange(date) %>% 
                    mutate(is.last.game = c(rep(0, n()-1), 1)) %>% ungroup()%>% arrange(ID))$is.last.game


constr_Q_rw1 <- function(ID, id, is.last.game, is.new.manager, date = df$date) {
  Q = inla.as.sparse(matrix(rep(0, 4*G*G), 2*G, 2*G))
  
  for (i in ID) {
    if ( !is.last.game[i] ) {  # if the current game is the last game of the team, skip the update step
      j = ID[id == (id[i] + 1)]
      cd = as.numeric(date[j] - date[i])*1  # effect from the day difference 
      # cd = 1  # cancel the effect
      efm = is.new.manager[i]*100  # effect from new managers
      # efm = 0  # cancel the effect
      a = b = c = d = 1/(cd+efm)  # precision parameters
      Q[i,i] = Q[i,i] + a
      Q[j,j] = Q[j,j] + b
      Q[i,j] = Q[i,j] - c
      Q[j,i] = Q[j,i] - d
    }
  }
  
  return(Q)
}

Q.a = constr_Q_rw1(ID, id.a, is.last.game.a, is.new.manager.a)
# Q.d = constr_Q(ID, id.d, is.last.game.d, is.new.manager.d)

# Test if the precision matrix is equivalent to the block precision matrix
data = data.frame(ID=ID, id=id.a)
ID.sorted.a = (data %>% arrange(id))$ID
# Q.new = Q.new.a = Q.a[ID.sorted.a, ID.sorted.a]
Q = Q.a[ID.sorted.a, ID.sorted.a]


# data = data.frame(ID=ID, id=id.d)
# ID.sorted.d = (data %>% arrange(id))$ID
# Q.new.d = Q.d[ID.sorted.d, ID.sorted.d]
# 
# all(Q.new.a == Q.new.d)

# Q[c(73:77),c(73:77)]
# all(Q == Q.new)


## rw2

# constr_Q_rw2 <- function(ID, id, is.last.game, is.new.manager, date = df$date) {
#   Q = inla.as.sparse(matrix(rep(0, 4*G*G), 2*G, 2*G))
#   
#   for (i in ID) {
#     if ( !is.last.game[i] ) {  # if the current game is the last game of the team, skip the update step
#       j = ID[id == (id[i] + 1)]
#       k = ID[id == (id[i] + 2)]
#       cd = as.numeric(date[j] - date[i])  # effect from the day difference 
#       # cd = 1  # cancel the effect
#       efm = is.new.manager[i]*100  # effect from new managers
#       # efm = 0  # cancel the effect
#       denom = cd
#       Q[i,i] = Q[i,i] + 1/denom
#       Q[j,j] = Q[j,j] + 4/denom
#       Q[k,k] = Q[k,k] + 1/denom
#       Q[i,j] = Q[i,j] - 2/denom
#       Q[j,i] = Q[j,i] - 2/denom
#       Q[i,k] = Q[i,k] + 1/denom
#       Q[k,i] = Q[k,i] + 1/denom
#       Q[j,k] = Q[j,k] - 2/denom
#       Q[k,j] = Q[k,j] - 2/denom
#     }
#   }
#   
#   return(Q)
# }
# 
# Q.a = constr_Q_rw2(ID, id.a, is.last.game.a, is.new.manager.a)
# # Q.d = constr_Q(ID, id.d, is.last.game.d, is.new.manager.d)
# 
# # Test if the precision matrix is equivalent to the block precision matrix
# data = data.frame(ID=ID, id=id.a)
# ID.sorted.a = (data %>% arrange(id))$ID
# Q.new = Q.new.a = Q.a[ID.sorted.a, ID.sorted.a]
