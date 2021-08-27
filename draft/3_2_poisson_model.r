source("code/2_2_structure_ma.r")

df$ID.a = df$ID.d = ID
Ts = length(unique(df$attack))
# Ts = 20

# The formula of linear predictor
formula <- y ~ 1 +
  f(ID.a, model='generic0', Cmatrix = Q.a, diagonal = 1e-5, rankdef = Ts,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005),
        initial = log(100000),
        fixed = T)
      )
  ) +
  f(ID.d, model='generic0', Cmatrix = Q.d, diagonal = 1e-5, rankdef = Ts,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005),
        initial = log(100000),
        fixed = T)
    )
  )

# Pass the formula and data to inla to train the model
result <- inla(formula, data = df, family='poisson',
               control.predictor = list(compute = TRUE),
               control.compute = list(config = TRUE))

# Print the summary of the result
summary(result)

# rw1
# only with attack strength: -2138.36
# only with defense strength: -2204.05
# with both: -2024.29, -2029.00 (diagonal=1e-1), -2231.36 (rankdef=Ts)

# divided by cd: 764.57 (2 seasons), 757.45(diagonal=1e-1), 555.25 (rankdef=Ts), 584.27(rankdef=20),
# 1157.38 (3 seasons)

# add effects from new managers:  592.76(rankdef=Ts)


# rw2
# with both: -1970.77

# divided by cd:  765.20 (2 seasons)


# The formula of linear predictor
id = within(df %>% arrange(attack, date), id <- paste(date, attack))$id
df = df %>% mutate(
  ID.a = factor(paste(date, attack), levels=id),
  ID.d = factor(paste(date, defense), levels=id)
)

formula <- y ~ 1 +
  f(ID.a, model='generic0', Cmatrix = Q.new, values = id, diagonal = 1e-5, rankdef = Ts,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005),
        initial = log(100000),
        fixed = T))
  ) + 
  f(ID.d, model='generic0', Cmatrix = Q.new, values = id, diagonal = 1e-5, rankdef = Ts,
    hyper = list(
      theta = list(
        prior = "loggamma",
        param = c(1,.00005),
        initial = log(100000),
        fixed = T))
  )

# Pass the formula and data to inla to train the model
result <- inla(formula, data = df, family='poisson',
               control.predictor = list(compute = TRUE), 
               control.compute = list(config = TRUE))

# Print the summary of the result
print(summary(result))

