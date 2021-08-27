#' Model the attack and defense strength as random effects
#' Assume the strength of each team is a random variable changing over time
#' Regard the process as a random walk
#' The default setting of 'generic0' in R-INLA could be used to implement this specification

m.pois = function(df) {
  ls = get_Sig(data, cd=0, nmg=0)  # The attack/defense strength share the same precision matrix
  Sig = ls$Sig
  id.levels = ls$id
  
  df.brw = df %>% mutate(
    ID.a = factor(paste(date, attack), levels=id.levels),
    ID.d = factor(paste(date, defense), levels=id.levels)
  )
  
  
  formula <- y ~ 1 +
    f(ID.a, model='generic0', Cmatrix = Sig, values = id.levels, diagonal = 1e-5, rankdef = Ts,
      hyper = list(
        theta = list(
          prior = "loggamma",
          param = c(1,.00005),
          initial = log(100000),
          fixed = T)) # To save time, we fixed the hyperparameters
    ) +
    f(ID.d, model='generic0', Cmatrix = Sig, values = id.levels, diagonal = 1e-5, rankdef = Ts,
      hyper = list(
        theta = list(
          prior = "loggamma",
          param = c(1,.00005),
          initial = log(100000),
          fixed = T))
    )
  
  m.brw <- inla(formula, family='poisson', data = df.brw,
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))
  
  return(m.brw)
}
