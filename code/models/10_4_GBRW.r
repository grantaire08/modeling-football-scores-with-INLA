#' Generalization of BRW by considering both time difference and the arrival of new managers

m.pois = function(df) {
  ls = get_Sig(data, cd=1, nmg=1)  # The attack/defense strength share the same precision matrix
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
  
  m.gbrw <- inla(formula, family='poisson', data = df.brw,
                control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))
  
  return(m.gbrw)
}
