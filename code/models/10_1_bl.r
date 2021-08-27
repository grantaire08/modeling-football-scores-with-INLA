#' Model attack and defense strength as fixed effects
#' Simple Poisson regression
m.pois = function(df) {
  formula <- y ~ 1 + attack + defense
  
  m.bl <- inla(formula, family = "poisson", data=df,
               control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))
  
  return(m.bl)
}