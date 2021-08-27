#' Model attack and defense strength as iid random effects
#' #' The default setting of 'iid' in R-INLA could be used to implement this specification
#' 
m.pois = function(df) {
  formula <- y ~ 1 + 
    f(attack, model="iid", hyper=list(theta=list(initial=1, fixed=T))) + 
    f(defense, model="iid", hyper=list(theta=list(initial=1, fixed=T)))
  
  m.iid <- inla(formula, family = "poisson", data=df,
               control.predictor = list(compute = TRUE), control.compute = list(config = TRUE))
  
  return(m.iid)
}