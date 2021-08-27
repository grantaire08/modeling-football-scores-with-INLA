#' Model the attack and defense strength as random effects
#' Assume the strength of each team is a random variable changing over time
#' Regard the process as a random walk
#' The default setting of 'generic0' in R-INLA could be used to implement this specification
#' 
#' Random effects in R-INLA are defined with multivariate Gaussian distribution with mean zero 
#' and precision matrix tau·Sigma
#' The 'generic0' assumes Sigma is a constant matrix
#' 
#' Random walk could be implemented using 'generic0' by setting Sigma to be the structure matrix of random walk:
#'  1  -1
#' -1   2  -1
#'     ...
#'     -1   2  -1
#'          -1  1 
#' the structure matrix is constructed by adding the matrix (represents one step in the random walk) with following form:
#'      i   j
#'  i   1  -1
#'  j  -1   1
#'  (unspecified entries are zero)
#'  
#'  Back to our specification, we take u as a vector of random variables to represent each teams attack/defense
#'  ability at each games, to be precise
#'  u = (xi_{tm1,day1}, ..., xi_{tm1,dayn1},...,xi{tmTs,day1},...,xi_{tmTs,daynTs})
#'  (arranged by team first and then the dates)
#'  Since each team's strength changes over time as a random walk
#'  the corresponding precision matrix will be a block matrix whose block is the structure matrix of random walk
#'  We call this specification, blocked random walk (BRW)
#'  
#'  Further, we could generalize BRW by:
#'  1. Consider the effect from the time difference between two games next to each other
#'  2. Consider the effect from the arrival of new managers (honeymoon period/the first game)
#'  Saying considering those effects, we mean decreasing the precision (increase the variance) by divding 
#'  one-step blocks by a constant related to those effects

# Construct the precision matrix

get_Sig <- function(df, cd = 0, nmg = 0){
  # Sort df by team and date; save the new order by id
  id.levels = within(df %>% arrange(attack, date), id <- paste(date, attack))$id  # The order of block matrix
  id = (df %>% mutate(id = as.numeric(factor(paste(date, attack), levels=id.levels))))$id
  idx = 1:nrow(df)
  df$idx = idx
  
  # Construct the precision matrix by adding up one-step precision matrix 
  # is.last.game indicates whether the game is the last game in the dataset for each team
  # if so stop adding up
  
  Sig = inla.as.sparse(matrix(rep(0, 4*G*G), 2*G, 2*G))  # initialize the output
  for (i in idx) {
    if ( df$is.last.game[i] == FALSE ) {  # if the current game is the last game of the team, skip the update step
      j = idx[id == (id[i] + 1)]
      
      eff.cd = (as.numeric(df$date[j] - df$date[i]))*cd  # time difference between two games next to each other
      eff.nmg = (df$is.hm.a[i])*nmg  # if the game effected by the arrival of new managers
      
      eff = 1 + eff.cd + eff.nmg
      
      Sig[i,i] = Sig[i,i] + eff
      Sig[j,j] = Sig[j,j] + eff
      Sig[i,j] = Sig[i,j] - eff
      Sig[j,i] = Sig[j,i] - eff
    }
  }
  
  # So far, the precision matrix Sigma follows the order of idx
  # Arrange the matrix by id
  idx.sorted = (data.frame(idx, id) %>% arrange(id))$idx
  Sig.sorted = Sig[idx.sorted, idx.sorted]
  
  return(list(Sig=Sig.sorted, id=id.levels))
}