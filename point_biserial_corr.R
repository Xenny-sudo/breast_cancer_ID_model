#For the purposes of our experiment we want to make a logistic regression, however we don't want to use all features given to avoid overfitting. 
#In order to do so we figure out which variables correlate the best with point bi-serial correlation. 
#From these variables we can make models of all the possible permutations of said models.
pbc <- function(target, predictor){
  #Sample size
  n1 <- length(target)
  n2 <- length(predictor)
  
  N <- n1 + n2
  
  #Std
  s1 <- sd(target)
  s2 <- sd(predictor)
  
  
  thetaPool <- sqrt((((n1-1)*s1**2)+((n2-1)*s2**2))/(n1+n2-2))
  #Degrees of freedom within a group
  dfw <- N-2
  
  #Hedge's G
  g <- (mean(target) - mean(predictor))/thetaPool
  
  #Point biserial Corr
  r <- g/sqrt(g**2+dfw*((1/n1)+(1/n2)))
}