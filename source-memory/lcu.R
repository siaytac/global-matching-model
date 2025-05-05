# Log Likelihood ratio transformation
lcu <- function(d,ss,lambda){
  
  A <- (ss**2 - 1)/(2*ss**2)
  B <- d/ss**2
  C <- -(d**2/(2*ss**2) + log(ss) + lambda)
  
  -B/(2*A) + c(1,-1)*sqrt(B**2 - 4*A*C)/(2*A)
}