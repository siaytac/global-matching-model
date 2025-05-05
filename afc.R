#Find the accuracy for the AFC task
afc <- function(mu1,sd1,mu2,sd2,k){
  
  integrate(function(x,mu1,sd1,mu2,sd2) dnorm(x,mu1,sd1)*pnorm(x,mu2,sd2)^(k-1), -Inf, Inf, mu1=mu1, sd1=sd1, mu2=mu2, sd2=sd2)$value
  
}