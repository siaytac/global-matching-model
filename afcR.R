
#Find the accuracy for the AFC task when there is one new and (k-1) old items
afcR <- function(mu1,sd1,mu2,sd2,k){

  integrate(function(x,mu1,sd1,mu2,sd2) dnorm(x,mu2,sd2)*(1-pnorm(x,mu1,sd1))^(k-1), -Inf, Inf, mu1=mu1, sd1=sd1, mu2=mu2, sd2=sd2)$value

}
