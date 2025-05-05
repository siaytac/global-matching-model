# Osth and Dennis (2015) Global Matching Model
tpe <- function(ll, lltest, Mss, contextDrift, k, ritem) {
  
  for (r in 1:length(ritem)) {
    
    for (i in 1:lltest) { 
      
      d <- osth(l=ll, ritem=ritem[r], Mss=Mss, Mtt=1, Vss=.1, Vtt=.01, Vti=.0005, p=.05, beta=.05)
      
      # Fox & Osth (2023) Equations for LLR Transformation
      dp  <- d[1]/sqrt(d[4])
      var <- d[3]/d[4]
      
      mun <- -((dp^2/2)*((var+3)/(4*var))+log(sqrt(var)))
      muo <- (dp^2)*((var+1)/(2*var))+mun
      
      sdn <- dp*((var+1)/(2*var))
      sdo <- sqrt(var)*sdn
      
      if (k == 1) {
        crit <- 0 # 0 represents an unbiased criterion
  
        hr[r,i] <- 1-pnorm(crit,muo,sdo)
        fr[r,i] <- 1-pnorm(crit,mun,sdn)
      
      } else { pc[r,i] <- afc(muo,sdo,mun,sdn,k) }
      
      #context drift account
      ll <- ll+k #add k alternatives
      Mss <- Mss*contextDrift^k #drift in context due to recognition testing
      #lmbd <- lmbd+0.001 #For the Kilic et al. (2017) data - people becoming more conservative throughout the test
      
    }
    
    #reset
    ll=50
    Mss=1
    
  }
  return(
    if (k == 1) { list(params=list(LL=ll, LLtest=lltest, Mss=Mss, contextDrift=contextDrift, Mtt=1, Vss=.1, Vtt=.01, Vti=.0005, p=.05, beta=.05, r=ritem)
                       , task=list(k=k)
                       , preds=list(hr=hr,fr=fr) )}
      else { list(params=list(LL=ll, LLtest=lltest, Mss=Mss, contextDrift=contextDrift, Mtt=1, Vss=.1, Vtt=.01, Vti=.0005, p=.05, beta=.05, r=ritem)
                  , task=list(k=k)
                  , preds=list(acc=pc)) }
  )
}
