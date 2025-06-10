# Osth and Dennis (2015) Global Matching Model - Context Drift
tpe <- function(ll, lltest, Mss, Mtt, Vss, Vtt, Vti, p, beta, contextDrift, k, ritem) {
  
  for (i in 1:lltest) { 
    
    d <- osth(l=ll, ritem=ritem, Mss=Mss, Mtt=Mtt, Vss=Vss, Vtt=Vtt, Vti=Vti, p=p, beta=beta)
    
    # LLR Transformation
    dp  <- as.numeric(d[1]/sqrt(d[4]))
    var <- as.numeric(d[3]/d[4])
    
    mun <- -((dp^2/2)*((var+3)/(4*var))+log(sqrt(var)))
    muo <- (dp^2)*((var+1)/(2*var))+mun
    
    sdn <- dp*((var+1)/(2*var))
    sdo <- sqrt(var)*sdn
    
    if (k == 1) {
      crit <- 0 # 0 represents an unbiased criterion
      
      hr[i] <- 1-pnorm(crit,muo,sdo)
      fr[i] <- 1-pnorm(crit,mun,sdn)
      
    } else { pc[i] <- afc(muo,sdo,mun,sdn,k) }
    
    # context drift account
    ll <- ll+k # add k alternatives to the list
    Mss <- Mss*contextDrift^k # drift in the context due to retrieval attempt(s)    
  }
  
  return(
    if (k == 1) { list(params=list(LL=ll, LLtest=lltest, Mss=Mss, contextDrift=contextDrift, Mtt=Mtt, Vss=Vss, Vtt=Vtt, Vti=Vti, p=p, beta=beta, r=ritem)
                       , task=list(k=k)
                       , preds=list(hr=hr,fr=fr) )}
    else { list(params=list(LL=ll, LLtest=lltest, Mss=Mss, contextDrift=contextDrift, Mtt=Mtt, Vss=Vss, Vtt=Vtt, Vti=Vti, p=p, beta=beta, r=ritem)
                , task=list(k=k)
                , preds=list(acc=pc)) }
  )
}
