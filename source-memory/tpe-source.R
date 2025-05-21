tpeSource <- function(ll, lltest, ritem, rsource, nitem, k, Mss, contextDrift, Mtt, Maa, Mbb, Vss, Vtt, Vti, Vsu, Vaa, Vbb, Vab, Vba, Vac, Vbc) {

  for (i in 1:(lltest)) { 
    
    d <- osthSource(l=ll, ritem=ritem, rsource=rsource, nitem=nitem, Mss=Mss, Mtt=Mtt, Maa=Maa, Mbb=Mbb, Vss=Vss, Vtt=Vtt, Vti=Vti, Vsu=Vsu, Vaa=Vaa, Vbb=Vbb, Vab=Vab, Vba=Vba, Vac=Vac, Vbc=Vbc)
    
    #item recognition
    muo <- d[1]/sqrt(d[4])
    sso <- d[3]/d[4]
    
    crit <- lcu(d=muo, ss=sqrt(sso), lambda=0)[1]
    
    hr[i] <- 1-pnorm(crit,muo,sqrt(sso)) # predicted hit rate
    fr[i] <- 1-pnorm(crit,0,1) # predicted false alarm rate
    
    #source memory
    dsource <- (d[5]-d[6])/sqrt(d[7]) #(ma-mb)/sqrt(va) or sqrt(vb) because equal variance
    
    pc[i] <- afc(dsource, 1, 0, 1, k=2) # k = 2 because it is a source memory task
    
    #context drift account
    ll <- ll+k #add k alternatives
    Mss <- Mss*contextDrift^k
    
  }
  
  return(
    list(params=list(LL=ll, LLtest=lltest, ritem=ritem, rsource=rsource, nitem=nitem, Mss=Mss, contextDrift=contextDrift,
                     Mtt=1, Maa=0.6, Mbb=0.6, Vss=.1, Vtt=.05, Vti=.001, Vsu=.0001, Vaa=.01, Vbb=.01, Vab=.01, Vba=.01, Vac=.25, Vbc=.25)
         , task=list(k=k)
         , preds=list(hr=hr, fr=fr, source=pc) ) 
  )
}
