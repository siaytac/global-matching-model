
# Purpose: Fit the source memory model (Osth et al., 2018) to data to get parameter estimates

rm(list=ls())
graphics.off()

# set the working directory

# load the functions you will need to successfully run this model fitting
source("model-source.R") # the model code
source("lcu.R") # find criterion on LR scale
source("../afc.R") # convert dp to accuracy

# the model function
osth_fit <- function(Q, pp, l, nitem) {
  
  ritem <- Q[1]
  Mss   <- 1 # fixed
  Mtt   <- 1 # fixed
  Maa   <- Q[2]
  Mbb   <- Q[3]
  Vss   <- Q[4]
  Vtt   <- Q[5]
  Vti   <- Q[6]
  Vsu   <- Q[7]
  Vaa   <- Q[8]
  Vbb   <- Q[9]
  Vab   <- Q[10]
  Vba   <- Q[11]
  Vac   <- Q[12]
  Vbc   <- Q[13]
  
  rsource <- Q[14]
  
  dd <- osthSource(l=l, ritem=ritem, rsource=rsource, nitem=nitem, Mss=Mss, Mtt=Mtt, Maa=Maa, Mbb=Mbb, 
                   Vss=Vss, Vtt=Vtt, Vti=Vti, Vsu=Vsu, Vaa=Vaa, Vbb=Vbb, Vab=Vab, Vba=Vba, Vac=Vac, Vbc=Vbc)
  
  #item recognition
  muo <- dd[1]/sqrt(dd[4])
  sso <- dd[3]/dd[4]
  
  crit <- lcu(d=muo, ss=sqrt(sso), lambda=0)[1]
  
  pc<-c()
  
  pc[1] <- 1-pnorm(crit,muo,sqrt(sso))
  pc[2] <- 1-pnorm(crit,0,1)
  
  #source memory
  dsource <- (dd[5]-dd[6])/sqrt(dd[7]) 
  
  pc[3] <- afc(dsource, 1, 0, 1, 2)
  
  return(sum((pc-pp)^2))
  
}

# sample data: hr, far, source memory accuracy
aggData <- c(.65, .08, .75)

howmany <- 100

fits <- matrix(NA, howmany, 15)
for (i in 1:howmany) {
  
  tryCatch({
    print(i)
    START <- runif(14)
    fit <- nlminb(START, osth_fit, lower=0, upper=Inf, pp=aggData, l=48, nitem=500)  
    
    fits[i,1]    <- fit$objective
    fits[i,2:15] <- fit$par
    
  }, error=function(e){})
  
}
