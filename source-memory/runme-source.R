rm(list=ls())
graphics.off()

# set the working directory

# load the functions you need to get the model's predictions
source("model-source.R") # Osth et al. (2018) Source Memory Model 
source("lcu.R") # the function to find criterion on LR scale 
source("tpe-source.R") # test position effect for source memory
source("../afc.R") # convert dp to accuracy

# set the parameters                             
ll <- 48 # study list length
lltest <- ll*2 # test list length
nitem <- 500 # number of prior occurrences of items (would be different for low- vs. high-freq words)
Mss <- 1 # mean context match
contextDrift <- .9925
k <- 1 # alternative choices in each trial (1 if yes-no; k if kafc)

ritem <- 1.5 # learning rate for items
rsource <- 0.5 # learning rate for context/source

hr <- fr <- pc <- c() # save the model's predictions

# run the simulation                           
simulations <- tpeSource(ll=ll, lltest=lltest, ritem=ritem, rsource=rsource, nitem=nitem, Mss=Mss, contextDrift=contextDrift, k=k)
