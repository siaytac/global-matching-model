rm(list=ls()) # clean the global environment
graphics.off() # clean the plots

# set up the working directory
my.dir <- ""
setwd(my.dir)

# load the functions you will need to get the model's predictions
source("model.R") # the Osth & Dennis (2015) model function get the means and variances of the distributions
source("afc.R") # find accuracy on a kAFC task
source("tpe.R") # test position effect

# set up the parameters 
ll <- 50 # study list length
lltest <- 50 # test list length
Mss <- 1 # mean context match
contextDrift <- .9955 
k <- 2 # alternative choices in each trial (1 if yes-no; k if kafc)

ritem <- 0.6 # learning rate 

hr <- fr <- pc <- c() # to save the model predictions

simulation <- tpe(ll, lltest, Mss, Mtt=1, Vss=.1, Vtt=.01, Vti=.0005, p=.05, beta=.05, contextDrift, k, ritem)
