rm(list=ls()) # clean the global environment
graphics.off() # clean the plots

# set up the working directory

# load the functions you will need to get the model's predictions
source("model.R") # the Osth & Dennis (2015) model function get the means and variances of the distributions
source("afc.R") # find accuracy on a kAFC task
source("afcR.R") # find accuracy on a kAFC* task (where there's one new item & and multiple old items)
source("tpe.R") # test position effect

# set up the parameters 
ll <- 50 # study list length
lltest <- 50 # test list length
Mss <- 1 #mean context match
contextDrift <- .9955 #.9985
k <- 2 #alternative choices in each trial (1 if yes-no; k if kafc)

ritem <- seq(0.3, 1.2, by=0.3) # learning rate(s) 

hr <- fr <- pc <- matrix(NA, nrow=length(ritem), lltest) # create empty matrices to save the predictions

simulation <- tpe(ll, lltest, Mss, contextDrift, k, ritem)


# get the predictions
preds <- simulation$preds

per <- 10 # how many trials in each test block?
nblock <- lltest/per

# Murdock-Anderson-1975 test positions: c(2,5,8,11,14)

hh <- ff <- acc <- matrix(NA, length(ritem), nblock) # create empty matrices to save the mean for each test block
for (j in 1:nblock) {
  if (k == 1) {
    hh[,j] <- rowMeans(preds$hr[,(1+per*(j-1)):(per*j)])
    ff[,j] <- rowMeans(preds$fr[,(1+per*(j-1)):(per*j)])
  } else { acc[,j] <- rowMeans(preds$acc[,(1+per*(j-1)):(per*j)]) }
}


# plot the predictions
cols <- c("darkgreen", "orange2", "red3")

if (k == 1) { # yes-no recognition task
  plot(c(1:nblock), hh[1,], ylim=c(0,1), xlab="Test Block", ylab="p(old)",
       type="b", pch=19, col="gray1", main="Yes-No Recognition Task")
  points(c(1:nblock), ff[1,], type="b", pch=17, col="gray1")
  
  for (i in 1:3) {
    points(c(1:nblock), hh[i+1,], type="b", pch=19, col=cols[i])
    points(c(1:nblock), ff[i+1,], type="b", pch=17, col=cols[i])
  }
  
  } else { # kAFC task
  plot(c(1:nblock), acc[1,], ylim=c(0,1), xlab="Test Block", ylab="Accuracy",
         type="b", pch=19, col="gray1", main=paste0(k, "-Alternative Forced-Choice Task")) 
  for (i in 1:3) { points(c(1:nblock), acc[i+1,], type="b", pch=19, col=cols[i]) }
  
  legend(legend=c(paste("r =", ritem)), pch=19, col=c("gray1", cols), "bottomright")
  }
