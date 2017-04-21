#*******************************************************************************************************#
#Author: Vivek Gupta, Date 4/1/17
#Purpose: To converge to a normal for any +ve r.v. Get a transform for the data and palce a CI on the transform function

#Great tool for feature engineering or runnign statistical analysis on data 
#careful on running this for CI about mean and sigma, retransforms dont work mathematically

y1 <-  #sort(#data vector, +ve in real, example below)
#y1 <- sort(rexp(100,80))
n <- length(y1)
theta <- -3 #starting seed , we will start with this seed and go to the +ve value of the seed.
iterations <- seq(theta, abs(theta)*2, 0.001) # this holds theta's, power transforms 
yt0 <-log(y1)
var_yt0 <- var(yt0)
l0 <- (0-1)*sum(log(y1)) - 0.5*n*(log(2*pi*var_yt0)+1)
t0 <- 0
logLikelihood <- as.vector(rep(0,length(iterations)))# this holds logliklihoods 1:1 with power transforms 
for (i in 1:length(logLikelihood)) {
  
  yt <- (y1^iterations[i] - 1)/iterations[i]
  var_yt <- var(yt)
  logLikelihood[i] <- (iterations[i]-1)*sum(log(y1)) - 0.5*n*(log(2*pi*var_yt)+1)
  if(abs(iterations[i]) < 1.0e-10) iterations[i] <- 0 # to cover for the iteration value when theta->0 
  if(abs(iterations[i]) < 1.0e-10) logLikelihood[i] <-l0 # to cover for the iteration value when theta->0 
  
}
plot(iterations,logLikelihood)
(theta_max <- iterations[which(logLikelihood==max(logLikelihood))])
(tU = max(logLikelihood)+.5*qchisq(.95,1)) #Upper bound on theta
(tL=  max(logLikelihood)-.5*qchisq(.95,1))#lower bound on theta
(tM=max(logLikelihood)) #theta max

(iL <- min(which((logLikelihood > tL) & (logLikelihood < tM)))) #index of lower bound on theta 
(iU <- max(which((logLikelihood > tL) & (logLikelihood < tM))))#index of upper bound on theta 

abline(v=iterations[iL])
abline(v=iterations[iU])
abline(v=theta_max, col=2)


