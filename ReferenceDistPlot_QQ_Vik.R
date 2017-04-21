#*******************************************************************************************************#
#Author: Vivek Gupta, Date 3/10/17
#Purpose: Some stuff on Reference dist plot , when and how to apply them 
# List 3 cases 


# QQ plot / reference distribution plot when Fsub0 is completely specified , 
#Note that intercept must be close to 0 and slope must be close to 1 
#Yi's are sorted sample quantiles.
#Xi's are quantiles of the exact distribtion evaluated at u=(1/2n, 1-1/2n,1/n) thats is ui's are (i-0.5)/2n for i=1,2..n

y <- sort(rgamma(100,2.3,scale = 4.5))
u <- seq(1/200,1-(1/200),1/100)
x<-qgamma(u,2.3,scale = 4.5)
plot(x,y)
z<-lm(y~x)
abline(z)

# QQ plot / reference distribution plot when Fsub0 is not completely specified , that is we know the distribution 
#but do not know the parameters of the dist. and we do not whethere whether Fsub0 is a member of loc-scale family
#We use MLE method but it may lead to inaccuracies. 
#Note that intercept must be close to 0 and slope must be close to 1 
#Yi's are sorted sample quantiles.
#Xi's are quantiles of the exact distribtion evaluated at u=(1/2n, 1-1/2n,1/n) thats is ui's are (i-0.5)/2n for i=1,2..n

y <- sort(rgamma(100,2.3,scale = 4.5)) # assuming that alpha and beta are unknown . 
u <- seq(1/200,1-(1/200),1/100)

library("MASS")
estimates <-fitdistr(y,"Gamma",lower=0)$estimate
x<-qgamma(u,2.8531823,scale = 1/0.2597574)
plot(x,y)
z<-lm(y~x)
abline(z)
z
# QQ plot / reference distribution plot when Fsub0 is not completely specified , that is we know the distribution 
#but do not know the parameters of the dist. and we know Fsub0 is a member of loc-scale family
#We use graphical method against standard member of the family
#Note that intercept in this case specified the loc param and slope specifies the scale param for the dist.
#Yi's are sorted sample quantiles.
#Xi's are quantiles of the exact distribtion evaluated at u=(1/2n, 1-1/2n,1/n) thats is ui's are (i-0.5)/2n for i=1,2..n

y <- sort(rgamma(100,2.3,scale = 4.5)) # assuming that alpha and beta are unknown . 
u <- seq(1/200,1-(1/200),1/100)
x<-qgamma(u,2.3,scale = 1) #since gamma is a scale family. 
plot(x,y)
z<-lm(y~x)
abline(z)
z #<-gives slope as the scale of the F which comes to be 4.3

#Example with normal distribution 
y <- sort(rnorm(100,2.3,4.5)) # assuming that mean and variance are known 
u <- seq(1/200,1-(1/200),1/100)
x<-qnorm(u,0,1) 
plot(x,y)
z<-lm(y~x)
abline(z)
z #<-gives slope as the scale of the F which comes to be 4.3


#Example with expo distribution 
y <- sort(rexp(100,1/4.5)) # assuming that mean and variance are known 
u <- seq(1/200,1-(1/200),1/100)
x<-qexp(u,1) #or 
x<-log(1-u)
plot(x,y)
z<-lm(y~x)
abline(z)
z #<-gives slope as the scale of the F which comes to be 4.3


y<-sort(log(rweibull(100,2.3,scale = 4.5)))
u <- seq(1/200,1-(1/200),1/100)
x<-log(-log(1-u))
plot(x,y)
z<-lm(y~x)
abline(z)
z #<-gives slope as the scale of the F which comes to be 4.3
(alpha <- exp(1.4878))#shape of weibull 
(gamma <- 1/.4114) #scale of weibull 
