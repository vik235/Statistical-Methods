#*******************************************************************************************************#
#Author: Vivek Gupta
#Purpose: To run Wilcoxon signed rank test when the random variables has ties.  Otherwise use wilcox.test in R
#It runs hypothesis test agains median and for mean (since r.v is supposed to be symmetric to employ this test)
#Great and a very powerful test 

#Example setup of the analysis
#A new device has been developed which allows patients to evaluate their blood sugar levels.
#The most widely device currently on the market yields widely variable results. The new device is evaluated
#by 25 patients having nearly the same distribution of blood sugar levels yielding the following data:
  
#125 123 117 123 115 112 128 118 124
#111 116 109 125 120 113 123 112 118
#121 118 122 115 105 118 131

#Remember the conditions , random var Y must be continuous and symmetric (tails can be thicker than normal)
#Refer to Dr Longnecker notes for complete conditions. 
#Its a great, powerful test about median almost close to t-test and outsmarts t-tests when the tails are not normal

y =sort(c(125,123,117,123,115,112,128,118,124,
          111,116,109,125,120,113,123,112,118,
          121,118,122,115,105,118,131))
n=length(y)
u =seq(1/(2*n),1-1/(2*n),1/n)

#confirm symmetry or normality (normality not neccessary)
plot(qnorm(u),y)
abline(lm(y~qnorm(u)))

shapiro.test(y)

#H0 : mu_tilde >= 120 , H1: mu_tilde < 120;p < 1/2
med0=120
x=y-120

x1=x[x != 0]
nstar=length(x1)
c1=sort(abs(x1))
c2=order(sort(abs(x1)))
r= rep(0,nstar) #holds relative ranks based on abs values of x's
cr=rep(0,nstar) #holds correspondign ranks based on true values of x's
while(any(r==0))
{
  i=  as.vector(which(r==0))[1]
  r[which(c1==c1[i])]=mean(c2[which(c1==c1[i])]) #populate rank with ties
}
which(c1==abs(x1[2]))
for (index in 1:length(x1)) {
  cr[index]=r[which(c1==abs(x1[index]))[1]]
  
}
wplus= sum(cr[which(x1>0)])
wminus= sum(r[which(x1<0)])

qsignrank(.05,nstar,TRUE)

# We have wplus, test statistic =112.5 and critical value = 92, since wplus is greater than critical
#value we fail to reject the null hypotheis

#pvalue
psignrank(wplus,nstar,TRUE)
#0.1514496

#wilcox.test(y,rep(190,25),alternative = "less", paired = T, conf.int=T) Cannot compute with ties
#3) 95% bounds on the median. 