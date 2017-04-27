#*******************************************************************************************************#
#Author: Vivek Gupta, Date 4/25/17 ; inspired by Dr Longneckers notes STATS 641 @TAMU
#Purpose: Find correlation, for lag 1 AR(1) data via the corr plot and also by the von Neumann test
# Valid for normal data and AR(1) only, harsh conditions on data. 



y  = rep(0,150)

y = c(99.49334,99.52677 ,99.97428, 99.92296,
 101.75614,101.01371,100.98810,99.75055, 97.46384, 97.53430, 98.04784,
  98.07517,98.82416 ,99.76238, 98.80839, 99.68745, 98.98936, 99.97021,
 100.24419,99.20544 ,98.68648, 99.86162, 98.52796, 98.65856, 98.44828,
  98.68382,98.81820, 98.95779, 98.86101, 99.94051, 99.07316, 99.79825,
 100.98074,99.49758, 98.49279, 98.75596, 99.79505, 99.55042, 97.95154,
  98.44396,99.85578 ,100.94686,99.18997, 100.28856,100.81671,100.96743,
 102.65183,101.44725,99.87142, 100.39037,98.71361, 98.29617, 97.32980,
  97.75256,97.45476 ,98.92480, 98.85961, 98.99318, 98.35815, 96.41931,
  96.83995,98.65699 ,98.74763, 98.04781, 99.76564, 101.78196,100.23923,
 101.04499,100.38626,100.53723,101.11212,103.02086,101.16156,101.12732,
 101.44200,101.07557,100.48501,102.01062,102.82457,103.05481,102.40505,
 102.28694,102.04882,102.17501,103.50176,103.05857,101.70988,103.21122,
 101.06555,102.83203,101.87783,101.29349,100.94850,99.73423, 99.35706,
  97.81771,96.22237, 95.61504, 97.07419, 98.32577, 99.51197, 100.24588,
  98.77193,100.68195,100.97093,101.29578,102.54733,102.12300,100.88476,
  99.46352,98.28398, 97.97027, 99.39698, 99.26945, 98.46643, 98.78961,
  99.91420,99.36403, 99.38256, 98.52452, 99.14218, 100.23898,102.28226,
 101.23884,100.27694,100.94119,99.96187, 99.82365, 99.52422, 100.39115,
 100.02542,100.55945,99.81125, 101.53655,101.31927,99.53333, 98.86780,
 100.04772,97.64038, 97.29670, 99.17730, 98.80871, 99.77137, 100.00520,
 100.06072,101.89751,101.49231,101.71528,102.79661,102.02205)

#Is the data normal and AR(1), if yes then apply von Neumann test and approximations for large n 

shapiro.test(y)
#yes, pvalue=.5 

#check for correlation, timeseries plot 

n  = length(y)
ytime1  = ts(y,start=1,frequency=1) 
plot.ts(ytime1,type="b",ylab="Y_t",xlab="Time (t)",main="Y_t  vs  Time (t)")
ymean  = mean(y)
abline(h=ymean,lty=2)

#corr 2, plot of Xt vs Xt-1
yt  = y[2:150]
ytlag1  = y[1:149]
plot(ytlag1,yt,main="Lag Plot",ylab="Y_t",xlab="Y_t-1")

#Correlation exists

#Calculation of von Neumann Statistics

dif1  = (yt-ytlag1)^2
num1  = sum(dif1)
y2  = (y-ymean)^2
den1  = sum(y2) 
Q  = (num1/(n-1))/(den1/n) 
prd1  = (yt-ymean)*(ytlag1-ymean)
prdsum1  = sum(prd1)
rho1  = prdsum1/den1
rho1 #estimate of rho, correlation coeff from the data 
Q #Q statisitic or von Neumann statisitics 


#since n > 60, we will use approx from critical values of Q 
Q_p_alpha= ((2*n)/(n-1)) - qnorm(.05)*2/sqrt(n)
Q_n_alpha= ((2*n)/(n-1)) + qnorm(.05)*2/sqrt(n)

#Hypothesis 
#Ho(for +ve corr) : rho <= 0 H1: rho >0 
#reject if Q is less that Q_p_alpha
#From the data we reject null 

#Hypothesis 
#Ho(for -ve corr) : rho >= 0 H1: rho <0 
#reject if Q is more that Q_n_alpha

#Impact on analysis if corr exists 
 #1 : Low CI coverage than promised 
 #2 : Large Type I errors, more than what promised. 
 #: Incorrect variance/SE of sampling distr of Xbar for example, underestimated if +ve corr and corr not accounted for. 
