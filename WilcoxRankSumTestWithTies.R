#*******************************************************************************************************#
#Author: Vivek Gupta
#Purpose: To run Wilcoxon rank sum test when the random variables has ties.  Otherwise use wilcox.test in R
#It runs hypothesis test agains median and for mean (since r.v is supposed to be symmetric to employ this test)
#Great and a very powerful test 
#1. Order the N = n + m observations from smallest to largest
#2. Replace the data values with their ranks: 1 to N (In case of ties, assign the average rank to the
#                                                    tied values)
#3. Let W1 be the sum of the ranks for the Xis and let W2 be the sum of the ranks for the Yis
#4. W1 +W2 =
#  ??N
#i=1 i = N(N + 1)=2
#5. Given the size of the test, , use Table 11 in the Textbook or the R functions qwilcox and
#pwilcox to set the critical value and to compute the p-value for W1 or W2

#Note: This is solved for a particular case of two sided difference. Process of setting up hypothesis is manual 
#Example setup of the analysis

y1 <- (c(0,0,0,4,2,2,5,4,2,1,0,12,1,30,0,3,28,2,21,8,82,12,10,2,0))
#y1 <- (c(0,1,1,2,2,1,2,29,2,2,0,13,0,19,1,3,26,30,5,4,94,1,9,3,0))
y2 <- (c(0,0,0,2,3,0,0,4,0,5,4,22,0,64,4,4,43,3,16,19,95,6,22,0,0))


ymix=sort(c(sort(y1),sort(y2))) #mix the vectors together
ymixs=order(sort(ymix)) #grab the order of the sorted mixed vector , 1st iteration on ranks.

r=rep(0,length(ymix)) #hold the ranks here corresponding to the index of the mixed vector


while(any(r==0))
{
  i = as.vector(which(r==0))[1]
  r[which(ymix==ymix[i])]=mean(ymixs[which(ymix==ymix[i])])
}

ymix
ymixs

w1 = rep(0,length(y1))
w2= rep(0,length(y2))


for (i in 1:length(y1)) {
  w1[i]=r[which(ymix==y1[i])[1]]
}

for (i in 1:length(y2)) {
  w2[i]=r[which(ymix==y2[i])[1]]
}

W1= sum(w1)
W2=sum(w2)
W1
#638.5
W2
#636.5

pvalue= 2*min(1-pwilcox(sum(w1)-25*13-1,25,25),pwilcox(sum(w1)-25*13,25,25))
#1 for comparing y1 vs y2
pwilcox(sum(w1)-25*13,25,25)

qwilcox(1-.025,25,25)+25*13

25*(50-1)-(qwilcox(1-.025,25,25)+25*13)
