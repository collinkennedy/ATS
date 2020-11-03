###3

#
library(astsa)

JohnsonJohnson

jj

#create a better dataframe
#jjQtrEarnings = data.frame(x = rep(1960:1980,each = 4),y = 1:84)
#colnames(jjQtrEarnings) = c("year","t")
#jjQtrEarnings

rm(list = ls())

t = time(jj)-1970 #center it
jjReg = lm(log(jj) ~ 0 + t + as.factor(cycle(jj)))
sum(jjReg$coefficients)
jjReg$fitted.values



jjReg$coefficients[4]-jjReg$coefficients[5]

plot.ts(log(jj))
lines(jjReg$fitted.values,col = "blue")
summary(jjReg)

library(car)
linearHypothesis(jjReg,"jjReg$coefficients[4]=jjReg$coefficients[5]")
jjReg
View(as.factor(time(jj)))




Qtr = factor(cycle(jj,4))
trend = time(jj) -1970
reg = lm(log(jj) ~ 0 + trend + Qtr,na.action = NULL)
plot.ts(log(jj))
lines(fitted(reg),col = 3)



version

jjFit = lm()

str(jj)
?JohnsonJohnson

str(JohnsonJohnson)

options(scipen=999)

Qtr = factor(rep(1:4,21))
trend = time(jj)
trend
logreg = lm(log(jj)~0+trend+Qtr, na.action=NULL,data = jj)
plot(logreg$fitted.values)

#non log transformed
reg = lm(jj ~ trend + Qtr, na.action = NULL,data=jj)
plot(reg$fitted.values)

library(astsa)
par(mfrow=2:1)
tsplot(jj,ylab="QEPS", type = "o", col = 4, main = "JJ Quarterly Earnings")

tsplot(log10(jj),ylab="QEPS", type = "o", col = 4, main = "logJJ Quarterly Earnings")


#6
plot.ts(varve)

var(varve[1:(length(varve))/2])

var(varve[(length(varve)/2):length(varve)])

logTransformedYt = log(varve)
ts = cbind(varve,logTransformedYt)
plot.ts(ts)

var(logTransformedYt[1:(length(logTransformedYt))/2])

var(logTransformedYt[(length(varve)/2):length(logTransformedYt)])



hist(varve)
hist(logTransformedYt)
plot.ts(logTransformedYt)

acf(logTransformedYt)


#d
Ut = logTransformedYt - lag(logTransformedYt,1)
plot.ts(Ut)
acf(Ut)

