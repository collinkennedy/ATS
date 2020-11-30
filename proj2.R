#Consider the US monthly 30-year conventional mortgage rates from April 1971 to November 
#2011. Data are available from Federal Reserve Economic Data and are in the file  
#"mortgage.txt" (available in the folder "Datasets" on the course canvas page). 
#Data file consists of 5 columns ( year, month, day, morg, ffr), where "morg" is the monthly
#mortgage rate and  "ffr" is the monthly federal funds rate. Analyze this dataset using the 
#methods that you have learned in this course. Your final report should address the following.

rm(mortgage_ts)
library(ggplot2)
library(tidyverse)
library(astsa)
install.packages("lubridate")
mortgage = read.table("/Users/collinkennedy/Downloads/Downloads/mortgage.txt", header = TRUE, sep=" ")
View(mortgage)






##month = factor(cycle(temperature$temperature),12) #create the seasonal component


mortgage_ts <- ts(mortgage$morg, start=c(1971, 4), end=c(2011, 11), frequency=12)
plot(mortgage_ts)
ffr_ts = ts(mortgage$ffr, start=c(1971, 4), end=c(2011, 11), frequency=12)


#(a) Explain the data, why it is a time series data.
#This data represents the changes in the 30 year monthly mortgage rate from April 1971
# to November 2011. On a more statistical level, this data is time series data because
#it is a collection of observations at equally spaced time points.



#b)Use exploratory analysis to describe the data


#include a box plot 


#time series plots
par(mfrow= c(2,1)) 
morgTSPlot = plot.ts(mortgage_ts, main = " US Monthly Mortgage Rate: 1971 -2011",ylab= "Mortgage Rate")
ffrTSPlot = plot.ts(ffr_ts, ylab = "Federal Funds Rate", main = "Federal Funds Rate: 1971 - 2011")

                   


#histograms of mortgage and federal funds rate
morg_hist = ggplot(mortgage, aes(x = morg)) +
  geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "darkgreen")+
  scale_x_discrete("mortgage")+
  ggtitle("Histogram: Mortgage Rates") #fix x axis

ffr_hist = ggplot(mortgage, aes(x = ffr)) +
  geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "darkblue")+
  scale_x_discrete("federal funds rate")+
  ggtitle("Histogram: Federal Funds rate") #fix x axis

library("gridExtra")
grid.arrange(morg_hist,ffr_hist)


#c) (c) Determine if the monthly mortgage rate series is stationary. 
#If the series was not stationary apply appropriate transformation(s) to make the transformed series 
#stationary.

#Given that there is a clear downward trend in the mortgage rate, this is a clear violation
#of the constant mean assumption for stationarity.

#transform the data: applying differencing
transformed_morg_ts = diff(mortgage_ts,1)
plot(transformed_morg_ts)

#the trend appears to have been eliminated, and the stationary assumption appears to be met

#d) Compute sample autocorrelation and partial autocorrelation functions for the monthly 
#mortgage rate and explain their meanings. Is there any model suggested by the 
#autocorrelation or partial autocorrelation functions?
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
par(mfrow= c(2,1)) 
acf(transformed_morg_ts)
pacf(transformed_morg_ts)

acf(mortgage_ts)

#Is there any model suggested by the autocorrelation or partial autocorrelation functions?

#Since both the acf and pacf have 1 statistically significant lags, I could see an 
#Ar(1) or an MA(1) model fitting well.
#or do we say ARIMA(1,1,0) or ARIMA(0,1,1)



#(e) Build an ARIMA model for the monthly mortgage rate. Perform model checking and 
#write down the fitted model. If there are competing models that fit the data, determine 
#your model selection criterion.


#ARIMA(1,1,0)
sarima(mortgage_ts,p = 1,d = 1, q = 0)
?sarima

#ARIMA(0,1,1)
sarima(mortgage_ts,p=0,d=1,q=1)

#explain why ARIMA(0,1,1) is better


#(f) Mortgage rate is known to depend on the Federal Funds rate. Build a time series 
#model for the mortgage rate using the lag-1 federal funds rate as an explanatory variable. 
#Perform model checking and write down the fitted model. (If LaTeX: \{F_t\}
 

laggedFFR = lag(mortgage$ffr)                                                                                                                                                                                                                                       
morgLM = lm(mortgage$morg ~ lag(mortgage$ffr))
summary(morgLM)

#check the acf of the residuals
acf2(resid(morgLM))

sarima(resid(morgLM), 0, 0, 1) #check if MA(1) model for errors

?sarima

