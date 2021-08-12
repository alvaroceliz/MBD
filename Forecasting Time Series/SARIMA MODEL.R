library(fBasics)
library(forecast) 

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# *** SESSION 8. Real Example ***
datos<-read.csv("Data 2.csv",header=TRUE,sep=";",dec=",")
y<-datos[,2] [0:83]
t<-datos[,2] [84:107]
f<-datos[,2]

ts.plot(w)
z <- log(y)
w <- diff(z)

par(mar=c(5,5,5,5))

nlags=72     # play with this parameter..
par(mfrow=c(2,1))
acf(w,nlags)
pacf(w,nlags)  

s=4       # seasonal parameter FOR THIS DATA SET

ndiffs(w, alpha=0.05, test=c("adf")) # regular differences?

nsdiffs(w,m=s,test=c("ocsb"))  # seasonal differences?


# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(z,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?


fit<-arima(z,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    
SARIMA model of order 1,1,0 & 0,1,1, while s being equal to 4 as well as SARIMA model of order 0,1,1 & 3,1,0, 
SARIMA model of order 0,1,3 & 0,1,0, SARIMA  of order 1,1,0 & 2,1,0

Box.test(fit$residuals,lag=36) # white noise residuals?

Box.test(fit$residuals,lag=20)    # play with the number of lags

shapiro.test(fit$residuals)  # normality test

fit_2<-arima(z,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit_2
ts.plot(fit_2$residuals)
par(mfrow=c(2,1))
acf(fit_2$residuals,nlags)
pacf(fit_2$residuals,nlags)

error <- t - exp(y.pred$pred)
real <- t
Box.test(fit_2$residuals,lag=36)
mean(abs(error/real)) *100

# normality test graphically
hist(fit$residuals,prob=T,ylim=c(0,0.002),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# point predictions and standard errors

y.pred<-predict(fit_2,n.ahead=24)
y.pred$pred   # point predictions
y.pred$se    # standard errors


# plotting real data with point predictions

new <- c(y,exp(y.pred$pred)) # real data + predicted values

plot.ts(new,main="Predictions",
        ylab="EPS",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2)# for the second series
lines(f,col=4,lwd=2)# for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)
