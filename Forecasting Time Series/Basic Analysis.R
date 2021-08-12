# Before starting, we have to install and activate some R libraries in order to use the necessary statistics and time series functions
# install.packages("fBasics")  # basic statistics 
# install.packages("forecast")  # time series functions
library(fBasics)
library(forecast)

# 1.	ANALYZING THE SIMULATED TIME SERIES - file Sessions2&3sim.csv

# Set the folder (path) that contains this R file as the working directory

folder_path <- "/UNIVERSIDAD/Master/2. 2nd Semester/9. Forecasting Time Series/Practice/Group/"
data<- read.table(file.path(folder_path, "Data 1.csv"), sep = ";", header = TRUE, dec = ",")

# Reading the files indicating the path
# data<-read.csv("C:/Users/usuario/Desktop/LPC/MASTERS/MBD_abril_2019/Sessions2&3sim.csv",header=TRUE,sep=";",dec=",")
# Once the time series are read into R, we give a name to each time series data 
# (represented by each column)
series1=data[,1] [1:300]
series2=data[,2] [1:300]
series3=data[,3] [1:300]
series4=data[,4] [1:300]
series5=data[,5] [1:2000]
series6=data[,6] [1:3000]
series7=data[,7] [1:3000]
#A.	Let's analyze stationarity for each time series
y<-series1    # from now, "y" is the data we are going to work with

par(mar=c(1,4,1,1)) # to adjust graphic size

par(mfrow=c(3,1)) # plot the series, its acf and pacf together
ts.plot(y)   
acf(y)
pacf(y)

mean(y) # compute basic statistics
sd(y)
skewness(y)
kurtosis(y,method=c("moment"))  

# formal unit root test (Augmented Dickey Fuller test). Testing for stationarity.
# Ho: the process is not stationary. We need, at least, a unit root
# H1: the process is stationary. We have to check different models (lags)
ndiffs(y, alpha=0.05, test=c("adf")) # number of regular differences?

#Checking for normality graphically
hist(y,prob=T,ylim=c(0,0.6),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")
# formal normality test
# Ho: the data is normally distributed
# H1: the data is not normally distributed
shapiro.test(y)
#B.	Testing for WHITE NOISE graphically

# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test (y, lag = 20, type="Ljung")  # Null: ro1=.=ro20=0

# C.	Testing for STRICT WHITE NOISE

par(mar=c(1,4,1,1)) # to adjust graphic size

par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(y^2)   
acf(y^2)
pacf(y^2)
# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test(y^2,lag=20, type="Ljung")    # Null: ro1=.=ro20=0

# D.	Testing for GAUSSIAN WHITE NOISE
shapiro.test(y)
# GWN ??? SWN

# Just in case we need to take one difference to the original data (as in this case)

z<-diff(y)  
ts.plot(z)

par(mfrow=c(3,1))
ts.plot(z)   
acf(z)
pacf(z)
ts.plot(z^2)   
acf(z^2)
pacf(z^2)
mean(z)
ndiffs(z, alpha=0.05, test=c("adf"))

Box.test (z, lag = 20, type="Ljung") 

Box.test (z^2, lag = 20, type="Ljung") 

#Checking for normality

shapiro.test(z)

hist(z,prob=T,ylim=c(0,0.6),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")


# 2.	ANALYZING THE REAL TIME SERIES - file Sessions2&3real.csv

data<-read.csv("Sessions2&3real.csv",header=TRUE,sep=";",dec=",")
#
# data<-read.csv("C:/Users/usuario/Desktop/LPC/MASTERS/MBD_abril_2019/Sessions2&3real.csv",header=TRUE,sep=";",dec=",")
# we have to follow exactly the same steps as before for the spot price series and for the return series
spot<-data[,1] # spot price series
y<-spot # follow all the steps above
# from now, "y" is the data we are going to work with
par(mfrow=c(3,1))
ts.plot(y)   
acf(y)
pacf(y)

#Checking for normality
shapiro.test(y)

hist(y,prob=T,ylim=c(0,0.02),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# RETURN TIME SERIES - STATIONARY TRANSFORMATION
returns<-diff(log(spot)) # returns series
z<-returns

# Here is the same code for the transformed variable "z"
ts.plot(z)   # time series plot

mean(z) # basic statistics
sd(z)
skewness(z)
kurtosis(y,method=c("moment"))

shapiro.test(z) # to check normality

hist(z,prob=T,ylim=c(0,30),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")



par(mfrow=c(3,1)) # is the transformed data white noise?
ts.plot(z)   
acf(z)
pacf(z)

Box.test(z,lag=20)  # Null: ro1=.=ro20=0 formal white noise test

par(mfrow=c(3,1)) # is the transformed data strict white noise?
ts.plot(z^2)   
acf(z^2)
pacf(z^2)

Box.test(z^2,lag=20)  # Null: ro1=.=ro20=0
