library("readxl")
library("dplyr")
library(urca)

data_path <- "C:/Users/thomas/ownCloud/data science/Time Series and Forecasting Methods/Assignment-2020-2021/Data_Assignment.xlsx"

data <- read_excel(data_path, skip =1)
data <- data %>% select(-2)
print(data)

setwd("C:/Users/thomas/ownCloud/data science/Time Series and Forecasting Methods/Assignment-2020-2021")

### we will analyse y8 ###
y8 <- data$EMN
time_index = data$date
y8 <- ts(y8, frequency=12, start = c(1990,4))
pdf(file = 'time_series_histogram_EMN.pdf', width = 9, height = 6)
par(mfrow=c(1,2))
plot(time_index, y8,type="l", col='red', lwd=1,main="Time Series plot of EMN Monthly Returns", ylab="Quarterly earnings per share")
abline(h= mean(y8), col='blue')
legend("topleft", legend = c("EMN values", "mean of EMN"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8 )
hist(y8, nclass=15, main = "Histogram of EMN")
dev.off()

#Testing Stationarity
m=ar(y8)
res<-ur.df(y8,type="none",lag=m$order-1)
summary(res) #Series is stationary as pvalue<0.05

# notes
# 1) Mean of y =0.0038 so almost no drift offset
# 2) heteroscedacity between the signal samples.
shapiro.test(y8)
# for a level of .05 significance we failed to reject
# the null hypothesis, so the data are assumed to be normal

# QQ plot
qqnorm(y8,main="Normal QQplot of EMN") # normal Q-Q plot
qqline(y8)

#Identification step

pdf(file = 'acf_pacf.pdf', width = 9, height = 6)
par(mfrow=c(1,2)) # set up the graphics
acf(x=y8, lag.max=60, main="ACF of ENM") # autocorrelation function plot
pacf(x=y8, lag.max=60, main="PACF of ENM") # partial autocorrelation function
dev.off()
# We see that the significant terms are:
# MA - Terms 6:12:19
# AR - Terms 6:26:32

Box.test(y8, 60,type="Box-Pierce")

# we see that of a reasonal time window of 60 lags
# we almost failed to reject the null hypothesis
Box.test(y8, 60,type="Ljung-Box")
# we reject the null hypothesis, meaning that there
# might be autocorrelations

# Esimation step

ma1fit = arima(y8, order = c(0,0,1))
ma1fit
# not significant coeff

ma6fit_r = arima(y8, order = c(0,0,6), fixed =c(0,0,0,0,0,NA,NA))
ma6fit_r
#coeff is significant

ma612fit_r = arima(y8, order = c(0,0,12), fixed =c(0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
ma612fit_r

#AR-6 fit
ar6_restricted = arima(y8,order=c(6,0,0),fixed = c(0,0,0,0,0,NA,NA))
ar6_restricted
# coeff is significant
# 0.2854/0.0690


ar6residuals=ar6_restricted$residuals
#ar6residuals

par(mfrow=c(3,2)) # set up the graphics
acf(ts(ar6residuals,freq=1), 60, main="ACF of residuals")
pacf(ts(ar6residuals,freq=1), 60, main="PACF of residuals")
acf(ts(ar6residuals^2,freq=1), 60, main="ACF of squared residuals")
pacf(ts(ar6residuals^2,freq=1), 60, main="PACF of squared residuals")
qqnorm(ar6residuals,main="Normal QQplot of residuals")
qqline(ar6residuals)

# AR-6-26 model
ar6_26fit=arima(y8,order=c(26,0,0),fixed=c(0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,NA,NA), method="ML", optim.method="Nelder-Mead")
ar6_26fit
# Coeff are significant

ar6_26residuals=ar6_26fit$residuals
pdf(file = 'diagnostic_checking.pdf', width = 9, height = 6)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(ar6_26residuals,freq=1), 60, main="ACF of residuals")
pacf(ts(ar6_26residuals,freq=1), 60, main="PACF of residuals")
acf(ts(ar6_26residuals^2,freq=1), 60, main="ACF of squared residuals")
pacf(ts(ar6_26residuals^2,freq=1), 60, main="PACF of squared residuals")
qqnorm(ar6_26residuals,main="Normal QQplot of residuals")
qqline(ar6_26residuals)
dev.off()

# Predictions
forecast=predict(ar6_26fit,10)
forecast

pdf(file = 'forecast.pdf', width = 9, height = 6)
# plot of forecasts with 1 s.e
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y8,LL); maxx = max(y8,UL)
ts.plot(y8, forecast$pred, ylim=c(-0.1,0.1))
lines(forecast$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()

#erotima 2
y8 <- data$EMN
y8
x1<-data$`RUS-Rf`
x1

x2<-data$`RUS(-1)-Rf(-1)`
x2
x3<-data$`MXUS-Rf`
x4<-data$`MEM-Rf`
x5<-data$SMB
x6<-data$HML
x7<-data$MOM
x8<-data$`SBGC-Rf`
x9<-data$`SBWG-Rf`
x10<-data$`LHY-Rf`
x11<-data$DEFSPR
x12<-data$`FRBI-Rf`
x13<-data$`GSCI--Rf`
x14<-data$VIX
x15<-data$Rf
#==============================================
# Some plots
#==============================================
pairs(cbind(y8,x1,x2)) 

#================================================
# Estimate multiple regression using command lm()
#================================================
model <- lm(y8~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15)
summary(model)
AIC(model)
# We show that the variable x1, x5,x7 and x15 is of most significance
# for our model

## erotima 3
mydata <- cbind(x1,x5,x6,x7,x8,x15)
model2 <- lm(y8~x1+x5+x6+x7+x8+x15)
summary(model2)
AIC(model2)

#Check Autocorrelation of the residuals and heteroscedasticity
lmres<-residuals(model2)
pdf(file = 'acf_pacf_residuals.pdf', width = 9, height = 6)
par(mfrow=c(3,2))
plot(lmres, type="l", main="Plot of residuals")
hist(lmres, main="histogram of returns")
acf(lmres,50, main="ACF of residuals")#Small issue at lag 6
pacf(lmres,50, main="PACF of residuals")#Small issue at lag 6,13
acf(lmres^2,50, main="ACF of squared residuals") #Heteroscedasticity problem at 3,4
pacf(lmres^2,50, main="PACF of squared residuals")#Heteroscedasticity problem at lag 3,4
dev.off()
Box.test(lmres,lag=12,type="Ljung") 
# We failed to reject H0, indicating no autocorelation effects
Box.test(lmres^2,lag=12,type="Ljung")#Ho is rejected, heterosceasticity issue
# applying Ljung-Box test on the squared residuals shows 
# that the null hypothesis of no autocorrelation is rejected, 
# indicating heteroscedastic effects, 
# i.e. the volatility of the return series is time-varying. 
#To take into account for these characteristics, we will estimate ARCH/GARCH models.


ext <- data.matrix(mydata[,])

require(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(13, 0)), 
mean.model = list(armaOrder = c(0, 0), external.regressors = ext), 
distribution.model = "norm", start.pars = NULL, fixed.pars = list())

garch <- ugarchfit(data = y8, spec = spec, solver = "hybrid", fit.control = list(scale = 1)) 
garch 
plot(garch)

#RETRIVE STANDARDIZED RESIDUALS 
residuals1<-ts(residuals(garch,standardize=TRUE),freq=1)

#AUTOCORRELATION:
pdf(file = 'acf_pacf_arch.pdf', width = 9, height = 6)
par(mfrow=c(2,2))
acf(residuals1, 50)#Autocorrelation ok
pacf(residuals1,50)#Autocorrelation ok
#HETEROSCEDASTICITY
acf(residuals1^2, 50)#Heteroscedasticity fixed
pacf(residuals1^2,50)
dev.off()
Box.test(residuals1,lag=50,type="Ljung")
Box.test(residuals1^2,lag=50,type="Ljung")

#NORMALITY
plot(garch)#type 9
shapiro.test((residuals1))#Ho not rejected residuals are normal!



### we will analyse y7 ###
## erotima 1
y7 <- data$DS
time_index = data$date
y7 <- ts(y7, frequency=12, start = c(1990,4))

par(mfrow=c(1,2))
plot(time_index, y7,type="l", col='red', lwd=1,main="Time Series plot of DS Monthly Returns", ylab="Quarterly earnings per share")
abline(h= mean(y7), col='blue')
legend("topleft", legend = c("DS values", "mean of DS"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8 )
hist(y7, nclass=15, main = "Histogram of DS")

qqnorm(y7,main="Normal QQplot of DS") # normal Q-Q plot
qqline(y7)

shapiro.test(y7)

lj=log(y7)
shapiro.test(lj)

dlj=diff(lj)
shapiro.test(dlj)
# The data is not nornal, we can not model them