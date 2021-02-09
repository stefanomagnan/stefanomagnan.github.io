#### Forecasting Time Series - Homerwork 2 - Group "B" ####

# Team: Stefano Magnan, Nicolas Boer, Eleonora Jimenez, Rebecca Rosser, Alberto De Roni, Laura Frazer

# Upload the dataset
# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
datos<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")

# Install  packages
library(fBasics)
library(forecast) 

# Defining the dataset
y <- datos[,2] # Put the values in a y variable (quarterly data)
s = 4 # We have quarterly data
nlags=100

ts.plot(y) # We identify that the data has an upward trend, is not stationary (at least in the mean, the variance let's check after taking the difference)
# probably we are looking at seasonal data

# We try to identify if data is stationary
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?

# We identify that the data is not stationary, because we need one difference in both regular and seasonal part of the data

z <- diff(y) # We take one difference to make data stationary in the regular data
ts.plot(z) # Now data is stationary in the mean for the regular part, but not for the seasonal
title("Y - one difference in the regular part")

# We also notice that data is characterized by an increasing variance, after removing seasonality difference we can assess if we need log transformation in the original data

# We try to identify if data is stationary
ndiffs(z, alpha=0.05, test=c("adf")) # regular differences? Now 0
nsdiffs(z,m=s,test=c("ocsb"))  # seasonal differences? Still 1

#a and b are the same model, for coherence we go with the a model (to see the various steps)
#a
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s))
#b
# fit<-arima(z,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=s)) 

# Plotting the residuals
par(mar=c(3,3,3,3))
ts.plot(fit$residuals)
title("Residuals - one difference on both regular and seasonal part")
# After removing the differences in the regular and seasonal parts, we still have data that is not stationary in the variance, 
# so we go back at the beginning and we apply the log operator (to achieve fully stationary data in both mean and variance)

#LOG TRANSFORMATION ON ORIGINAL DATA

y_ln <- log(y)

ts.plot(y_ln) # We identify that the data has an upward trend, is not stationary in the mean but is stationary in the variance
# Probably we are looking at seasonal data

#we try to identify if data is stationary
ndiffs(y_ln, alpha=0.05, test=c("adf")) # regular differences? =1

nsdiffs(y_ln,m=s,test=c("ocsb"))  # seasonal differences? =1

# We identify that the data is not stationary, because we need one difference in both regular and seasonal part of the data

z_ln <- diff(y_ln) # We remove the non-stationary in the regular data

ts.plot(z_ln) # Now data is stationary in the mean for the regular part, but not for the seasonal
nsdiffs(z_ln,m=s,test=c("ocsb"))  # seasonal differences? =1
# After taking the log, we can see that data is now stationary in the variance

#a
fit<-arima(y_ln,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) #Base model with stationary data
fit
ts.plot(fit$residuals)
title("Residuals - log(Y) with 1 diff in regular and seasonal part")

# It seems residuals are stationary in both mean and variance
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
title("ACF")
pacf(fit$residuals,nlags)  
title("PACF")

# looking at the ACF, we can see that for the seasonal part, we have only lag 4 out of boundary, which identifies a SMA(1), because s = 4, but we can see that we have also lag 1 out of bound, that can identify some relationships in the regular part
# looking at the PACF, we can see that for the seasonal part, we have lags 4 and 8 out of bound, SAR(2)

### MODELING:

#MODEL 1
#ARIMA(0,1,0)(2,1,0) s = 4
fit1<-arima(y_ln,order=c(0,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit1
#coefficients are significant
par(mfrow=c(2,1))
acf(fit1$residuals,nlags)
pacf(fit1$residuals,nlags)  
# 1st and 5th lag out of bound
Box.test(fit1$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 2
#ARIMA(1,1,0)(2,1,0) s = 4
fit2<-arima(y_ln,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s))
fit2
# Coefficients are significant
par(mfrow=c(2,1))
acf(fit2$residuals,nlags)
pacf(fit2$residuals,nlags)
# We can see that the first lag is not out of bound anymore, but still the fifth is
Box.test(fit2$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 3
#ARIMA(0,1,1)(2,1,0) s = 4
fit3<-arima(y_ln,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s)) 
fit3
# Coefficients are significant
par(mfrow=c(2,1))
acf(fit3$residuals,nlags)
pacf(fit3$residuals,nlags)
# We can see that the first lag is not out of bound anymore, but still the fifth is
Box.test(fit3$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 4
#ARIMA(5,1,0)(2,1,0) s = 4
fit4<-arima(y_ln,order=c(5,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit4
#coefficients 3, 5 and S1 are not significant
par(mfrow=c(2,1))
acf(fit4$residuals,nlags)
pacf(fit4$residuals,nlags)
# Nothing out of boundaries in both ACF and PACF
Box.test(fit4$residuals,lag=36) #p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 5
#ARIMA(0,1,5)(2,1,0) s = 4
fit5<-arima(y_ln,order=c(0,1,5),seasonal=list(order=c(2,1,0),period=s)) 
fit5
# Coefficients 2, 3, S1 and S2 are not significant
par(mfrow=c(2,1))
acf(fit5$residuals,nlags)
pacf(fit5$residuals,nlags)
# Lag 5 is still slightly out of bound in both ACF and PACF
Box.test(fit5$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 6
#ARIMA(1,1,1)(2,1,0) s = 4
fit6<-arima(y_ln,order=c(1,1,1),seasonal=list(order=c(2,1,0),period=s)) 
fit6
#coefficients AR1, MA1 are not significant
par(mfrow=c(2,1))
acf(fit6$residuals,nlags)
pacf(fit6$residuals,nlags)
# Lag 5 is still slightly out of bound in both ACF and PACF
Box.test(fit6$residuals,lag=36) #p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 7
#ARIMA(0,1,0)(0,1,1) s = 4
fit7<-arima(y_ln,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit7
# Coefficient is significant
ts.plot(fit$residuals)
# It seems residuals are stationary in both mean and variance
par(mfrow=c(2,1))
acf(fit7$residuals,nlags)
pacf(fit7$residuals,nlags) 
# We have lag one clearly out of bound in both ACF and PACF, but formal test tell us that data is already WN, so we will check AR(1) and MA(1) for regular part
Box.test(fit7$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 8
#ARIMA(1,1,0)(0,1,1) s = 4
fit8<-arima(y_ln,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit8
# Coefficients are significant
par(mfrow=c(2,1))
acf(fit8$residuals,nlags)
pacf(fit8$residuals,nlags) 
# We have only the fifth lag slightly out of bound in both ACF and PACF
Box.test(fit8$residuals,lag=36) #p value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 9
#ARIMA(0,1,1)(0,1,1) s = 4
fit9<-arima(y_ln,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s)) 
fit9
# Coefficients are significant
par(mfrow=c(2,1))
acf(fit9$residuals,nlags)
pacf(fit9$residuals,nlags) 
# It seems there is no lag out of bound in both ACF and PACF
Box.test(fit9$residuals,lag=36) #p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#Model 10
#ARIMA(0,1,0)(1,1,0) s = 4
fit10<-arima(y_ln,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit10
# Coefficient is significant
ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit10$residuals,nlags)
pacf(fit10$residuals,nlags)
# We have the first and fifth lags out of bound in both ACF and PACF
Box.test(fit10$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#MODEL 11
#ARIMA(1,1,0)(1,1,0) s = 4
fit11<-arima(y_ln,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit11
#coefficients are significant
ts.plot(fit11$residuals)
par(mfrow=c(2,1))
acf(fit11$residuals,nlags)
pacf(fit11$residuals,nlags)  
Box.test(fit11$residuals,lag=36) # p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


#Model 12
#ARIMA(0,1,5)(1,1,0) s = 4
fit12<-arima(y_ln,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=s)) 
fit12
# Coefficients are significant
ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit12$residuals,nlags)
pacf(fit12$residuals,nlags)  
# We have only the fifth lag slightly out of bound in both ACF and PACF
Box.test(fit12$residuals,lag=36) #p-value is higher than 0.05, so we cannot reject h0, so data seems not linearly dependent


### FORECASTING ###
#Forecasting performance exercise
y_train <- datos[,2][1:83] #calculate the train data (leave out last 24 obs)
y_train_ln <- log(y_train)
#z_train_ln <- diff(y_train)

# List of models 
fit1<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 1
fit2<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 2
fit3<-arima(y_train_ln,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s)) #MODEL 3
# fit4<-arima(y_train_ln,order=c(5,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 4
# fit5<-arima(y_train_ln,order=c(0,1,5),seasonal=list(order=c(2,1,0),period=s)) #MODEL 5
# fit6<-arima(y_train_ln,order=c(1,1,1),seasonal=list(order=c(2,1,0),period=s)) #MODEL 6
fit7<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s)) #MODEL 7
fit8<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) #MODEL 8
fit9<-arima(y_train_ln,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s)) #MODEL 9
fit10<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) #MODEL 10
fit11<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) #MODEL 11
fit12<-arima(y_train_ln,order=c(0,1,5),seasonal=list(order=c(1,1,0),period=s)) #MODEL 12

### Plotting the predictions vs real data
# Setting the parameters for the graphs
par(mfrow=c(3,3)) # // plotting all the graphs in one screen (we used a tv)
# par(mfrow=c(3,1)) // plotting 3 for 1


#Now we start with a different analysis, visualizing in a graphical way the difference between real data and the predictions
#fit1
fit1<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 1
y.pred<-predict(fit1,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 1")

#fit2
fit2<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 2
y.pred<-predict(fit2,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 2")

#fit3
fit3<-arima(y_train_ln,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s)) #MODEL 3
y.pred<-predict(fit3,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 3")

#fit4
# fit4<-arima(y_train_ln,order=c(5,1,0),seasonal=list(order=c(2,1,0),period=s)) #MODEL 4
# y.pred<-predict(fit4,n.ahead=24) #predict the next 24 values from the 83rd point onward
# y.pred$pred #visualize the point predictions of the 24 future points
# y.pred$se #visualize the standard error of the point predictions of the 24 future points
# pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
# real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
# c=1:24
# plot(c,real,type="b",col="red") #plot the real values
# lines(c,pred_fin,col="blue",type="b") #plot the predicted values
# legend("bottomleft",c("real","forecast"),
#        col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
# title("Model 4")

# #fit5
# fit5<-arima(y_train_ln,order=c(0,1,5),seasonal=list(order=c(2,1,0),period=s)) #MODEL 5
# y.pred<-predict(fit5,n.ahead=24) #predict the next 24 values from the 83rd point onward
# y.pred$pred #visualize the point predictions of the 24 future points
# y.pred$se #visualize the standard error of the point predictions of the 24 future points
# pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
# real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
# c=1:24
# plot(c,real,type="b",col="red") #plot the real values
# lines(c,pred_fin,col="blue",type="b") #plot the predicted values
# legend("bottomleft",c("real","forecast"),
#        col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
# title("Model 5")
# 
# #fit6
# fit6<-arima(y_train_ln,order=c(1,1,1),seasonal=list(order=c(2,1,0),period=s)) #MODEL 6
# y.pred<-predict(fit6,n.ahead=24) #predict the next 24 values from the 83rd point onward
# y.pred$pred #visualize the point predictions of the 24 future points
# y.pred$se #visualize the standard error of the point predictions of the 24 future points
# pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
# real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
# c=1:24
# plot(c,real,type="b",col="red") #plot the real values
# lines(c,pred_fin,col="blue",type="b") #plot the predicted values
# legend("bottomleft",c("real","forecast"),
#        col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
# title("Model 6")

#fit7
fit7<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s)) #MODEL 7
y.pred<-predict(fit7,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 7")

#fit8
fit8<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) #MODEL 8
y.pred<-predict(fit8,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 8")

#fit9
fit9<-arima(y_train_ln,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s)) #MODEL 9
y.pred<-predict(fit9,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 9")

#fit10
fit10<-arima(y_train_ln,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) #Model 10
y.pred<-predict(fit10,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 10")

#fit11
fit11<-arima(y_train_ln,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) #MODEL 11
y.pred<-predict(fit11,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 11")

#fit12
fit12<-arima(y_train_ln,order=c(0,1,5),seasonal=list(order=c(1,1,0),period=s)) #Model 12
y.pred<-predict(fit12,n.ahead=24) #predict the next 24 values from the 83rd point onward
y.pred$pred #visualize the point predictions of the 24 future points
y.pred$se #visualize the standard error of the point predictions of the 24 future points
pred_fin <- exp(y.pred$pred) #undo the log transformation we have done at the beginning
real<-datos[,2][84:107] #taking real values from the original dataset that we will use to compare with the predictions
c=1:24
plot(c,real,type="b",col="red") #plot the real values
lines(c,pred_fin,col="blue",type="b") #plot the predicted values
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" ) #plot the legend
title("Model 12")

# After plotting the graphs we have seen that the most promising models are 8 and 9
n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 24 observations
horizontes<-8 # number of periods ahead



## FORECASTING EXERCISE ##

# Setting up the forecasting exercise
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)]

MSFE1<-matrix(0,nrow=horizontes,ncol=1)
MAPE1<-matrix(0,nrow=horizontes,ncol=1)

MSFE2<-matrix(0,nrow=horizontes,ncol=1)
MAPE2<-matrix(0,nrow=horizontes,ncol=1)

MSFE3<-matrix(0,nrow=horizontes,ncol=1)
MAPE3<-matrix(0,nrow=horizontes,ncol=1)

MSFE4<-matrix(0,nrow=horizontes,ncol=1)
MAPE4<-matrix(0,nrow=horizontes,ncol=1)

MSFE5<-matrix(0,nrow=horizontes,ncol=1)
MAPE5<-matrix(0,nrow=horizontes,ncol=1)

MSFE6<-matrix(0,nrow=horizontes,ncol=1)
MAPE6<-matrix(0,nrow=horizontes,ncol=1)

MSFE7<-matrix(0,nrow=horizontes,ncol=1)
MAPE7<-matrix(0,nrow=horizontes,ncol=1)

MSFE8<-matrix(0,nrow=horizontes,ncol=1)
MAPE8<-matrix(0,nrow=horizontes,ncol=1)

MSFE9<-matrix(0,nrow=horizontes,ncol=1)
MAPE9<-matrix(0,nrow=horizontes,ncol=1)

MSFE10<-matrix(0,nrow=horizontes,ncol=1)
MAPE10<-matrix(0,nrow=horizontes,ncol=1)

MSFE11<-matrix(0,nrow=horizontes,ncol=1)
MAPE11<-matrix(0,nrow=horizontes,ncol=1)

MSFE12<-matrix(0,nrow=horizontes,ncol=1)
MAPE12<-matrix(0,nrow=horizontes,ncol=1)


### RECURSIVE FORECASTING ###

#Model8
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE8[Periods_ahead]<-mean(error^2);
  MAPE8[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE8
MSFE8


#Model9
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE9[Periods_ahead]<-mean(error^2);
  MAPE9[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE9
MSFE9

#For the completeness of the analysis, we want to check the forecast accuracy for the rest of the models

#Model1
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE1[Periods_ahead]<-mean(error^2);
  MAPE1[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE1
MSFE1


#Model2
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE2[Periods_ahead]<-mean(error^2);
  MAPE2[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE2
MSFE2


#Model3
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE3[Periods_ahead]<-mean(error^2);
  MAPE3[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE3
MSFE3


#Model7
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE7[Periods_ahead]<-mean(error^2);
  MAPE7[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE7
MSFE7


#Model10
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE10[Periods_ahead]<-mean(error^2);
  MAPE10[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE10
MSFE10


#Model11
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE11[Periods_ahead]<-mean(error^2);
  MAPE11[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE11
MSFE11


#Model12
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,5),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE12[Periods_ahead]<-mean(error^2);
  MAPE12[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE12
MSFE12


# Dataframe with all the MAPE results
df_recursive <- data.frame(m1 = MAPE1, m2 = MAPE2, m3 = MAPE3, m7 = MAPE7, m8 = MAPE8, m9 = MAPE9, m10 = MAPE10, m11 = MAPE11, m12 = MAPE12)
df_recursive$AVG <- c(NA, NA, NA, NA)
df_recursive$AVG <- rowMeans(df_recursive[,1:9])
df_recursive$MIN <- rowMins(df_recursive[,1:9])
df_recursive$MAX <- rowMaxs(df_recursive[,1:9])
df_recursive

write.csv(df_recursive, "path/forecasting_recursive.csv", row.names=FALSE, quote=FALSE)


### ROLLING FORECASTING ###

# Setting up the forecasting exercise
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)]

MSFE1<-matrix(0,nrow=horizontes,ncol=1)
MAPE1<-matrix(0,nrow=horizontes,ncol=1)

MSFE2<-matrix(0,nrow=horizontes,ncol=1)
MAPE2<-matrix(0,nrow=horizontes,ncol=1)

MSFE3<-matrix(0,nrow=horizontes,ncol=1)
MAPE3<-matrix(0,nrow=horizontes,ncol=1)

# MSFE4<-matrix(0,nrow=horizontes,ncol=1)
# MAPE4<-matrix(0,nrow=horizontes,ncol=1)
# 
# MSFE5<-matrix(0,nrow=horizontes,ncol=1)
# MAPE5<-matrix(0,nrow=horizontes,ncol=1)
# 
# MSFE6<-matrix(0,nrow=horizontes,ncol=1)
# MAPE6<-matrix(0,nrow=horizontes,ncol=1)

MSFE7<-matrix(0,nrow=horizontes,ncol=1)
MAPE7<-matrix(0,nrow=horizontes,ncol=1)

MSFE8<-matrix(0,nrow=horizontes,ncol=1)
MAPE8<-matrix(0,nrow=horizontes,ncol=1)

MSFE9<-matrix(0,nrow=horizontes,ncol=1)
MAPE9<-matrix(0,nrow=horizontes,ncol=1)

MSFE10<-matrix(0,nrow=horizontes,ncol=1)
MAPE10<-matrix(0,nrow=horizontes,ncol=1)

MSFE11<-matrix(0,nrow=horizontes,ncol=1)
MAPE11<-matrix(0,nrow=horizontes,ncol=1)

MSFE12<-matrix(0,nrow=horizontes,ncol=1)
MAPE12<-matrix(0,nrow=horizontes,ncol=1)

#Here we calculate the MAPE and MSFE for the 2 most promising models

#Model8
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE8[Periods_ahead]<-mean(error^2);
  MAPE8[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE8
MSFE8


#Model9
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE9[Periods_ahead]<-mean(error^2);
  MAPE9[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE9
MSFE9

#For the completeness of the analysis, we want to check the forecast accuracy for the rest of the models

#Model1
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE1[Periods_ahead]<-mean(error^2);
  MAPE1[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE1
MSFE1


#Model2
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE2[Periods_ahead]<-mean(error^2);
  MAPE2[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE2
MSFE2


#Model3
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE3[Periods_ahead]<-mean(error^2);
  MAPE3[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE3
MSFE3

#Model7
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE7[Periods_ahead]<-mean(error^2);
  MAPE7[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE7
MSFE7


#Model10
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE10[Periods_ahead]<-mean(error^2);
  MAPE10[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE10
MSFE10


#Model11
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE11[Periods_ahead]<-mean(error^2);
  MAPE11[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE11
MSFE11


#Model12
for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y_ln[i:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(0,1,5),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead];
  MSFE12[Periods_ahead]<-mean(error^2);
  MAPE12[Periods_ahead]<-mean(abs(error/real)) *100;
}
MAPE12
MSFE12

# Dataframe with all the MAPE results
df_rolling <- data.frame(m1 = MAPE1, m2 = MAPE2, m3 = MAPE3, m7 = MAPE7, m8 = MAPE8, m9 = MAPE9, m10 = MAPE10, m11 = MAPE11, m12 = MAPE12)
df_rolling$AVG <- c(NA, NA, NA, NA)
df_rolling$AVG <- rowMeans(df_rolling[,1:9])
df_rolling$MIN <- rowMins(df_rolling[,1:9])
df_rolling$MAX <- rowMaxs(df_rolling[,1:9])
df_rolling

write.csv(df_rolling, "path/forecasting_rolling.csv", row.names=FALSE, quote=FALSE)
