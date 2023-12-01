library(itsmr)
library("readxl")
library(tseries) # for ADF test
library(forecast) #for forecasting and SARIMA model
library(astsa) #for sariam model

#import original dataset
port_orford <- read_excel("/Users/thitruong//Desktop/Illinois Tech/Spring 2023/MATH 446/m446-Project/m446-project-data/PortOrford.xlsx")
san_francisco <- read_excel("/Users/thitruong//Desktop/Illinois Tech/Spring 2023/MATH 446/m446-Project/m446-project-data/San_Francisco.xlsx")
seattle <- read_excel("/Users/thitruong//Desktop/Illinois Tech/Spring 2023/MATH 446/m446-Project/m446-project-data/Seattle.xlsx")

#*MHW: Mean High Water*
#The average of all the high water heights observed over the National Tidal Datum Epoch. 
#For stations with shorter series, comparison of simultaneous observations with a control 
#tide station is made in order to derive the equivalent datum of the National Tidal Datum Epoch.

#WE ONLY interested in the highest value of high water heights
layout(1:1)
highest_SanFrancisco <- san_francisco$Highest
plot(highest_SanFrancisco, type = "b",ylab="Highest of Mean High Water (ft)",
     xlab= "Month", main = "The Average of Highest Water Heights in San Francisco, CA")

highest_PortOrford <- port_orford$Highest
plot(highest_PortOrford, type = "b",ylab="Highest of Mean High Water (ft)",
     xlab= "Month", main = "The Average of Highest Water Heights in Port Orford, OR")

highest_Seattle <- seattle$Highest
plot(highest_Seattle, type = "b",ylab="Highest of Mean High Water (ft)",
     xlab= "Month", main = "The Average of Highest Water Heights in Seattle, WA")

#====================================================

#THIS ONE IS JUST FOR A BETTER LOOK ON OUR DATA
#time- series dataset for the highest water from 1/2012 - 12/2022
ts_san_francisco <- ts(highest_SanFrancisco, frequency = 12, start =c(2012,1), end = c(2022,12))
ts_san_francisco #display the data

ts_port_orford <- ts(highest_PortOrford, frequency = 12, start =c(2012,1), end = c(2022,12))
ts_port_orford #display the data

ts_seattle<- ts(highest_Seattle, frequency = 12, start =c(2012,1), end = c(2022,12))
ts_seattle#display the data

#=============================================================

#TIME-SERIES plot (with time-axis by months)
plot(ts(highest_SanFrancisco), ylab="Highest Water Heights (ft)", xlab="Time (Month)",main="Time Series Plot 01/2012 - 01/2022")
#time-series plot (with time-axis by years)
plot(ts_san_francisco,ylab="Highest Water Heights (ft)",main="Time Series Plot")

plot(ts(highest_PortOrford), ylab="Highest Water Heights (ft)", xlab="Time (Month)",main="Time Series Plot 01/2012 - 01/2022")
#time-series plot (with time-axis by years)
plot(ts_port_orford,ylab="Highest Water Heights (ft)",main="Time Series Plot")

plot(ts(highest_Seattle), ylab="Highest Water Heights (ft)", xlab="Time (Month)",main="Time Series Plot 01/2012 - 01/2022")
#time-series plot (with time-axis by years)
plot(ts_seattle,ylab="Highest Water Heights (ft)",main="Time Series Plot")


#=====================================================
#detecting seasonality
#however, we don't use it for our analysis
tscomponents_san_francisco <- decompose(ts_san_francisco)
plot(tscomponents_san_francisco)

tscomponents_port_orford <- decompose(ts_port_orford)
plot(tscomponents_port_orford)

tscomponents_seattle <- decompose(ts_seattle)
plot(tscomponents_seattle)

#==========================================================
#NOT GOING USE IT! since it's not useful
#regression model 
regresmodel <- lm(highest_PortOrford ~ time(ts_PO))
regresmodel
plot(ts_port_orford,ylab="Highest Water Heights (ft)",main="Time Series Plot")
abline(regresmodel,col="red") #not relly useful graph
par(mfrow=c(2,2))
plot(regresmodel)

#==============================================================
#ACF and PACF
layout(1:1)
acf(highest_SanFrancisco,main="ACF of Series",lag.max=60)
pacf(highest_SanFrancisco,main="PACF of Series ", lag.max=60)
#NOTICE THAT BASED ON THE ACF PLOT, THE DIFFERENCE IS 6.
highestSF_Diff <- diff(highest_SanFrancisco,differences = 6)
acf(highestSF_Diff, lag.max = 100,main="ACF of Series - Differencing ")
pacf(highestSF_Diff,lag.max=100,main ="PACF of Series - Differencing ")

acf(highest_PortOrford,main="ACF of Series",lag.max=60)
pacf(highest_PortOrford,main="PACF of Series ", lag.max=60)
#NOTICE THAT BASED ON THE ACF PLOT, THE DIFFERENCE IS 12.
highestPO_Diff <- diff(highest_PortOrford,differences = 12)
acf(highestPO_Diff, lag.max = 100,main="ACF of Series - Differencing ")
pacf(highestPO_Diff,lag.max=100,main ="PACF of Series - Differencing ")

acf(highest_Seattle,main="ACF of Series",lag.max=60)
pacf(highest_Seattle,main="PACF of Series ", lag.max=60)
#NOTICE THAT BASED ON THE ACF PLOT, THE DIFFERENCE IS 12.
highestS_Diff <- diff(highest_PortOrford,differences = 12)
acf(highestS_Diff, lag.max = 100,main="ACF of Series - Differencing ")
pacf(highestS_Diff,lag.max=100,main ="PACF of Series - Differencing ")

#==================================================================

#We're going to test for different ARIMA model - minAICC

##A WAY TO FIND THE "BEST" MODEL - which minimization of the AIC and MLE to obtain an ARIMA model.

#San Francisco
check_model_SF <- auto.arima(ts(highest_SanFrancisco,frequency = 12)
                             ,trace = TRUE) #check for a best fit model
SF_model <- sarima(highest_SanFrancisco,2,0,2,0,0,2,12)
SF_model
#ARIMA Forecasting
layout(1:2)
pred_SFmodel <- sarima.for(highest_SanFrancisco, 12, 2,0,2,0,0,2,12, xlab="Month",
                          main="Forecating Highest Water Level of San Francisco, CA")
plot(pred_SFmodel$pred,ylab="Heightest Water Level (ft)",xlab="Month")
#check if the actual data
actualdataSF <- c(2.43,1.843,1.879)
pred_SFmodel_values <- pred_SFmodel$pred[1:3]
error_SFmodel <- actualdataSF-pred_SFmodel_values
SFtab <- data.frame(actualdataSF,pred_SFmodel_values,error_SFmodel)
colnames(SFtab) <- c("Actual Values","Predicted Values", "Error")
SFtab


#Port Orford
check_model_PO <- auto.arima(ts(highest_PortOrford,frequency = 12)
                             ,trace = TRUE) #check for a best fit model
PO_model <- sarima(highest_PortOrford,0,0,1,2,0,0,12)
PO_model
#ARIMA Forecasting
layout(1:2)
pred_POmodel <- sarima.for(highest_PortOrford, 12, 0,0,1,2,0,0,12, xlab="Month",
                           main="Forecating Highest Water Level of Port Orford, OR")
plot(pred_POmodel$pred,ylab="Heightest Water Level (ft)",xlab="Month")
#check if the actual data
actualdataPO <- c (3.32,1.457,1.804)
pred_POmodel_values <- pred_POmodel$pred[1:3]
error_POmodel <- actualdataPO-pred_POmodel_values
POtab <- data.frame(actualdataPO,pred_POmodel_values,error_POmodel)
colnames(POtab) <- c("Actual Values","Predicted Values", "Error")
POtab

#seattle
check_model_S <- auto.arima(ts(highest_Seattle,frequency = 12)
                             ,trace = TRUE) #check for a best fit model
S_model <- sarima(highest_Seattle,1,0,0,2,0,0,12)
S_model
#ARIMA Forecasting
layout(1:2)
pred_Smodel <- sarima.for(highest_Seattle, 12, 1,0,0,2,0,0,12, xlab="Month",
                          main="Forecating Highest Water Level of Seattle, WA")
plot(pred_Smodel$pred,ylab="Heightest Water Level (ft)",xlab="Month")
#check if the actual data
actualdataS <- c (3.114,2.399,1.963)
pred_Smodel_values <- pred_Smodel$pred[1:3]
error_Smodel <- actualdataS-pred_Smodel_values
Stab <- data.frame(actualdataS,pred_Smodel_values,error_Smodel)
colnames(Stab) <- c("Actual Values","Predicted Values", "Error")
Stab


