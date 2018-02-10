library(vars)
library(forecast)
library(tseries)

### Finding the cointegration test
library(urca)

### Importing the data
data <- read.csv(file = "VECM.csv")
data$Date <- as.Date(data$Date, format ="%d-%m-%Y")

rs <- diff(log(data$NAV),lag = 1)
rf1 <- diff(log(data$F1_NIFTY50),lag = 1)
rf2 <- diff(log(data$F2_NIFTY_MID50),lag = 1)
rf3 <- diff(log(data$F3_BANKNIFTY),lag = 1)


### Testing the stationarity of the return
adf.test(rf1, alternative = "stationary")
adf.test(rf2, alternative = "stationary")
adf.test(rf3, alternative = "stationary")
### Futures returns are stationary

adf.test(rs, alternative = "stationary")
### Spot returns are stationary

### Testing the stationarity of the prices
adf.test(data$F1_NIFTY50, alternative = "stationary")
adf.test(data$F2_NIFTY_MID50, alternative = "stationary")
adf.test(data$F3_BANKNIFTY, alternative = "stationary")
### Futures prices are stationary

adf.test(data$NAV, alternative = "stationary")
### Spot prices are stationary

#dats
dat1 <- cbind(rs,rf1)
dat2 <- cbind(rs,rf2)
dat3 <- cbind(rs,rf3)

#dat1--Nifty50
#### First we have to find the lag order, 
m_lag1 = VARselect(dat1, lag.max = 10, type = "const")
m_lag1$selection[1]

### Fitting the VAR model
varm1 = VAR(dat1, p = m_lag1$selection[1], type = "const")
summary(varm1)
names(summary(varm1))

### Prediction using the VAR model
predict(varm1, n.ahead = 1)


## Doing the cointegration test on the prices

### Fitting the VAR order of the prices data
price_lag1 = VARselect(data[c(2,3)], lag.max = 10, type = "const")

caMod1 = ca.jo(data[c(2,3)], type = "eigen", K = price_lag1$selection[1], spec="longrun", ecdet = "const")
summary(caMod1)

### Extracting the covariance matrix
covm1 = summary(varm1)$covres

### Finding the hedge ration
hratio1 = covm1[2]/covm1[4]
hratio1

#Hedge Effectiveness
R_unhedged1 <- rs
R_hedged1 <- rs-hratio1*rf1

Var_unhedged1 <- var(rs)
var_hedged1 <- var(rs)+(hratio1^2)*var(rf1)-2*hratio1*var(rs,rf1)

Hedge_effectiveness1 <- (Var_unhedged1- var_hedged1)/Var_unhedged1 

