#dat3--BankNifty
#### First we have to find the lag order, 
m_lag3 = VARselect(dat3, lag.max = 10, type = "const")
m_lag3$selection[1]

### Fitting the VAR model
varm3 = VAR(dat3, p = m_lag3$selection[1], type = "const")
summary(varm3)
names(summary(varm3))

### Prediction using the VAR model
predict(varm3, n.ahead = 1)

## Doing the cointegration test on the prices

### Fitting the VAR order of the prices data
price_lag3 = VARselect(data[c(2,5)], lag.max = 10, type = "const")

caMod3 = ca.jo(data[c(2,5)], type = "eigen", K = price_lag3$selection[1], spec="longrun", ecdet = "const")
summary(caMod3)

### Extracting the covariance matrix
covm3 = summary(varm3)$covres

### Finding the hedge ration
hratio3 = covm3[2]/covm3[4]
hratio3

#Hedge Effectiveness
R_unhedged3 <- rs
R_hedged3 <- rs-hratio3*rf3

Var_unhedged3 <- var(rs)
var_hedged3 <- var(rs)+(hratio3^2)*var(rf3)-2*hratio3*var(rs,rf3)

Hedge_effectiveness3 <- (Var_unhedged3- var_hedged3)/Var_unhedged3 
