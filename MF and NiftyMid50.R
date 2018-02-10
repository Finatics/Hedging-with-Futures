#dat2--NiftyMid50
#### First we have to find the lag order, 
m_lag2 = VARselect(dat2, lag.max = 10, type = "const")
m_lag2$selection[1]

### Fitting the VAR model
varm2 = VAR(dat2, p = m_lag2$selection[1], type = "const")
summary(varm2)
names(summary(varm2))

### Prediction using the VAR model
predict(varm2, n.ahead = 1)

## Doing the cointegration test on the prices

### Fitting the VAR order of the prices data
price_lag2 = VARselect(data[c(2,4)], lag.max = 10, type = "const")

caMod2 = ca.jo(data[c(2,4)], type = "eigen", K = price_lag2$selection[1], spec="longrun", ecdet = "const")
summary(caMod2)

### Extracting the covariance matrix
covm2 = summary(varm2)$covres

### Finding the hedge ration
hratio2 = covm2[2]/covm2[4]
hratio2

#Hedge Effectiveness
R_unhedged2 <- rs
R_hedged2 <- rs-hratio2*rf2

Var_unhedged2 <- var(rs)
var_hedged2 <- var(rs)+(hratio2^2)*var(rf2)-2*hratio2*var(rs,rf2)

Hedge_effectiveness2 <- (Var_unhedged2- var_hedged2)/Var_unhedged2 
