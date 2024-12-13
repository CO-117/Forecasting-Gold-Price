library(rugarch)
library(quantmod)
library(tseries)
library(FinTS)
library(forecast)

# Split the data into training (2010-2020) and testing (2021-2022) sets
daily_data$Date <- as.Date(daily_data$Date)
train_data <- subset(daily_data, Date < as.Date("2022-06-01"))
test_data <- subset(daily_data, Date >= as.Date("2022-06-01"))
adf.test(train_data$Price)
adf.test(diff(log(train_data$Price)))
ArchTest(train_data$DiffPrice)
auto.arima(train_data$Price, trace= TRUE)

#ARIMA model
spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,0)), mean.model=list(armaOrder=c(1,1), arfima= TRUE), fixed.pars=list(arfima = 1), distribution.model="norm" )
garchfit=ugarchfit(spec = spec, solver="hybrid", data=train_data$Price)
garchfit
tsdisplay(garchfit@fit$residuals^2)
checkresiduals(garchfit@fit$residuals^2)
ugfore <- ugarchforecast(garchfit, n.ahead = 143)
ugfore
plot(ugfore)
1
3
0
d1= ugfore@forecast$seriesFor
rmse(d1, test_data$Price)
mse(d1, test_data$Price)
mape(d1, test_data$Price)
r1


#GARCH
spec1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                   distribution.model="norm")
garchfit1=ugarchfit(spec1, data=train_data$DiffPrice[-1])
garchfit1
ugfore1 <- ugarchforecast(garchfit1, n.ahead = 143)
plot(ugfore1)
1
3
0
d2= ugfore1@forecast$seriesFor
rmse(d2,test_data$DiffPrice)
mse(d2,test_data$DiffPrice)
mape(d2,test_data$DiffPrice)


#HYBRID ARMA-GARCH 
spec2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                       distribution.model="norm", fixed.pars=list(omega=0))
garchfit2=ugarchfit(spec2, data=train_data$DiffPrice[-1])
garchfit2
ugfore2 <- ugarchforecast(garchfit1, n.ahead = 143)
plot(ugfore2)
1
3
0
d3= ugfore2@forecast$seriesFor
d3
rmse(d3,test_data$DiffPrice)
mse(d3,test_data$DiffPrice)
mape(d3,test_data$DiffPrice)
