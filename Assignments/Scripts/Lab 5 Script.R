library(forecast)
library(tseries)
library(ggplot2)

set.seed(123)
model1 = arima.sim(model = list(order = c(1,0,0), ar = 0.6), n = 100)
model2 = arima.sim(model = list(ma = 0.9), n = 100)
model3 = arima.sim(model = list(order = c(1,0,1), ar = 0.6, ma = 0.9), n = 100)

par(mfrow = c(1,3))
acf1 = Acf(model1, lag.max = 40, main = "ACF of ARMA(1,0)")
acf2 = Acf(model2, lag.max = 40, main = "ACF of ARMA(0,1)")
acf3 = Acf(model3, lag.max = 40, main = "ACF of ARMA(1,1)")

par(mfrow = c(1,3))
pacf1 = Pacf(model1, lag.max = 40, main = "PACF of ARMA(1,0)")
pacf2 = Pacf(model2, lag.max = 40, main = "PACF of ARMA(0,1)")
pacf3 = Pacf(model3, lag.max = 40, main = "PACF of ARMA(1,1)")

plot(model1)
plot(model2)
plot(model3)

#N = 1000

set.seed(123)
model4 = arima.sim(model = list(order = c(1,0,0), ar = 0.6), n = 1000)
model5 = arima.sim(model = list(ma = 0.9), n = 1000)
model6 = arima.sim(model = list(order = c(1,0,1), ar = 0.6, ma = 0.9), n = 1000)

par(mfrow = c(1,3))
acf4 = Acf(model1, lag.max = 40, main = "ACF of ARMA(1,0)")
acf5 = Acf(model2, lag.max = 40, main = "ACF of ARMA(0,1)")
acf6 = Acf(model3, lag.max = 40, main = "ACF of ARMA(1,1)")

par(mfrow = c(1,3))
pacf4 = Pacf(model1, lag.max = 40, main = "PACF of ARMA(1,0)")
pacf5 = Pacf(model2, lag.max = 40, main = "PACF of ARMA(0,1)")
pacf6 = Pacf(model3, lag.max = 40, main = "PACF of ARMA(1,1)")

#QUESTION 4

set.seed(123)
model = sim_sarima(model = list(ma = 0.5, sar = 0.8), 
                   nseasons = 12, n = 100)

par(mfrow = c(1,2))
model_acf = Acf(model, lag.max = 40, main = "ACF of SARIMA Model")
model_pacf = Pacf(model, lag.max = 40, main = "PACF of SARIMA Model")
