library(forecast)

Kingston.Data <-read.csv("C:\\Users\\chinm\\Downloads\\kingston_clean.csv",
                         header=TRUE, sep=",")

#time series Temp celcius
Kingston_ts <- ts(Kingston.Data$Temp,start=1872, frequency=12)

#decomposed plots
decomp <- stl(Kingston_ts, t.window=12, s.window="periodic") 
plot(decomp)

Acf(Kingston_ts,main="")
Pacf(Kingston_ts, main="")

#train test split: 80:20
train<-window(Kingston_ts, end=c(1987,12))
test<-window(Kingston_ts, start=c(1988,01), end=c(2016,12))

#tbats metrics: rmse on test set: 1.975139
tbats_kingston <- tbats(train)
tbats_kingston
forecasted_temp_tbats<-forecast(tbats_kingston,h=348)
accuracy(forecasted_temp_tbats,test)

#arima metrics: rmse on test set: 2.295892
arima_kingston <- auto.arima(train, seasonal = TRUE,stepwise = FALSE,approximation = FALSE )
arima_kingston
forecasted_temp_arima<-forecast(arima_kingston,h=348)
accuracy(forecasted_temp_arima,test)

#Using tbats since it has a lower rmse, forecasting till 2100 using entire dataset from 1872 to 2016
kingston_pred <- tbats(Kingston_ts)
kingston_pred
forecasted_temp<-forecast(kingston_pred,h=1008,level=c(0.80,0.90))
plot(forecasted_temp)

#exporting the values to get point prediction
write.csv(forecasted_temp,"forecast.csv")
