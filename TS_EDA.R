# ----------------------------------------
# Assignment 3 - 
#   Archel Aguilar
#   980 817 67
#
#   Time series movie data - EDA
#------------------------------------------

#install.packages("dplyr")
library(dplyr)

#install.packages("magrittr")
library(magrittr)

#install.packages("lubridate")
library(lubridate)

#install.packages("tibble")
library(tibble)

#install.packages("tidyr")
library(tidyr)


#install.packages("ggplot2")
library(ggplot2)

#install.packages("naniar")
library(naniar)

#install.packages("purrr")
library(purrr)

#install.packages("Amelia")
library(Amelia)


#install.packages("plotly")
library(plotly)

#install.packages("Metrics")
library(Metrics)


#install.packages("stringr")
library(stringr)

#install.packages("forecast")
library(forecast)


#install.packages("urca")
library(urca)




movies = read.csv(file="MonthlyTSResults.csv", stringsAsFactors = FALSE)

movies = movies %>%
  mutate(Date = as.Date(paste(Year, Month, "1", sep="-")),
         SalesAdj = Sales/100
         )

str(movies)

movies.ts = ts(movies$SalesAdj, start=1982, frequency=12)


#-------------------------EDA
print(movies.ts)

summary(movies.ts)

ts.plot(movies.ts)

start(movies.ts)
end(movies.ts)

movies %>%
  ggplot(aes(y=Sales, x=Date)) +
  geom_point() +
  scale_y_log10(labels = scales::dollar)

movies %>%
  ggplot(aes(y=Sales, x=Date)) +
  geom_line() +
  geom_smooth() +
  scale_y_log10(labels = scales::dollar)



movies.diff = diff(movies.ts, s=0.5)
movies.diff1 = diff(movies.ts, s=12)


ts.plot(movies.diff)


acf(movies.ts, lag.max=48)

acf(movies.diff, lag.max=48)

acf(movies.diff1, lag.max=48)

#run KPSS test to see if data is stationary
#install.packages("urca")
#library(urca)

movies.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 3.5465 (reject null therefore not stationary)

movies.diff %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.0274 

#------try fourier transformation
#movies.fft = fft(movies.diff)

#acf(movies.fft)


#------fit arima AR model
arima(movies.diff, order=c(1,0,0))

#-----fit arima MA model
arima(movies.diff, order=c(0,0,1))



#---------------------MOVIES ADJUSTED---------------------------
movies.tsadj = ts(movies$SalesAdj, start=1982, frequency=12)

ts.plot(movies.tsadj)

movies.diffadj = diff(movies.tsadj, s=0.5)

ts.plot(movies.diffadj)

acf(movies.diffadj)

movies.diffadj %>%
  ur.kpss() %>%
  summary()


#movies.fft = fft(movies.diffadj)
#acf(movies.fft)

#------fit arima AR model
arima(movies.diffadj, order=c(1,0,0)) #aic = 14951

#-----fit arima MA model
arima(movies.diffadj, order=c(0,0,1)) #aic = 14734


#------try tbats
movies.tbats = tbats(movies.diffadj)

movies.tbats$seasonal.periods


test = forecast(movies.tsadj)
summary(test)

#--------fit ETS model

movies.ets = ets(movies.tsadj)
summary(movies.ets)

movies.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(movies.ets)

#------------------------------------------------------------



#------------train / test split
movies.train = window(movies.ts, end=c(2009, 12))
movies.test = window(movies.ts, start=c(2010,1), end=c(2016,12))

#-----------TRAINING DATA

movies.train.tbats = tbats(movies.train)
movies.train.tbats$seasonal.periods

#check model via forecast
movies.train.forecast = forecast(movies.train)
summary(movies.train.forecast)

movies.train.ets = ets(movies.train)
summary(movies.train.ets)


movies.train.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(movies.train.ets)

movies.train.predict = forecast(movies.train.ets)

#compare with test
accuracy(movies.train.predict, movies.test)

#compare with simple models
movies.train.avg = meanf(movies.train, h=50)
movies.train.naive = rwf(movies.train, h=50)
movies.train.drift = rwf(movies.train, drift=TRUE, h=50)

result = rbind(accuracy(movies.train.predict, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.avg, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.naive, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.drift, movies.test)[2, c(2,3,5,6)]
               )

rownames(result) = c("ETS", "Average", "Naive", "Drift")
result


#plot over test data
autoplot(movies.train) +
  autolayer(movies.test, PI=FALSE, series="Actual") +
  autolayer(movies.train.predict, PI=FALSE, series="ETS Forecast") +
  autolayer(movies.train.avg, PI=FALSE, series="Average") +
  autolayer(movies.train.naive, PI=FALSE, series="Naive") +
  autolayer(movies.train.drift, PI=FALSE, series="Drift") +
  scale_y_log10(labels = scales::dollar)


#run cross validation
forcastfn = function(y, h) {
  forecast(ets(y), h=h)
  }

e = tsCV(movies.ts, forcastfn, h=1)
sqrt(mean(e^2, na.rm=TRUE)) #RMSE (CV)
#2936853


#----------------------------------train / test split - for 2017

movies.train = window(movies.ts, end=c(2016, 12))
movies.test = window(movies.ts, start=c(2017,1), end=c(2018,9))

#-----------TRAINING DATA

movies.train.tbats = tbats(movies.train)
movies.train.tbats$seasonal.periods

#check model via forecast
movies.train.forecast = forecast(movies.train)
summary(movies.train.forecast)

movies.train.ets = ets(movies.train)
summary(movies.train.ets)


movies.train.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(movies.train.ets)

movies.train.predict = forecast(movies.train.ets)

#compare with test
accuracy(movies.train.predict, movies.test)

#compare with simple models
movies.train.avg = meanf(movies.train, h=50)
movies.train.naive = rwf(movies.train, h=50)
movies.train.drift = rwf(movies.train, drift=TRUE, h=50)

result = rbind(accuracy(movies.train.predict, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.avg, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.naive, movies.test)[2, c(2,3,5,6)],
               accuracy(movies.train.drift, movies.test)[2, c(2,3,5,6)]
)

rownames(result) = c("ETS", "Average", "Naive", "Drift")
result


#plot over test data
autoplot(movies.train) +
  autolayer(movies.test, PI=FALSE, series="Actual") +
  autolayer(movies.train.predict, PI=FALSE, series="ETS Forecast") +
  autolayer(movies.train.avg, PI=FALSE, series="Average") +
  autolayer(movies.train.naive, PI=FALSE, series="Naive") +
  autolayer(movies.train.drift, PI=FALSE, series="Drift") +
  scale_y_log10(labels = scales::dollar)


e = tsCV(movies.ts, forcastfn, h=1)
sqrt(mean(e^2, na.rm=TRUE)) #RMSE (CV)
#2936853

#743,682,200

#Next steps
# - repeat all steps for wide data



#-----------

