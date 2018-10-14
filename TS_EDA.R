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

autoplot(movies.ts)

ggseasonplot(movies.ts) + 
  scale_y_log10(labels = scales::dollar)


ggseasonplot(movies.ts, polar=TRUE)

#--filter from year 2000
movies2000.ts = window(movies.ts, start=2000)

autoplot(movies2000.ts) +
  scale_y_log10(labels = scales::dollar)

ggseasonplot(movies2000.ts) + 
  scale_y_log10(labels = scales::dollar)






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

#run ljung-box test
Box.test(movies.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value < 2.2e-16 (suggests this is data is not white noise.)


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

#----------------------------------------COMPARE TWO MOVIES

wMovies = read.csv("WeeklyBoxOfficeResults2015-2018.csv")

str(wMovies)

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "-", "n/a")

#----------------------Initial cleaning of data
  
wMovies = wMovies %>%
  mutate(
    WeeklyGross = trimws(WeeklyGross),
    TheatreCount = trimws(TheatreCount),
    TotalGross = trimws(TotalGross),
    Budget = trimws(Budget)
  ) %>% 
  replace_with_na(replace= list(
                            WeeklyGross = na_strings,
                            TheatreCount = na_strings,
                            Average = na_strings,
                            TotalGross = na_strings,
                            Budget = na_strings
                            )
                  ) %>% 
  mutate(
    WeeklyGross = as.numeric(gsub("[\\$,]", "", WeeklyGross)),
    TheatreCount = as.numeric(gsub("[\\$,]", "", TheatreCount)),
    Average = as.numeric(gsub("[\\$,]", "", Average)),
    TotalGross = as.numeric(gsub("[\\$,]", "", TotalGross)),
    Budget = formatC(as.numeric(gsub("[\\$,]", "", Budget)) * 1000000, digits=10, format="d")
  ) 


mTaken = wMovies %>%
  filter(Title %in% c("Taken 3"))

mPadd = wMovies %>%
  filter(Title %in% c("Paddington"))


mTaken.ts = ts(mTaken$WeeklyGross, start=c(2015, 2), frequency=52)

mPadd.ts = ts(mPadd$WeeklyGross, start=c(2015, 3), frequency=52)

print(mTaken.ts)
print(mPadd.ts)

plot(mTaken.ts)
plot(mPadd.ts)


mTaken.diff = diff(mTaken.ts)
plot(mTaken.diff)
acf(mTaken.diff)

mTaken.log = log(mTaken.ts)
plot(mTaken.log)
acf(mTaken.log)

mTaken.difflog = diff(log(mTaken.ts))
plot(mTaken.difflog)
acf(mTaken.difflog)


mPadd.diff = diff(mPadd.ts)
plot(mPadd.diff)
acf(mPadd.diff)

mPadd.log = log(mPadd.ts)
plot(mPadd.log)
acf(mPadd.log)

mPadd.difflog = diff(log(mPadd.ts))
plot(mPadd.difflog)
acf(mPadd.difflog)


#----------------------------------train / test split - for paddington

mPadd.train = window(mPadd.ts, end=c(2015, 17))
mPadd.test = window(mPadd.ts, start=c(2015,18), end=c(2015,24))

#-----------TRAINING DATA

mPadd.train.tbats = tbats(mPadd.train)
mPadd.train.tbats$seasonal.periods

#check model via forecast
mPadd.train.forecast = forecast(mPadd.train)
summary(mPadd.train.forecast)

print(mPadd.train)
print(mPadd.test)


mPadd.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.5864 - relatively stationary


#------fit arima AR model
mPadd.ar = arima(mPadd.ts, order=c(1,0,0))

mPadd.ar %>%
  forecast() %>%
  autoplot()

checkresiduals(mPadd.ar)

mPadd.train.predict_ar = forecast(mPadd.ar)

#-----fit arima MA model
mPadd.ma = arima(mPadd.ts, order=c(0,0,1))

mPadd.ma %>%
  forecast() %>%
  autoplot()

checkresiduals(mPadd.ma)

mPadd.train.predict_ma = forecast(mPadd.ma)




#---------------------------------------------------------------------------------------------
#-----------------------------TS BY GENRE----------------------------------------
#---------------------------------------------------------------------------------------------

mMovies = read.csv("MonthlyMoviesResults.csv", stringsAsFactors = FALSE)

str(mMovies)

moviesFull = read.csv(file="MovieClean3.csv", stringsAsFactors = FALSE)

str(moviesFull)

moviesFull = moviesFull %>%
  select(
    ID,
    imdbID,
    TMDBID,
    Title,
    Runtime,
    Rated,
    Awards,
    IMDB_Rating,
    Wide_Release,
    G_Action:G_Western
  )


moviesComb = left_join(mMovies, moviesFull, by=c("Title"))

write.csv(moviesComb, "MonthlyMoviesCombined.csv")


#--------------------------------------------

moviesComb = read.csv("MonthlyMoviesCombined.csv")

moviesComb = moviesComb %>%
  mutate(RecordDate = as.Date(paste(Year, Month, 1, sep="-")))
  




#-------------------------EDA

#------------------------------ACTION
action = moviesComb %>%
  filter(G_Action == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales))%>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100)


action.ts = ts(action$AverageSales, start=c(2014,6), frequency=12)
autoplot(action.ts)
ggseasonplot(action.ts) + 
  scale_y_log10(labels = scales::dollar)


ggseasonplot(action.ts, polar=TRUE)


action.tbats = tbats(action.ts)
action.tbats$seasonal.periods


action.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.1288 (stationary)

#run ljung-box test
Box.test(action.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value = 0.2106 - not significant therefore stationary

ggAcf(action.ts)


#check model via forecast
action.forecast = forecast(action.ts)
summary(action.forecast)

action.ets = ets(action.ts)
summary(action.ets)

action.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(action.ets)







#------------------------------FAMILY
family = moviesComb %>%
  filter(G_Family == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales)) %>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100)


#test = family %>%
#  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month"))
#write.csv(test, "test.csv")

family.ts = ts(family$AverageSalesAdj, start=c(2014,6), frequency=12)
autoplot(family.ts)
ggseasonplot(family.ts) + 
  scale_y_log10(labels = scales::dollar)

ggseasonplot(family.ts, polar=TRUE)


family.tbats = tbats(family.ts)
family.tbats$seasonal.periods


family.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.1288 (stationary)

#run ljung-box test
Box.test(family.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value = 0.9984 - not significant therefore stationary

ggAcf(family.ts)


#check model via forecast
family.forecast = forecast(family.ts)
summary(family.forecast)


#------fit ETS model
family.ets = ets(family.ts)
summary(family.ets)

family.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(family.ets)


#------fit arima AR model
family.ar = arima(family.ts, order=c(1,0,0))

family.ar %>%
  forecast() %>%
  autoplot()

checkresiduals(family.ar)

#mPadd.train.predict_ar = forecast(mPadd.ar)

#-----fit arima MA model
family.ma = arima(family.ts, order=c(0,0,1))

family.ma %>%
  forecast() %>%
  autoplot()

checkresiduals(family.ma)


#------------------------------HORROR
horror = moviesComb %>%
  filter(G_Horror == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales)) %>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100)



horror.ts = ts(horror$AverageSales, start=c(2014,6), frequency=12)
autoplot(horror.ts)
ggseasonplot(horror.ts) + 
  scale_y_log10(labels = scales::dollar)

ggseasonplot(horror.ts, polar=TRUE)



horror.tbats = tbats(horror.ts)
horror.tbats$seasonal.periods


horror.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.5868 (stationary)

#run ljung-box test
Box.test(horror.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value = 0.137 - not significant therefore stationary

ggAcf(horror.ts)


#check model via forecast
horror.forecast = forecast(horror.ts)
summary(horror.forecast)


#------fit ETS model
horror.ets = ets(horror.ts)
summary(horror.ets)

horror.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(horror.ets)


#------fit arima AR model
horror.ar = arima(horror.ts, order=c(1,0,0))

horror.ar %>%
  forecast() %>%
  autoplot()

checkresiduals(horror.ar)

#mPadd.train.predict_ar = forecast(mPadd.ar)

#-----fit arima MA model
horror.ma = arima(horror.ts, order=c(0,0,1))

horror.ma %>%
  forecast() %>%
  autoplot()

checkresiduals(horror.ma)





#------------------------------ANIMATION
animation = moviesComb %>%
  filter(G_Animation == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales)) %>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100)
  
  
animation.ts = ts(animation$AverageSalesAdj, start=c(2014,6), frequency=12)
autoplot(animation.ts)

ggseasonplot(animation.ts) + 
  scale_y_log10(labels = scales::dollar)

ggseasonplot(animation.ts, polar=TRUE)



animation.tbats = tbats(animation.ts)
animation.tbats$seasonal.periods


animation.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 0.1336 (stationary)

#run ljung-box test
Box.test(animation.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value = 0.3111 - not significant therefore stationary

ggAcf(animation.ts)


#check model via forecast
animation.forecast = forecast(animation.ts)
summary(animation.forecast)


#------fit ETS model
animation.ets = ets(animation.ts)
summary(animation.ets)

animation.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(animation.ets)


#------fit arima AR model
animation.ar = arima(animation.ts, order=c(1,0,0))

animation.ar %>%
  forecast() %>%
  autoplot()

checkresiduals(animation.ar)

#mPadd.train.predict_ar = forecast(mPadd.ar)

#-----fit arima MA model
animation.ma = arima(animation.ts, order=c(0,0,1))

animation.ma %>%
  forecast() %>%
  autoplot()

checkresiduals(animation.ma)

#---------------------------------------------------




