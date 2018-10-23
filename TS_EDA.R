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


tbreaks = c(0, 10, 100, 1000, 10000, 100000)
salesBreaks2 = c(0, 1000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)
salesBreaks3 = c(0, 500000000, 1000000000, 1500000000, 2000000000, 2500000000, 3000000000)


#-------------------------EDA
print(movies.ts)

summary(movies.ts)

autoplot(movies.ts)

start(movies.ts)
end(movies.ts)

ggseasonplot(movies.ts) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Month",
    y = "Box office Sales",
    title = "Box Office Sales Seasonal Plot (adj. for inflation) (1982-2018)",
    caption = "Data source: IMDb 2018"
  ) 


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
  scale_y_continuous(labels = scales::dollar, breaks=salesBreaks3) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Year",
    y = "Box office Sales",
    title = "Box Office Sales Trend (adj. for inflation) (1982-2018)",
    caption = "Data source: IMDb 2018"
  ) 


yearlySales = movies %>%
  filter(Year >= 2000) %>% 
  filter(Year != 2018) %>%
  group_by(Year) %>%
  summarise(Sales = sum(Sales))


yearlySales %>%
  ggplot(aes(y=Sales, x=Year)) +
    geom_point() +
    geom_smooth(method="lm") +
    scale_y_continuous(labels = scales::dollar)


yearlySales.ts = ts(yearlySales$Sales, start=2000, frequency=1)

autoplot(yearlySales.ts) +
  scale_y_continuous(labels = scales::dollar) +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Year",
    y = "Box office Sales",
    title = "Box Office Sales Trend by Year (2000-2018)",
    caption = "Data source: IMDb 2018"
  ) 


yearlySales.mod = lm(Sales ~ Year, data=yearlySales)

summary(yearlySales.mod)

# Call:
#   lm(formula = Sales ~ Year, data = yearlySales)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -651187771 -463647707  -29284973  383330460 1032325625 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.591e+11  4.672e+10   5.546 4.43e-05 ***
#   Year        -1.226e+08  2.326e+07  -5.273 7.58e-05 ***
#               -122,600,000        
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.12e+08 on 16 degrees of freedom
# Multiple R-squared:  0.6347,	Adjusted R-squared:  0.6119 
# F-statistic:  27.8 on 1 and 16 DF,  p-value: 7.584e-05


predict(yearlySales.mod, data.frame(Year=2011), interval="prediction")




#movies.diff = diff(movies.ts, lag=0.5)
#movies.diff1 = diff(movies.ts, lag=12)
#ts.plot(movies.diff)
# movies.diff %>%
#   ur.kpss() %>%
#   summary()
#Value of test-statistic is: 0.0274 

# acf(movies.diff, lag.max=48)
# 
# acf(movies.diff1, lag.max=48)

#------try fourier transformation
#movies.fft = fft(movies.diff)

#acf(movies.fft)

#------fit arima AR model
# arima(movies.diff, order=c(1,0,0))
# 
# #-----fit arima MA model
# arima(movies.diff, order=c(0,0,1))


acf(movies.ts, lag.max=48)


#run KPSS test to see if data is stationary
#install.packages("urca")
#library(urca)

movies.ts %>%
  ur.kpss() %>%
  summary()
#Value of test-statistic is: 3.5465 (reject null therefore not stationary)



#run ljung-box test
Box.test(movies.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value < 2.2e-16 (suggests this is data is not white noise.)


#---------------------MOVIES ADJUSTED---------------------------
movies.tsadj = ts(movies$SalesAdj, start=1982, frequency=12)

autoplot(movies.tsadj)

#movies.diffadj = diff(movies.tsadj, s=0.5)

#ts.plot(movies.diffadj)

#acf(movies.diffadj)

#movies.diffadj %>%
#  ur.kpss() %>%
#  summary()


#movies.fft = fft(movies.diffadj)
#acf(movies.fft)

#------fit arima AR model
#arima(movies.diffadj, order=c(1,0,0)) #aic = 14951

#-----fit arima MA model
#arima(movies.diffadj, order=c(0,0,1)) #aic = 14734


#------try tbats
movies.tbats = tbats(movies.tsadj)

movies.tbats$seasonal.periods

#test = forecast(movies.tsadj)
#summary(test)

#--------fit ETS model

movies.ets = ets(movies.tsadj)
summary(movies.ets)

movies.ets %>%
  forecast() %>%
  autoplot()

checkresiduals(movies.ets)

#------------------------------------------------------------



#------------train / test split
mtrain = window(movies.ts, end=c(2017, 12))
mtest = window(movies.ts, start=c(2018,1))

#-----------TRAINING DATA

mtrain.tbats = tbats(mtrain)
mtrain.tbats$seasonal.periods

#check model via forecast
mtrain.forecast = forecast(mtrain, h=11)
summary(mtrain.forecast)
#ETS(M,Ad,M) 

plot(decompose(mtrain, type="multiplicative"))
plot(decompose(mtrain, type="additive"))

####################################
#seasonal naive model benchmark
####################################
mtrain.snaive = snaive(mtrain, h=11)
checkresiduals(mtrain.snaive)
#p-value < 2.2e-16

mtrain.snaive.forecast = forecast(mtrain.snaive, h=11)
autoplot(mtrain.snaive.forecast)

accuracy(mtrain.snaive.forecast, movies.ts)
#                     ME    RMSE     MAE       MPE     MAPE     MASE       ACF1 Theil's U
# Training set  59504.31 3035342 2229979 -4.145280 26.03411 1.000000 -0.2346945        NA
# Test set     205741.78 3614098 2736626 -5.555291 33.94646 1.227198 -0.7272638 0.6143716

####################################
#holt winters
####################################
mtrain.hw = hw(mtrain, seasonal="multiplicative")
checkresiduals(mtrain.hw)
#p-value = 1.409e-06

mtrain.hw.forecast = forecast(mtrain.hw, h=11)
autoplot(mtrain.hw.forecast)

accuracy(mtrain.hw.forecast, movies.ts)
#                     ME    RMSE     MAE        MPE     MAPE      MASE       ACF1 Theil's U
# Training set -174307.3 2401895 1781478  -7.795988 22.08924 0.7988765 -0.1400893        NA
# Test set     -376747.8 3007318 2597870 -12.269922 30.90332 1.1649750 -0.6979878 0.5512812


####################################
#ETS model
####################################
BoxCox.lambda(mtrain)
#lambda: 0.7723345

mtrain.ets = ets(mtrain, lambda=0.77)
summary(mtrain.ets)
#ETS(A,Ad,A) 

checkresiduals(mtrain.ets)
#p-value = 1.296e-06

mtrain.ets.forecast = forecast(mtrain.ets, h=11)
autoplot(mtrain.ets.forecast)

accuracy(mtrain.ets.forecast, movies.ts)
#                      ME    RMSE     MAE       MPE     MAPE      MASE       ACF1 Theil's U
# Training set   89037.23 2301575 1648184 -4.641795 20.21898 0.7391028 -0.1492724        NA
# Test set     -140434.07 3052252 2418299 -6.794190 26.20513 1.0844491 -0.3823885 0.6090198

####################################
mtrain.ets2 = ets(mtrain)
summary(mtrain.ets2)
#ETS(M,Ad,M) 

checkresiduals(mtrain.ets2)
#p-value = 3.544e-08

mtrain.ets2.forecast = forecast(mtrain.ets2, h=11)
autoplot(mtrain.ets2.forecast)

accuracy(mtrain.ets2.forecast, movies.ts)
#                      ME    RMSE     MAE       MPE     MAPE      MASE        ACF1 Theil's U
# Training set   15951.69 2420758 1730113 -7.141452 21.93761 0.7758426 -0.09900464        NA
# Test set     -337682.74 3150617 2608235 -9.325550 28.76252 1.1696232 -0.35531581 0.6250818

####################################
# ARIMA models
####################################

mtrain.ARIMA = auto.arima(mtrain, lambda = 0.77)
summary(mtrain.ARIMA)
#ARIMA(2,0,2)(1,1,2)[12] with drift 

checkresiduals(mtrain.ARIMA)
#p-value = 4.346e-05

mtrain.ARIMA.forecast = forecast(mtrain.ARIMA, h=11)
autoplot(mtrain.ARIMA.forecast)

accuracy(mtrain.ARIMA.forecast, movies.ts)
#                       ME    RMSE     MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set    4730.502 2386697 1726311  -4.270845 20.20819 0.7741377 -0.04838043        NA
# Test set     -562900.289 3037126 2611596 -14.915814 32.25299 1.1711302 -0.74107030 0.5452672


mtrain.ARIMA2 = auto.arima(mtrain, lambda=0.77, stepwise = FALSE)
summary(mtrain.ARIMA2)
#ARIMA(2,0,2)(0,1,1)[12] with drift 

checkresiduals(mtrain.ARIMA2)
#p-value = 4.446e-05

mtrain.ARIMA2.forecast = forecast(mtrain.ARIMA2, h=11)
autoplot(mtrain.ARIMA2.forecast)

accuracy(mtrain.ARIMA2.forecast, movies.ts)
#                       ME    RMSE     MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set    6884.292 2392630 1724565  -4.245371 20.12314 0.7733549 -0.04817714        NA
# Test set     -558632.673 3046540 2575359 -14.784412 31.90639 1.1548806 -0.74839349 0.5427937


####################################
# Dynamic harmoic regression
####################################
Kval = 5 #must not be greater than half h

mtrain.fourier = auto.arima(mtrain, xreg=fourier(mtrain, K=Kval), seasonal = FALSE, lambda=0.77)
summary(mtrain.fourier)
#Regression with ARIMA(2,1,2) errors 

checkresiduals(mtrain.fourier)
#p-value < 2.2e-16

mtrain.fourier.forecast = forecast(mtrain.fourier, xreg=fourier(mtrain, K=Kval, h=11))
autoplot(mtrain.fourier.forecast)

accuracy(mtrain.fourier.forecast, movies.ts)
#KVal = 1
#                     ME    RMSE     MAE       MPE     MAPE     MASE        ACF1 Theil's U
# Training set  312511.2 3643997 2822092 -16.46314 40.86920 1.265524 -0.02773624        NA
# Test set     -113553.2 3919927 3421153 -21.16029 49.89222 1.534164 -0.14987212 0.8787288

#KVal = 4
# ME    RMSE     MAE       MPE     MAPE      MASE        ACF1 Theil's U
# Training set   59267.47 2455672 1811354 -7.533587 23.62782 0.8122743 -0.02248459        NA
# Test set     -140325.90 2846139 2409475 -8.745846 28.23378 1.0804922 -0.33095420 0.6573808

#KVal = 5
#                     ME    RMSE     MAE       MPE     MAPE     MASE       ACF1 Theil's U
# Training set 133124.43 2272232 1612038 -4.951010 20.32619 0.722894 -0.0361563        NA
# Test set      84063.98 3158286 2531576 -5.564687 27.57910 1.135247 -0.4935327  0.650723


####################################
# TBATS
####################################
mtrain.tbats = tbats(mtrain)
summary(mtrain.tbats)


checkresiduals(mtrain.tbats)
#p-value = 5.973e-05

mtrain.tbats.forecast = forecast(mtrain.tbats, h=11)
autoplot(mtrain.tbats.forecast)

accuracy(mtrain.tbats.forecast, movies.ts)
#                    ME    RMSE     MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set 119277.87 2274746 1619841 -4.425945 20.02277 0.7263931 -0.08338809        NA
#Test set     -67795.75 3234267 2616938 -7.069047 28.59743 1.1735257 -0.48550479 0.6482336



#compare with simple models
# movies.train.avg = meanf(movies.train, h=50)
# movies.train.naive = rwf(movies.train, h=50)
# movies.train.drift = rwf(movies.train, drift=TRUE, h=50)

result = rbind(accuracy(mtrain.snaive.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.hw.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.ets.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.ets2.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.ARIMA.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.ARIMA2.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.fourier.forecast, movies.ts)[2, c(2,3,5,6)],
               accuracy(mtrain.tbats.forecast, movies.ts)[2, c(2,3,5,6)]
               )

rownames(result) = c("Seasonal Naive", "Holt Winters", "ETS with Box Cox", "ETS", "ARIMA", "ARIMA Deep", "Fourier", "TBATS")
result

write.csv(result, "AccuracyResults.csv")


#plot over test data
autoplot(movies.train) +
  autolayer(mtrain.ets.forecast, PI=TRUE, series="ETS with Box Cox") +
  autolayer(movies.test, PI=FALSE, series="Actual") +
  autolayer(mtrain.snaive.forecast, PI=FALSE, series="Seasonal Naive") +
  #autolayer(mtrain.hw.forecast, PI=FALSE, series="Holt Winters") +
  #autolayer(mtrain.fourier.forecast, PI=FALSE, series="ARIMA Fourier") +
  #autolayer(mtrain.tbats.forecast, PI=FALSE, series="TBATS") +
  geom_line(size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales (\'00)",
    title = "Forecast - Box Office Sales"
  ) 


write.csv(mtrain.ets.forecast, "forecast.csv")




#run cross validation
forcastfn = function(y, h) {
  forecast(ets(y, lambda = 0.77), h=h)
  }

e = tsCV(movies.ts, forcastfn, h=1)
sqrt(mean(e^2, na.rm=TRUE)) #RMSE (CV)
#3042096





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
    Budget,
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

#------------------------------BUDGET (NOTE can't realy look at budget because it's increasing each month)
# monthBudget = moviesComb %>% 
#   filter(!is.na(Budget)) %>%
#   filter(RecordDate >= "2014-06-01") %>% 
#   group_by(RecordDate) %>%
#   summarise(AverageSales = mean(Sales),
#             AverageBudget = mean(Budget, na.rm=TRUE))%>% 
#   complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
#   mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
#   mutate(AverageSalesAdj = AverageSales/100,
#          AverageBudgetAdj = AverageBudget/100)
# 
# 
# budget.ts = ts(monthBudget$AverageBudget, start=c(2014,6), frequency=12)
# 
# autoplot(budget.ts)
# 
# ggseasonplot(budget.ts) + 
#   scale_y_log10(labels = scales::dollar)
# 
# ggseasonplot(budget.ts, polar=TRUE)


#------------------------------ACTION
action = moviesComb %>%
  filter(G_Action == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales))%>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100) 


action.ts = ts(action$AverageSalesAdj, start=c(2014,6), frequency=12)

autoplot(action.ts) +
  geom_line(colour="#0FC3C7", size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Action Films - Box Office Sales (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 



ggseasonplot(action.ts) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Action Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 


ggseasonplot(action.ts, polar=TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Action Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 

atrain = window(action.ts, end=c(2017,12))
atest = window(action.ts, start=c(2018,1))

#check seasonal
atrain.tbats = tbats(atrain)
atrain.tbats$seasonal.periods

ggAcf(atrain)
#every 6 months

#check model via forecast
atrain.forecast = forecast(atrain, h=9)
summary(atrain.forecast)
#ETS(M,N,M) 

plot(decompose(atrain, type="multiplicative"))
plot(decompose(atrain, type="additive"))


# action.ts %>%
#   ur.kpss() %>%
#   summary()
# #Value of test-statistic is: 0.1288 (stationary)

#run ljung-box test
# Box.test(action.ts, lag=48, fitdf=0, type="Ljung-Box")
#p-value = 0.2106 - not significant therefore stationary


####################################
#seasonal naive model benchmark
####################################
atrain.snaive = snaive(atrain, h=11)
checkresiduals(atrain.snaive)
#p-value = 0.01019

atrain.snaive.forecast = forecast(atrain.snaive, h=11)
autoplot(atrain.snaive.forecast)

accuracy(atrain.snaive.forecast, action.ts)
#                     ME      RMSE      MAE       MPE     MAPE     MASE       ACF1 Theil's U
# Training set -16036.71  554621.5 421628.2 -53.51309 100.3965 1.000000  0.1873508        NA
# Test set     410964.99 1055687.8 551729.1  23.62924  39.9780 1.308568 -0.2672339 0.9230756

####################################
#holt winters
####################################
atrain.hw = hw(atrain, seasonal="multiplicative")
checkresiduals(atrain.hw)
#p-value = 3.739e-12

atrain.hw.forecast = forecast(atrain.hw, h=11)
autoplot(atrain.hw.forecast)

accuracy(atrain.hw.forecast, action.ts)
#                    ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set -36655.19  290291.7 221393.4 -31.86942 52.50936 0.5250915  0.1997262        NA
#Test set     650129.72 1147419.3 660797.8  43.07260 46.28198 1.5672524 -0.3335457 0.9899353


####################################
#ETS model
####################################
BoxCox.lambda(atrain)
#lambda: -0.9999242

atrain.ets = ets(atrain, lambda=-0.99)
summary(atrain.ets)
#ETS(A,N,N) 

checkresiduals(atrain.ets)
#p-value = 0.7102

atrain.ets.forecast = forecast(atrain.ets, h=11)
autoplot(atrain.ets.forecast)

accuracy(atrain.ets.forecast, action.ts)
#                 ME      RMSE      MAE        MPE     MAPE     MASE       ACF1 Theil's U
#Training set 352939  606334.8 436583.9 -0.4682782 76.66413 1.035471  0.2162712        NA
#Test set     771485 1208189.2 791263.7 46.9370305 55.47998 1.876686 -0.2280923 0.9945454

####################################
atrain.ets2 = ets(atrain)
summary(atrain.ets2)
#ETS(M,N,M) 

checkresiduals(atrain.ets2)
#p-value < 2.2e-16

atrain.ets2.forecast = forecast(atrain.ets2, h=11)
autoplot(atrain.ets2.forecast)

accuracy(atrain.ets2.forecast, action.ts)
#                    ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set -34856.51  289515.4 215293.2 -34.99033 54.65808 0.5106233  0.1851594        NA
#Test set     480842.79 1082800.7 519794.1  21.93126 33.64938 1.2328258 -0.2963982 0.9488574

####################################
# ARIMA models
####################################

atrain.ARIMA = auto.arima(atrain, stepwise=FALSE)
summary(atrain.ARIMA)
#ARIMA(0,0,0)(1,1,0)[12] with drift

checkresiduals(atrain.ARIMA)
#p-value = 0.1697

atrain.ARIMA.forecast = forecast(atrain.ARIMA, h=11)
autoplot(atrain.ARIMA.forecast)

accuracy(atrain.ARIMA.forecast, action.ts)
#                    ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set  21318.23  233669.7 165178.3 -3.151477 39.78448 0.3917629  0.1573872        NA
#Test set     624103.85 1235299.6 699317.6 31.841859 54.46918 1.6586121 -0.3721868  1.040127

####################################
# Dynamic harmoic regression
####################################
Kval = 5 #must not be greater than half h

atrain.fourier = auto.arima(atrain, xreg=fourier(atrain, K=Kval), seasonal = FALSE)
summary(atrain.fourier)
#Regression with ARIMA(0,0,2) errors 

checkresiduals(atrain.fourier)
#p-value = 0.002455

atrain.fourier.forecast = forecast(atrain.fourier, xreg=fourier(atrain, K=Kval, h=11))
autoplot(atrain.fourier.forecast)

accuracy(atrain.fourier.forecast, action.ts)
#KVal = 1
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set   4191.657 411458.8 320918.2 -65.59795 96.05960 0.7611402  0.08181625        NA
#Test set     385256.589 961354.8 605643.0 -10.11733 61.33014 1.4364387 -0.30956649 0.8108147

#KVal = 4
#                        ME      RMSE      MAE        MPE     MAPE      MASE       ACF1 Theil's U
#Training set -9.950209e-11  287729.7 212727.4 -28.818827 52.13373 0.5045378  0.1623970        NA
#Test set      4.211149e+05 1095202.9 539328.7   6.910687 36.72000 1.2791571 -0.3240674 0.9618697

#KVal = 5
#                        ME    RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set -4.619370e-07  279604 207111.4 -24.79747 48.41671 0.4912181  0.2235713        NA
#Test set      4.310412e+05 1082402 508816.0  15.18998 31.89713 1.2067884 -0.3110706 0.9498678

####################################
# TBATS
####################################
atrain.tbats = tbats(atrain)
summary(atrain.tbats)


checkresiduals(atrain.tbats)
#p-value = 1.532e-14

atrain.tbats.forecast = forecast(atrain.tbats, h=11)
autoplot(atrain.tbats.forecast)

accuracy(atrain.tbats.forecast, action.ts)
#                     ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set   6726.194  267616.7 203304.5 -20.35439 45.38913 0.4821891  0.1921441        NA
#Test set     484211.306 1093232.7 514656.9  23.16543 31.78166 1.2206416 -0.3060543 0.9587776


####################################
# DYNAMIC REGRESSION
####################################

actionDR = moviesComb %>%
  filter(G_Action == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales),
            AverageTheatres = mean(Theatres))%>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100) 


actionDR.ts = ts(actionDR, start=c(2014,6), frequency=12)
autoplot(actionDR.ts, facets=TRUE)

ggseasonplot(actionDR.ts[,"AverageSales"]) + 
  scale_y_log10(labels = scales::dollar)


ggseasonplot(actionDR.ts[,"AverageSales"], polar=TRUE)


atrainDR = window(actionDR.ts, end=c(2017,12))
atestDR = window(actionDR.ts, start=c(2018,1))

tbreaks = c(0, 10, 100, 1000, 10000, 100000)
salesBreaks2 = c(0, 1000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

actionDR %>%
  ggplot(aes(y=AverageSales, x=AverageTheatres)) +
    geom_point() + 
    geom_smooth(method="lm") +
    scale_y_log10(labels = scales::dollar, breaks=salesBreaks2) +
    scale_x_log10(labels = scales::comma, breaks=tbreaks)

actionDR %>%
  summarise(corr = cor(AverageSales, AverageTheatres, use="complete.obs"))
#0.512


atrainDR.ARIMA = auto.arima(atrainDR[,"AverageSalesAdj"], xreg=atrainDR[,"AverageTheatres"])
summary(atrainDR.ARIMA)
#Regression with ARIMA(0,0,0) errors  

checkresiduals(atrainDR.ARIMA)
#p-value = 0.0001832

mean(atrainDR[,"AverageTheatres"])

#aTheatresTrain.tbats.forecast$mean
#average: rep(1695.256, 9)

#----------------predict threatres for xreg
actionTheatres.ts = ts(actionDR$AverageTheatres, start=c(2014,6), frequency=12)

actionTheatres.forecast = forecast(actionTheatres.ts)
summary(actionTheatres.forecast)

BoxCox.lambda(actionTheatres.ts)

aTheatresTrain = window(actionTheatres.ts, end=c(2017,12))
aTheatresTest = window(actionTheatres.ts, start=c(2018,1))

aTheatresTrain.ets = ets(aTheatresTrain)
summary(aTheatresTrain.ets)
#ETS(A,N,N)

checkresiduals(aTheatresTrain.ets)
#p-value = 0.06983

aTheatresTrain.ets.forecast = forecast(aTheatresTrain.ets, h=11)
autoplot(aTheatresTrain.ets.forecast)

accuracy(aTheatresTrain.ets.forecast, actionTheatres.ts)
#                      ME     RMSE      MAE       MPE     MAPE      MASE      ACF1 Theil's U
#Training set  -0.1498237 617.7974 495.0479 -14.00209 33.54440 0.8315496 0.2561144        NA
#Test set     348.9585368 668.8544 506.1660  10.18338 23.04865 0.8502251 0.1761885 0.8497397


aTheatresTrain.tbats = tbats(aTheatresTrain)
summary(aTheatresTrain.tbats)

checkresiduals(aTheatresTrain.tbats)
#p-value = 2.875e-07

aTheatresTrain.tbats.forecast = forecast(aTheatresTrain.tbats, h=11)
autoplot(aTheatresTrain.tbats.forecast)

accuracy(aTheatresTrain.tbats.forecast, actionTheatres.ts)
#                    ME     RMSE      MAE       MPE     MAPE      MASE         ACF1 Theil's U
#Training set  58.94234 389.0946 303.5297 -1.659450 18.73477 0.5098496  0.007540158        NA
#Test set     320.92869 802.4871 669.7459  8.348868 34.72212 1.1249961 -0.205610059 0.9680727

aThreatresPointForecast = aTheatresTrain.tbats.forecast$mean
#---------------------------------------------------------------


atrainDR.ARIMA.forecast = forecast(atrainDR.ARIMA, xreg=aTheatresTrain.tbats.forecast$mean)
autoplot(atrainDR.ARIMA.forecast)

accuracy(atrainDR.ARIMA.forecast, action.ts)
#                    ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set -16818.38  398731.6 301054.4 -72.52644 95.22877 0.7140281  0.1959714        NA
#Test set     390372.64 1045144.7 560175.3  -5.98785 48.91580 1.3286002 -0.2795301 0.8966207





#---------

#compare with simple models

aresult = rbind(accuracy(atrain.snaive.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.hw.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.ets.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.ets2.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.ARIMA.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.fourier.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrain.tbats.forecast, action.ts)[2, c(2,3,5,6)],
               accuracy(atrainDR.ARIMA.forecast, action.ts)[2, c(2,3,5,6)]
)

rownames(aresult) = c("Seasonal Naive", "Holt Winters", "ETS with Box Cox", "ETS", "ARIMA", "Fourier", "TBATS", "Dynamic Regression")
aresult


write.csv(aresult, "AccuracyResults.csv")


#plot pridictions
autoplot(atrain) + 
  autolayer(atrain.tbats.forecast, PI=TRUE, series="TBATS") +
  autolayer(atest, series="Actual") +
  autolayer(atrain.snaive.forecast, PI=FALSE, series="Naive") +
  #autolayer(atrain.ets2.forecast, PI=FALSE, series="ETS") +
  autolayer(atrain.fourier.forecast, PI=FALSE, series="Fourier") +
  #autolayer(atrainDR.ARIMA.forecast, PI=FALSE, series="Dynamic Regression") +
  geom_line(size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales (\'00)",
    title = "Forecast - Box Office Sales for Action Films"
  ) 
  
  
write.csv(atrain.tbats.forecast, "forecast.csv")


#------------------------------FAMILY
family = moviesComb %>%
  filter(G_Family == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales)) %>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100)



family.ts = ts(family$AverageSalesAdj, start=c(2014,6), frequency=12)

autoplot(family.ts) + 
  geom_line(colour="#0FC3C7", size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Family Films - Box Office Sales (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 


ggseasonplot(family.ts) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Family Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 

ggseasonplot(family.ts, polar=TRUE) + 
scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Family Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 


ftrain = window(family.ts, end=c(2017,12))
ftest = window(family.ts, start=c(2018,1))

#check seasonal
ftrain.tbats = tbats(ftrain)
ftrain.tbats$seasonal.periods

ggAcf(ftrain)
#no seasonality

#check model via forecast
ftrain.forecast = forecast(ftrain, h=9)
summary(ftrain.forecast)
#ETS(A,N,N)

plot(decompose(ftrain, type="multiplicative"))
plot(decompose(ftrain, type="additive"))



####################################
#seasonal naive model benchmark
####################################
ftrain.snaive = snaive(ftrain, h=11)
checkresiduals(ftrain.snaive)
#p-value = 0.5604

ftrain.snaive.forecast = forecast(ftrain.snaive, h=11)
autoplot(ftrain.snaive.forecast)

accuracy(ftrain.snaive.forecast, family.ts)
#                    ME      RMSE      MAE    MPE     MAPE      MASE        ACF1 Theil's U
#Training set -13619.94 1013043.5 559336.7   -Inf      Inf 1.0000000 0.415685207        NA
#Test set      42654.79  771783.3 433750.4 -234.1 311.6036 0.7754728 0.004719108  1.022455

####################################
#holt winters
####################################
ftrain.hw = hw(ftrain, seasonal="additive")
checkresiduals(ftrain.hw)
#p-value = 7.308e-06

ftrain.hw.forecast = forecast(ftrain.hw, h=11)
autoplot(ftrain.hw.forecast)

accuracy(ftrain.hw.forecast, family.ts)
#                    ME     RMSE      MAE     MPE     MAPE      MASE       ACF1 Theil's U
#Training set -73073.05 569911.6 355883.9     NaN      Inf 0.6362607 0.32154915        NA
#Test set     131668.01 587310.7 310238.1 226.702 968.7797 0.5546536 0.01400046 0.9061463


####################################
#ETS model
####################################
BoxCox.lambda(ftrain)
#lambda: 0.155163

ftrain.ets = ets(ftrain, lambda=0.155)
summary(ftrain.ets)
#ETS(A,N,N) 

checkresiduals(ftrain.ets)
#p-value = 0.6805

ftrain.ets.forecast = forecast(ftrain.ets, h=11)
autoplot(ftrain.ets.forecast)

accuracy(ftrain.ets.forecast, family.ts)
#                   ME     RMSE      MAE       MPE    MAPE      MASE       ACF1 Theil's U
#Training set 291930.2 688665.4 306067.4      -Inf     Inf 0.5471971  0.2539748        NA
#Test set     320909.6 657568.5 330383.4 -620.5099 702.199 0.5906700 -0.1447216  0.990426

####################################
ftrain.ets2 = ets(ftrain)
summary(ftrain.ets2)
#ETS(M,N,M) 

checkresiduals(ftrain.ets2)
#p-value = 0.3016

ftrain.ets2.forecast = forecast(ftrain.ets2, h=11)
autoplot(ftrain.ets2.forecast)

accuracy(ftrain.ets2.forecast, family.ts)
#                   ME     RMSE      MAE       MPE    MAPE      MASE       ACF1 Theil's U
#Training set 48907.10 625667.0 368889.3      -Inf     Inf 0.6595121  0.2539619        NA
#Test set     77703.38 579181.5 421282.7 -11686.55 11727.2 0.7531827 -0.1447216 0.8394459

####################################
# ARIMA models
####################################

ftrain.ARIMA = auto.arima(ftrain, stepwise=FALSE)
summary(ftrain.ARIMA)
#ARIMA(0,0,1) with non-zero mean 

checkresiduals(ftrain.ARIMA)
#p-value = 0.4007

ftrain.ARIMA.forecast = forecast(ftrain.ARIMA, h=11)
autoplot(ftrain.ARIMA.forecast)

accuracy(ftrain.ARIMA.forecast, family.ts)
#                   ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  3135.30 582826.9 398424.0      -Inf      Inf 0.7123152 -0.06999196        NA
#Test set     64572.38 566097.9 411504.2 -13297.04 13335.14 0.7357004 -0.16762714 0.8147015

#------with lambda

ftrain.ARIMA2 = auto.arima(ftrain, lambda=0.155, stepwise=FALSE)
summary(ftrain.ARIMA2)
#ARIMA(0,0,0) with non-zero mean 

checkresiduals(ftrain.ARIMA2)
#p-value = 0.7782

ftrain.ARIMA2.forecast = forecast(ftrain.ARIMA2, h=11)
autoplot(ftrain.ARIMA2.forecast)

accuracy(ftrain.ARIMA2.forecast, family.ts)
#                   ME     RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set 291939.2 688667.9 306064.1      -Inf      Inf 0.5471913  0.2539772        NA
#Test set     320911.1 657569.3 330383.6 -620.4413 702.1334 0.5906704 -0.1447216 0.9904269


####################################
# Dynamic harmoic regression
####################################
Kval = 3 #must not be greater than half h

ftrain.fourier = auto.arima(ftrain, xreg=fourier(ftrain, K=Kval), seasonal = FALSE)
summary(ftrain.fourier)
#Regression with ARIMA(0,0,1) errors 

checkresiduals(ftrain.fourier)
#p-value = 0.1431

ftrain.fourier.forecast = forecast(ftrain.fourier, xreg=fourier(ftrain, K=Kval, h=11))
autoplot(ftrain.fourier.forecast)

accuracy(ftrain.fourier.forecast, family.ts)
#KVal = 1
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set   1483.021 568518.9 390246.6      -Inf      Inf 0.6976954 -0.08947932        NA
#Test set     128780.480 639133.5 405285.5 -6953.442 6994.472 0.7245824 -0.03691108 0.9484501

#KVal = 3
#                      ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set    906.4322 550030.2 390504.4      -Inf      Inf 0.6981563 -0.07602971        NA
#Test set     162037.5714 587325.0 338543.3 -2487.071 2525.382 0.6052585 -0.05130645 0.8919307

#KVal = 4
#                     ME     RMSE      MAE       MPE    MAPE      MASE        ACF1 Theil's U
#Training set   2498.059 528091.8 390792.4       NaN     Inf 0.6986713 -0.04685841        NA
#Test set     205426.029 622869.4 343700.7 -6842.894 7382.84 0.6144792 -0.12668029 0.9669847



####################################
# TBATS
####################################
ftrain.tbats = tbats(ftrain)
summary(ftrain.tbats)


checkresiduals(ftrain.tbats)
#p-value = 0.1899

ftrain.tbats.forecast = forecast(ftrain.tbats, h=11)
autoplot(ftrain.tbats.forecast)

accuracy(ftrain.tbats.forecast, family.ts)
#                    ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  43961.89 582328.3 387037.8      -Inf      Inf 0.6919586 -0.07858938        NA
#Test set     143137.24 581177.8 379474.7 -9614.381 9657.663 0.6784370 -0.16528203 0.8650128


#compare with simple models

fresult = rbind(accuracy(ftrain.snaive.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.hw.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.ets.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.ets2.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.ARIMA.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.fourier.forecast, family.ts)[2, c(2,3,5,6)],
                accuracy(ftrain.tbats.forecast, family.ts)[2, c(2,3,5,6)]
)

rownames(fresult) = c("Seasonal Naive", "Holt Winters", "ETS with Box Cox", "ETS", "ARIMA", "Fourier", "TBATS")
fresult

write.csv(fresult, "AccuracyResults.csv")



#plot predictions
autoplot(ftrain) +
  autolayer(ftrain.ets.forecast, PI=FALSE, series="ETS with Box Cox") +
  #autolayer(ftrain.hw.forecast, PI=TRUE, series="Holt-Winters") +
  autolayer(ftrain.snaive.forecast, PI=FALSE, series="Naive") +
  autolayer(ftest, series="Actual") +
  #autolayer(ftrain.ets2.forecast, PI=FALSE, series="ETS") +
  #autolayer(ftrain.ARIMA.forecast, PI=FALSE, series="ARIMA") +
  #autolayer(ftrain.fourier.forecast, PI=FALSE, series="Fourier") +
  #autolayer(ftrain.tbats.forecast, PI=FALSE, series="TBATS") 
  geom_line(size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales (\'00)",
    title = "Forecast - Box Office Sales for Family Movies"
  ) 


write.csv(ftrain.hw.forecast, "forecast.csv")

#-----------------------





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

autoplot(horror.ts) + 
scale_y_continuous(labels = scales::dollar) +
  geom_line(colour="#0FC3C7", size=1) + 
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Horror Films - Box Office Sales (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 




ggseasonplot(horror.ts) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Horror Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 

ggseasonplot(horror.ts, polar=TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Horror Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 


htrain = window(horror.ts, end=c(2017,12))
htest = window(horror.ts, start=c(2018,1))

#check seasonal
htrain.tbats = tbats(htrain)
htrain.tbats$seasonal.periods

ggAcf(htrain)
#na

#check model via forecast
htrain.forecast = forecast(htrain, h=9)
summary(htrain.forecast)
#ETS(M,N,N) 

plot(decompose(htrain, type="multiplicative"))
plot(decompose(htrain, type="additive"))





####################################
#seasonal naive model benchmark
####################################
htrain.snaive = snaive(htrain, h=11)
checkresiduals(htrain.snaive)
#p-value = 0.05804

htrain.snaive.forecast = forecast(htrain.snaive, h=11)
autoplot(htrain.snaive.forecast)

accuracy(htrain.snaive.forecast, horror.ts)
#                  ME     RMSE      MAE       MPE     MAPE     MASE       ACF1 Theil's U
#Training set 4500371 27688432 20777359 -384.4854 457.2411 1.000000 -0.2550384        NA
#Test set     4773433 41795344 34580408 -279.6707 378.3337 1.664331 -0.1856061  0.997647

####################################
#holt winters
####################################
htrain.hw = hw(htrain, seasonal="multiplicative")
checkresiduals(htrain.hw)
#p-value = 1.288e-10

htrain.hw.forecast = forecast(htrain.hw, h=11)
autoplot(htrain.hw.forecast)

accuracy(htrain.hw.forecast, horror.ts)
#                   ME     RMSE      MAE        MPE      MAPE      MASE       ACF1 Theil's U
#Training set -1287457 17474842 14242890 -4813.3774 4843.9621 0.6855005 -0.2625973        NA
#Test set      1635458 23792993 19232023  -141.8529  176.0294 0.9256241 -0.1932762 0.4632862


####################################
#ETS model
####################################
BoxCox.lambda(htrain)
#lambda: -0.9355101

htrain.ets = ets(htrain, lambda=-0.93)
summary(htrain.ets)
#ETS(A,N,N) 

checkresiduals(htrain.ets)
#p-value = 1

htrain.ets.forecast = forecast(htrain.ets, h=11)
autoplot(htrain.ets.forecast)

accuracy(htrain.ets.forecast, horror.ts)
#                   ME     RMSE      MAE       MPE      MAPE      MASE        ACF1 Theil's U
#Training set 18958327 27152231 18997352 -21.43286 191.81691 0.9143295 -0.12011497        NA
#Test set     30949997 41873470 30949997  97.13894  97.13894 1.4896021 -0.03181549  1.245121

####################################
htrain.ets2 = ets(htrain, model="MNM")
summary(htrain.ets2)
#ETS(M,N,M) 

checkresiduals(htrain.ets2)
#p-value = 0.1179

htrain.ets2.forecast = forecast(htrain.ets2, h=11)
autoplot(htrain.ets2.forecast)

accuracy(htrain.ets2.forecast, horror.ts)
#                   ME     RMSE      MAE         MPE      MAPE      MASE       ACF1 Theil's U
#Training set  2311467 18104373 14493619 -5064.19088 5108.9908 0.6975679 -0.2128942        NA
#Test set     11331007 30077362 22612693   -87.21381  147.1287 1.0883334 -0.1380504 0.6471787

####################################
# ARIMA models
####################################

htrain.ARIMA = auto.arima(htrain, stepwise=FALSE)
summary(htrain.ARIMA)
#ARIMA(2,0,0) with non-zero mean

checkresiduals(htrain.ARIMA)
#p-value = 0.6885

htrain.ARIMA.forecast = forecast(htrain.ARIMA, h=11)
autoplot(htrain.ARIMA.forecast)

accuracy(htrain.ARIMA.forecast, horror.ts)
#                      ME     RMSE      MAE         MPE      MAPE      MASE         ACF1 Theil's U
#Training set   -48928.53 17654386 14033510 -6926.94757 6975.9745 0.6754232 -0.006661413        NA
#Test set     10189486.50 28853348 21020528   -78.70276  129.6092 1.0117036 -0.042898774 0.5443863


####################################
# Dynamic harmoic regression
####################################
Kval = 4 #must not be greater than half h

htrain.fourier = auto.arima(htrain, xreg=fourier(htrain, K=Kval), seasonal = FALSE)
summary(htrain.fourier)
#Regression with ARIMA(0,0,2) errors 

checkresiduals(htrain.fourier)
#p-value = 0.002455

htrain.fourier.forecast = forecast(htrain.fourier, xreg=fourier(htrain, K=Kval, h=11))
autoplot(htrain.fourier.forecast)

accuracy(htrain.fourier.forecast, horror.ts)
#KVal = 1
#                       ME     RMSE      MAE         MPE     MAPE      MASE        ACF1 Theil's U
#Training set 1.603672e-09 19370085 15896157 -7361.58685 7396.960 0.7650711 -0.12884910        NA
#Test set     1.096655e+07 30954816 22154663   -82.43512  133.647 1.0662887 -0.01978583 0.5466492

#KVal = 4
#                    ME     RMSE      MAE         MPE     MAPE      MASE        ACF1 Theil's U
#Training set -141807.3 16960753 13227508 -5377.00120 5433.506 0.6366309  0.04344608        NA
#Test set     8191615.1 26782090 19565303   -86.48636  132.025 0.9416646 -0.07389573 0.5204942

#KVal = 5
#                        ME     RMSE      MAE         MPE      MAPE      MASE       ACF1 Theil's U
#Training set -9.627974e-09 17597936 13640485 -5547.04699 5583.3732 0.6565072 -0.1917319        NA
#Test set      9.006571e+06 28152686 21479148   -95.76987  148.5584 1.0337766 -0.1335920 0.6028824

####################################
# TBATS
####################################
htrain.tbats = tbats(htrain)
summary(htrain.tbats)


checkresiduals(htrain.tbats)
#p-value = 0.2788

htrain.tbats.forecast = forecast(htrain.tbats, h=11)
autoplot(htrain.tbats.forecast)

accuracy(htrain.tbats.forecast, horror.ts)
#                   ME     RMSE      MAE         MPE       MAPE      MASE        ACF1 Theil's U
#Training set 10017487 21970745 16131033 -2685.92258 2760.00242 0.7763755 -0.16081054        NA
#Test set     20635056 34946970 23672662     7.02613   77.97988 1.1393490 -0.03181549 0.7784599



####################################
# DYNAMIC REGRESSION
####################################

horrorDR = moviesComb %>%
  filter(G_Horror == 1) %>%
  filter(RecordDate >= "2014-06-01") %>% 
  group_by(RecordDate) %>%
  summarise(AverageSales = mean(Sales),
            AverageTheatres = mean(Theatres))%>% 
  complete(RecordDate = seq(min(RecordDate), max(RecordDate), "1 month")) %>%
  mutate(AverageSales = ifelse(!is.na(AverageSales), AverageSales, 0)) %>%
  mutate(AverageSalesAdj = AverageSales/100) 


horrorDR.ts = ts(horrorDR, start=c(2014,6), frequency=12)
autoplot(horrorDR.ts, facets=TRUE)

ggseasonplot(horrorDR.ts[,"AverageSales"]) + 
  scale_y_log10(labels = scales::dollar)


ggseasonplot(horrorDR.ts[,"AverageSales"], polar=TRUE)


htrainDR = window(horrorDR.ts, end=c(2017,12))
htestDR = window(horrorDR.ts, start=c(2018,1))

tbreaks = c(0, 10, 100, 1000, 10000, 100000)
salesBreaks2 = c(0, 1000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

horrorDR %>%
  ggplot(aes(y=AverageSales, x=AverageTheatres)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks2) +
  scale_x_log10(labels = scales::comma, breaks=tbreaks)

horrorDR %>%
  summarise(corr = cor(AverageSales, AverageTheatres, use="complete.obs"))
#0.512


htrainDR.ARIMA = auto.arima(htrainDR[,"AverageSalesAdj"], xreg=htrainDR[,"AverageTheatres"])
summary(htrainDR.ARIMA)
#Regression with ARIMA(0,1,1)(1,0,0)[12] errors 

checkresiduals(htrainDR.ARIMA)
#p-value = 0.2679

mean(htrainDR[,"AverageTheatres"])
#1196.044

#hTheatresTrain.tbats.forecast$mean
#average: rep(1695.256, 9)

#----------------predict threatres for xreg
horrorTheatres.ts = ts(horrorDR$AverageTheatres, start=c(2014,6), frequency=12)

horrorTheatres.forecast = forecast(horrorTheatres.ts)
summary(horrorTheatres.forecast)

BoxCox.lambda(horrorTheatres.ts)

hTheatresTrain = window(horrorTheatres.ts, end=c(2017,12))
hTheatresTest = window(horrorTheatres.ts, start=c(2018,1))

hTheatresTrain.ets = ets(hTheatresTrain, model="ANN")
summary(hTheatresTrain.ets)
#ETS(A,N,N)

checkresiduals(hTheatresTrain.ets)
#p-value = 0.5226

hTheatresTrain.ets.forecast = forecast(hTheatresTrain.ets, h=11)
autoplot(hTheatresTrain.ets.forecast)

accuracy(hTheatresTrain.ets.forecast, actionTheatres.ts)
#                    ME      RMSE      MAE         MPE       MAPE      MASE        ACF1 Theil's U
#Training set  -1.20862  868.7433 693.1286 -1136.95052 1166.96415 0.8163821 -0.05499582        NA
#Test set     847.18135 1021.4257 854.2433    36.57646   37.18056 1.0061465  0.17618853  1.337232


hTheatresTrain.snaive = snaive(hTheatresTrain)
summary(hTheatresTrain.snaive)


hTheatresTrain.snaive.forecast = forecast(hTheatresTrain.snaive, h=9)
autoplot(hTheatresTrain.snaive.forecast)

accuracy(hTheatresTrain.snaive.forecast, actionTheatres.ts)
#                    ME     RMSE       MAE        MPE      MAPE     MASE        ACF1 Theil's U
#Training set  57.01613 1156.992  849.0247 -254.66791 307.94947 1.000000 -0.02527011        NA
#Test set     690.64990 1627.599 1488.7346   19.26087  72.29448 1.753464 -0.14394787  2.266681


hTheatresTrain.tbats = tbats(hTheatresTrain)
summary(hTheatresTrain.tbats)

checkresiduals(hTheatresTrain.tbats)
#p-value = 0.4628

hTheatresTrain.tbats.forecast = forecast(hTheatresTrain.tbats, h=9)
autoplot(hTheatresTrain.tbats.forecast)

accuracy(hTheatresTrain.tbats.forecast, actionTheatres.ts)
#                    ME      RMSE       MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  237.2039  934.6314  725.3545 -729.9588 776.9227 0.8543385 -0.04818008        NA
#Test set     1216.7225 1343.8779 1216.7225   56.1527  56.1527 1.4330825  0.17618853  1.777365

aThreatresPointForecast = hTheatresTrain.tbats.forecast$mean
#---------------------------------------------------------------


htrainDR.ARIMA.forecast = forecast(htrainDR.ARIMA, xreg=hTheatresTrain.ets.forecast$mean)
autoplot(htrainDR.ARIMA.forecast)

accuracy(htrainDR.ARIMA.forecast, horror.ts)
#                       ME     RMSE         MAE        MPE      MAPE        MASE        ACF1 Theil's U
#Training set     5066.348   102851    78904.85 -433.77653 772.29117   0.3797636  0.12730178        NA
#Test set     31018072.535 41927834 31018072.53   97.77622  97.77622 149.2878534 -0.03166578  1.247453


#compare with simple models

hresult = rbind(accuracy(htrain.snaive.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.hw.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.ets.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.ets2.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.ARIMA.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.fourier.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrain.tbats.forecast, horror.ts)[2, c(2,3,5,6)],
                accuracy(htrainDR.ARIMA.forecast, horror.ts)[2, c(2,3,5,6)]
)

rownames(hresult) = c("Seasonal Naive", "Holt Winters", "ETS with Box Cox", "ETS", "ARIMA", "Fourier", "TBATS", "Dynamic Regression")
hresult

write.csv(hresult, "AccuracyResults.csv")


#plot pridictions
autoplot(htrain) + 
  autolayer(htrain.tbats.forecast, PI=TRUE, series="TBATS") +
  autolayer(htrain.hw.forecast, PI=FALSE, series="Holt-Winters") + 
  autolayer(atest, series="Actual") +
  #autolayer(htrain.snaive.forecast, PI=FALSE, series="Naive") +
  #autolayer(htrain.ets.forecast, PI=FALSE, series="ETS") +
  #autolayer(htrain.ets2.forecast, PI=FALSE, series="ETS") +
  #autolayer(htrain.fourier.forecast, PI=FALSE, series="Fourier") +
  #autolayer(htrainDR.ARIMA.forecast, PI=TRUE, series="Dynamic Regression") +
  geom_line(size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales (\'00)",
    title = "Forecast - Box Office Sales for Horror Films"
  ) 



write.csv(htrain.tbats.forecast, "forecast.csv")








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

autoplot(animation.ts) + 
  geom_line(colour="#0FC3C7", size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Animation Films - Box Office Sales (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 

ggseasonplot(animation.ts) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Animation Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 

ggseasonplot(animation.ts, polar=TRUE) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales",
    title = "Animation Films - Box Office Sales Seasonal Plot (2014-2018)",
    caption = "Data source: IMDb 2018"
  ) 


anitrain = window(animation.ts, end=c(2017,12))
anitest = window(animation.ts, start=c(2018,1))

#check seasonal
anitrain.tbats = tbats(anitrain)
anitrain.tbats$seasonal.periods

ggAcf(anitrain)
#every 6 months

#check model via forecast
anitrain.forecast = forecast(anitrain, h=11)
summary(anitrain.forecast)
#ETS(A,N,N)

plot(decompose(anitrain, type="multiplicative"))
plot(decompose(anitrain, type="additive"))


####################################
#seasonal naive model benchmark
####################################
anitrain.snaive = snaive(anitrain, h=11)
checkresiduals(anitrain.snaive)
#p-value = 0.7012

anitrain.snaive.forecast = forecast(anitrain.snaive, h=11)
autoplot(anitrain.snaive.forecast)

accuracy(anitrain.snaive.forecast, animation.ts)
#                   ME      RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
#Training set -55180.3 1054890.1 720780.5      -Inf      Inf 1.0000000 0.03271392        NA
#Test set     127607.6  722123.4 462577.4 -104.2964 185.3326 0.6417729 0.15370403 0.5754416

####################################
#holt winters
####################################
anitrain.hw = hw(anitrain, seasonal="additive", damped=TRUE)
checkresiduals(anitrain.hw)
#p-value = 3.799e-05

anitrain.hw.forecast = forecast(anitrain.hw, h=11)
autoplot(anitrain.hw.forecast)

accuracy(anitrain.hw.forecast, animation.ts)
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set   40231.67 630195.6 412781.7      -Inf      Inf 0.5726872  0.06956397        NA
#Test set     -307927.19 933891.2 722338.0 -4336.403 4366.973 1.0021609 -0.44314428 0.4798093


####################################
#ETS model
####################################
BoxCox.lambda(anitrain)
#lambda: 4.102259e-05

anitrain.ets = ets(anitrain, model="AAA", damped = TRUE)
summary(anitrain.ets)
#ETS(A,N,N) 

checkresiduals(anitrain.ets)
#p-value = 0.1067

anitrain.ets.forecast = forecast(anitrain.ets, h=11)
autoplot(anitrain.ets.forecast)

accuracy(anitrain.ets.forecast, animation.ts)
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set   40231.67 630195.6 412781.7      -Inf      Inf 0.5726872  0.06956397        NA
#Test set     -307927.19 933891.2 722338.0 -4336.403 4366.973 1.0021609 -0.44314428 0.4798093

####################################
anitrain.ets2 = ets(anitrain)
summary(anitrain.ets2)
#ETS(M,N,M) 

checkresiduals(anitrain.ets2)
#p-value = 0.1067

anitrain.ets2.forecast = forecast(anitrain.ets2, h=11)
autoplot(anitrain.ets2.forecast)

accuracy(anitrain.ets2.forecast, animation.ts)
#                    ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  19194.18 776615.8 578799.2      -Inf      Inf 0.8030173  0.07651346        NA
#Test set     -31824.09 994285.2 704081.8 -9072.518 9094.919 0.9768325 -0.07255009 0.7843992

####################################
# ARIMA models
####################################

anitrain.ARIMA = auto.arima(anitrain, stepwise=FALSE)
summary(anitrain.ARIMA)
#ARIMA(0,0,0) with non-zero mean 

checkresiduals(anitrain.ARIMA)
#p-value = 0.1612

anitrain.ARIMA.forecast = forecast(anitrain.ARIMA, h=11)
autoplot(anitrain.ARIMA.forecast)

accuracy(anitrain.ARIMA.forecast, animation.ts)
#                       ME     RMSE      MAE       MPE    MAPE      MASE        ACF1 Theil's U
#Training set  2.43845e-11 776337.5 583677.0      -Inf     Inf 0.8097847  0.07650922        NA
#Test set     -5.10654e+04 995086.9 717825.6 -9344.802 9367.02 0.9959004 -0.07255009 0.7779923

####################################
# Dynamic harmoic regression
####################################
Kval = 5 #must not be greater than half h

anitrain.fourier = auto.arima(anitrain, xreg=fourier(anitrain, K=Kval), seasonal = FALSE)
summary(anitrain.fourier)
#Regression with ARIMA(0,0,0) errors

checkresiduals(anitrain.fourier)
# p-value = 0.02365

anitrain.fourier.forecast = forecast(anitrain.fourier, xreg=fourier(anitrain, K=Kval, h=11))
autoplot(anitrain.fourier.forecast)

accuracy(anitrain.fourier.forecast, animation.ts)
#KVal = 1
#                        ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  4.480716e-10 750259.3 556371.5      -Inf      Inf 0.7719015  0.02452362        NA
#Test set     -1.931254e+05 982099.4 786881.5 -13397.94 13417.73 1.0917076 -0.11482917 0.6929617

#KVal = 4
#                        ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set -3.864533e-10 596712.1 409193.9      -Inf      Inf 0.5677095  0.04369315        NA
#Test set     -2.055846e+05 759044.2 602140.8 -4588.588 4601.802 0.8354011 -0.18440605 0.4626988

#KVal = 5
#                        ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
#Training set  5.213533e-11 592225.9 394113.7      -Inf      Inf 0.5467874  0.06157492        NA
#Test set     -2.093362e+05 729029.1 590048.0 -3247.846 3260.532 0.8186237 -0.10917866 0.4441905

####################################
# TBATS
####################################
anitrain.tbats = tbats(anitrain)
summary(anitrain.tbats)


checkresiduals(anitrain.tbats)
#p-value = 5.894e-08

anitrain.tbats.forecast = forecast(anitrain.tbats, h=11)
autoplot(anitrain.tbats.forecast)

accuracy(anitrain.tbats.forecast, animation.ts)
#                    ME     RMSE      MAE      MPE     MAPE      MASE       ACF1 Theil's U
#Training set -145209.2 541519.0 408371.0     -Inf      Inf 0.5665678 -0.0510474        NA
#Test set      230000.0 749662.4 523397.2 1846.393 1950.272 0.7261534 -0.1744629 0.5982419


#compare with simple models

aniresult = rbind(accuracy(anitrain.snaive.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.hw.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.ets.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.ets2.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.ARIMA.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.fourier.forecast, animation.ts)[2, c(2,3,5,6)],
                  accuracy(anitrain.tbats.forecast, animation.ts)[2, c(2,3,5,6)]
)

rownames(aniresult) = c("Seasonal Naive", "Holt Winters", "ETS with Damping", "ETS", "ARIMA", "Fourier", "TBATS")
aniresult

write.csv(aniresult, "AccuracyResults.csv")



#plot pridictions
autoplot(anitrain) + 
  autolayer(anitrain.snaive.forecast, PI=TRUE, series="Naive") +
  autolayer(anitrain.fourier.forecast, PI=FALSE, series="Fourier") +
  autolayer(atest, series="Actual") +
  #autolayer(anitrain.hw.forecast, PI=FALSE, series="Holt-Winters") +
  #autolayer(anitrain.ets.forecast, PI=FALSE, series="ETS") +
  #autolayer(anitrain.ets2.forecast, PI=FALSE, series="ETS") +
  #autolayer(anitrain.tbats.forecast, PI=FALSE, series="TBATS") +
  #autolayer(anitrainDR.ARIMA.forecast, PI=TRUE, series="Dynamic Regression") +
  geom_line(size=1) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size=12),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Date",
    y = "Box office Sales (\'00)",
    title = "Forecast - Box Office Sales for Animation films"
  ) 


write.csv(anitrain.snaive.forecast, "forecast.csv")

#---------------------------------------------------




