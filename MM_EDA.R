#-----------------------------------
# Assignment 3 - Mixed Model EDA
#
# Archel Aguilar (98081767)
#-----------------------------------

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

#------------------------------------------------------------------------------
#                   START EDA
#------------------------------------------------------------------------------

mclean = read.csv("MovieClean2.csv", stringsAsFactors = FALSE)


mclean2 = mclean %>%
  mutate(Wide_Release = totTheatreCount >= 2000) %>%
  rename(
    ID = X.1,
    Theatres = totTheatreCount, 
    Sales = totGross, 
    Budget = totBudget, 
    Weeks_Showing = WeeksOn, 
    Year = startYear, 
    Opening_Week = startWeek,
    IMDB_Rating = IMDBRating,
    RottenTomatoes_Rating = RTRating,
    Metacritic_Rating = Metacritic
  ) 

View(mclean2)