#-----------------------------------------------------
# Statistical Thinking - Assignment 3
# Archel Aguilar - 98081767
# Web scrapting of movie sales - from https://www.boxofficemojo.com/monthly/?view=releasedate&yr=2018&month=9&adjust_yr=2018&p=.htm
#-----------------------------------------------------


#install.packages("httr")
library(httr)

#install.packages("rvest")
library(rvest)

#install.packages("dplyr")
library(dplyr)

#install.packages("magrittr")
library(magrittr)

#install.packages("lubridate")
library(lubridate)

#install.packages("jsonlite")
library(jsonlite)

#install.packages("rlist")
library(rlist)

#install.packages("tibble")
library(tibble)

#install.packages("tidyr")
library(tidyr)


#https://www.boxofficemojo.com/monthly/?view=releasedate&yr=2018&month=9&adjust_yr=2018&p=.htm


getYear = "2018"
getMonth = "2"
getAdjustyear = "2018"

base_url = "https://www.boxofficemojo.com/monthly/"
query_params = list(yr=getYear, month=getMonth, adjust_yr=getAdjustyear, view="releasedate")

resp = GET(url = base_url, query=query_params)

str(resp)

resp_html = content(resp)

# #using rvest functions
# install.packages("rvest")
# library(rvest)
# 
page_html = read_html(resp)
 
write_xml(page_html, file="resp_html.html")

title = page_html %>% 
 html_nodes("title") %>%
 html_text()
 

myMthColNames = c("Rank", "Title", "Studio", "Sales", "Theatres", "OpeningSales", "OpeningTheatres", "OpeningDate", "ClosingDate")

page_html %>% 
  html_nodes("table") %>%
  extract2(4) %>% 
  html_table(fill=TRUE) %>% 
  mutate(X10 = if (exists('X10', where = .)) X10 else NA) %>% 
  select(-X10) %>% #View()
  setNames(myMthColNames) %>% 
  filter(grepl(pattern="^\\d", Rank)) %>% 
  mutate(Year = getYear, 
         Month = getMonth) %>% 
  mutate(Sales = as.numeric(gsub("[\\$,]", "", Sales)),
         Theatres = as.numeric(gsub("[\\$,]", "", Theatres)),
         OpeningSales = as.numeric(gsub("[\\$,]", "", OpeningSales)),
         OpeningTheatres = as.numeric(gsub("[\\$,]", "", OpeningTheatres))
         ) %>% View()
  
  
  


 
#need to install this to access the list using extract2 function
# install.packages("magrittr")
# library(magrittr)
# 
# 
# myColNames = c("ThisWeek", "LastWeek", "Title", "Studio", "WeeklyGross", "PctChange", "TheatreCount", "TheatreChange", "Average", "TotalGross", "Budget", "WeekNum")
# 
# 
# weeklyBoxOffice = page_html %>%
#   html_nodes("table") %>%
#   extract2(5) %>% 
#   html_table() %>%
#   setNames(myColNames) %>%
#   filter(row_number()!=1) %>% 
#   filter(row_number()!=n()) %>%
#   mutate(calYear=getYear, calWeek=getWeek)
# 
# 
# #test errors
# 
#   tryCatch(
#     {
#       page_html %>%
#         html_nodes("table") %>%
#         extract2(5) %>% 
#         html_table() %>%
#         setNames(myColNames) %>%
#         filter(row_number()!=1) %>% 
#         filter(row_number()!=n()) %>%
#         mutate(calYear=getYear, calWeek=getWeek)
#     }
#     ,
#     error=function(e) return(NULL)
#   )



#------------------
# make it a function.

getWeeklyBoxOffice = function(theYear, theWeek, priceAdj) {
  
  #if (theYear <= year(now()) && theWeek <= week(now())-2) {
  
    tryCatch(
      {  
        base_url = "https://www.boxofficemojo.com/weekly/chart/"
        query_params = list(yr=theYear, wk=theWeek, adjust_yr=priceAdj)
        
        myResp = GET(url = base_url, query=query_params)
        
        myPage_html = read_html(myResp)
        
        myColNames = c("ThisWeek", "LastWeek", "Title", "Studio", "WeeklyGross", "PctChange", "TheatreCount", "TheatreChange", "Average", "TotalGross", "Budget", "WeekNum")
        
          myWeeklyBoxOffice = myPage_html %>%
          html_nodes("table") %>%
          extract2(5) %>%
          html_table() %>%
          setNames(myColNames) %>%
          filter(row_number()!=1) %>% 
          filter(row_number()!=n()) %>%
          mutate(calYear=theYear, calWeek=theWeek)
        
          return(myWeeklyBoxOffice)
      },
          error=function(e) return(NULL)
    )
  #}
  #else {
  #  return(NULL) 
  #}
}
         
        

df1 = getWeeklyBoxOffice("2017", "34", "2018")
df2 = getWeeklyBoxOffice("2018", "30", "2018")

View(df1)


#---------------------------------------------------------
#can now use rbind to concatenate rows.
#write a function to get data from start date to now


# week(ymd("2014-03-16", "2014-03-17","2014-03-18", '2014-01-01'))
# 
# startDate = "2017-01-01"


# countWeeks = floor(interval(startDate, now()) / duration(num=1, units="weeks"))
# 
# weekDates = ymd(startDate) + weeks(x = seq.int(from = 0, to = countWeeks, by = 1))
# 
# yearWeeks = data.frame(yr=year(weekDates), wk=week(weekDates))


getWeeklyBoxOfficeByDate = function(startDate, priceAdj) {
  
  countWeeks = floor(interval(startDate, now()) / duration(num=1, units="weeks"))
  
  weekDates = ymd(startDate) + weeks(x = seq.int(from = 0, to = countWeeks, by = 1))
  
  yearWeeks = data.frame(yr=year(weekDates), wk=week(weekDates))
  
  mydf = NULL
  fulldf = NULL
  for (x in 1:nrow(yearWeeks)) {
    myYear = yearWeeks[x,1]
    myWeek = yearWeeks[x,2]
    
    #wait 2 sec before next request to avoid spamming
    Sys.sleep(1)
    print(paste(myYear, myWeek, sep="-"))
    
    mydf = getWeeklyBoxOffice(myYear, myWeek, priceAdj)

    if(!is.null(mydf)) {
      if(!is.null(fulldf)) {

        fulldf = rbind(fulldf, mydf)

      } else {

        fulldf = mydf

      }

    }
  }
  return(fulldf)  

}

#-------------GET MONTHLY SALES FOR TIME SERIES

getMonthlyBoxOffice = function(theYear, theMonth, priceAdj, theType) {
  
  tryCatch(
    {  
      
      #check if proper date
      #only allow dates before current month. 
      theDay = "1"
      #theYear = "2018"
      #theMonth = "10"
      checkDate = paste(theYear, theMonth, theDay, sep="-")
      print(checkDate)
      
      if (!is.na(parse_date_time(checkDate,orders="ymd"))) {
        checkDate = as.Date(checkDate)
        
        if (as.duration(interval(checkDate,now())) %/% as.duration(months(1)) > 0) {
      
            #https://www.boxofficemojo.com/monthly/?view=releasedate&yr=2018&month=9&adjust_yr=2018&p=.htm
  
            if (is.na(theType)) {
              theType = "releasedate"
            }
            #other types:
            # - "releasedate" - full monthly results
            # - "widedate" - wide release movies only
            # - "limited" - limted release movies only
            
            
            base_url = "https://www.boxofficemojo.com/monthly/"
            query_params = list(yr=theYear, month=theMonth, adjust_yr=priceAdj, view=theType)
            
            myResp = GET(url = base_url, query=query_params)
            
            myPage_html = read_html(myResp)
          
            
            
            myMthColNames = c("Rank", "Title", "Studio", "Sales", "Theatres", "Opening", "OpeningTheatres", "OpeningDate", "ClosingDate")
            
            monthSales = myPage_html %>%
              html_nodes("table") %>%
              extract2(4) %>% 
              html_table(fill=TRUE) %>% 
              mutate(X10 = if (exists('X10', where = .)) X10 else NA) %>% 
              select(-X10) %>% #View()
              setNames(myMthColNames) %>% 
              filter(Rank == "Totals:") %>% 
              mutate(Year = theYear, 
                     Month = theMonth) %>% 
              select(Sales, Theatres, Year, Month) %>% 
              mutate(Sales = as.numeric(gsub("[\\$,]", "", Sales))) 
            
          
            return(monthSales)
      
      
        } else {
            return(NULL)
        }
        
      } else {
          return(NULL)
      }
      
    },
    error=function(e) return(NULL)
  )
}


#-----------------------GET BOX OFFICE BY MONTH TS -------------------------

getMonthlyBoxOfficeTSByDate = function(startDate, priceAdj, myType) {
  
  #startDate = "2017-1-1"
  cMonths = floor(interval(startDate, now()) / duration(num=1, units="months"))
  
  mDates = ymd(startDate) + months(x = seq.int(from = 0, to = cMonths, by = 1))
  
  yMonths = data.frame(yr=year(mDates), mth=month(mDates))
  
  
  mydf = NULL
  fulldf = NULL
  for (x in 1:nrow(yMonths)) {
    myYear = yMonths[x,1]
    myMonth = yMonths[x,2]
    
    #wait 2 sec before next request to avoid spamming
    Sys.sleep(1)
    print(paste(myYear, myMonth, "1", sep="-"))
    
    #other types:
    # - "releasedate" - full monthly results
    # - "widedate" - wide release movies only
    # - "limited" - limted release movies only
    
    mydf = getMonthlyBoxOffice(myYear, myMonth, priceAdj, myType)
    
    if(!is.null(mydf)) {
      if(!is.null(fulldf)) {
        
        fulldf = rbind(fulldf, mydf)
        
      } else {
        
        fulldf = mydf
        
      }
      
    }
  }
  return(fulldf)  
  
}


# - "releasedate" - full monthly results
# - "widedate" - wide release movies only
# - "limited" - limted release movies only
getdf = getMonthlyBoxOfficeTSByDate("1985-1-1", "2018", "releasedate")

write.csv(getdf, file="MonthlyTSResultsX.csv")


#-------------GET MONTHLY SALES PER MOVIE FOR TIME SERIES

#getdf = getMonthlyBoxOfficeMovies("2018", "9", "2018", "releasedate")


getMonthlyBoxOfficeMovies = function(theYear, theMonth, priceAdj, theType) {
  
  tryCatch(
    {  
      
      #check if proper date
      #only allow dates before current month. 
      theDay = "1"
      #theYear = "2018"
      #theMonth = "10"
      checkDate = paste(theYear, theMonth, theDay, sep="-")
      print(checkDate)
      
      if (!is.na(parse_date_time(checkDate,orders="ymd"))) {
        checkDate = as.Date(checkDate)
        
        if (as.duration(interval(checkDate,now())) %/% as.duration(months(1)) > 0) {
          
          #https://www.boxofficemojo.com/monthly/?view=releasedate&yr=2018&month=9&adjust_yr=2018&p=.htm
          
          if (is.na(theType)) {
            theType = "releasedate"
          }
          #other types:
          # - "releasedate" - full monthly results
          # - "widedate" - wide release movies only
          # - "limited" - limted release movies only
          
          
          base_url = "https://www.boxofficemojo.com/monthly/"
          query_params = list(yr=theYear, month=theMonth, adjust_yr=priceAdj, view=theType)
          
          myResp = GET(url = base_url, query=query_params)
          
          myPage_html = read_html(myResp)
          
          
          myMthColNames = c("Rank", "Title", "Studio", "Sales", "Theatres", "OpeningSales", "OpeningTheatres", "OpeningDate", "ClosingDate")
          
          monthSales = myPage_html %>%
            html_nodes("table") %>%
            extract2(4) %>% 
            html_table(fill=TRUE) %>% 
            mutate(X10 = if (exists('X10', where = .)) X10 else NA) %>% 
            select(-X10) %>% #View()
            setNames(myMthColNames) %>% 
            filter(grepl(pattern="^\\d", Rank)) %>% 
            mutate(Year = theYear, 
                   Month = theMonth) %>% 
            mutate(Sales = as.numeric(gsub("[\\$,]", "", Sales)),
                   Theatres = as.numeric(gsub("[\\$,]", "", Theatres)),
                   OpeningSales = as.numeric(gsub("[\\$,]", "", OpeningSales)),
                   OpeningTheatres = as.numeric(gsub("[\\$,]", "", OpeningTheatres))
            ) 

          
          return(monthSales)
          
          
        } else {
          return(NULL)
        }
        
      } else {
        return(NULL)
      }
      
    },
    error=function(e) return(NULL)
  )
}


#-----------------------GET MOVIES BY MONTH -------------------------
# - "releasedate" - full monthly results
# - "widedate" - wide release movies only
# - "limited" - limted release movies only
getmmovies = getMonthlyMoviesByDate("2014-1-1", "2018", "releasedate")

write.csv(getmmovies, file="MonthlyMoviesResults.csv")


getMonthlyMoviesByDate = function(startDate, priceAdj, myType) {
  
  #startDate = "2017-1-1"
  cMonths = floor(interval(startDate, now()) / duration(num=1, units="months"))
  
  mDates = ymd(startDate) + months(x = seq.int(from = 0, to = cMonths, by = 1))
  
  yMonths = data.frame(yr=year(mDates), mth=month(mDates))
  
  
  mydf = NULL
  fulldf = NULL
  for (x in 1:nrow(yMonths)) {
    myYear = yMonths[x,1]
    myMonth = yMonths[x,2]
    
    #wait 2 sec before next request to avoid spamming
    Sys.sleep(1)
    print(paste(myYear, myMonth, "1", sep="-"))
    
    #other types:
    # - "releasedate" - full monthly results
    # - "widedate" - wide release movies only
    # - "limited" - limted release movies only
    
    mydf = getMonthlyBoxOfficeMovies(myYear, myMonth, priceAdj, myType)
    
    
    if(!is.null(mydf)) {
      if(!is.null(fulldf)) {
        
        fulldf = rbind(fulldf, mydf)
        
      } else {
        
        fulldf = mydf
        
      }
      
    }
  }
  return(fulldf)  
  
}


#--------------------------------------------------------------------------------------

getdf = getWeeklyBoxOfficeByDate("2014-07-01", "2018")
 
write.csv(getdf, file="WeeklyBoxOfficeResults2015-2018.csv")


options(digits=10)

#view titles and sum weekly gross (strip $ and commas)
moviesbyTitle = getdf %>%
  group_by(Title, Studio) %>%
  mutate(startWeek=first(calWeek)) %>%
  group_by(Title, Studio, startWeek) %>% 
  summarize(calWeeklyGross = sum(as.numeric(gsub("[\\$,]", "", WeeklyGross))),
            totTheatreCount = sum(as.numeric(gsub("[\\$,]", "", TheatreCount))),
            totGross = max(as.numeric(gsub("[\\$,]", "", TotalGross))),
            totBudget = formatC(max(as.numeric(gsub("[\\$,]", "", Budget))* 1000000), digits = 10, format="d"),
            WeeksOn = max(WeekNum),
            startYear = min(as.numeric(calYear))
  ) %>%
  select(Title, Studio, calWeeklyGross, totTheatreCount, totGross, totBudget, WeeksOn, startYear, startWeek)



write.csv(moviesbyTitle, "MoviesByTitle.csv", row.names = FALSE)


options(scipen=999)

aggmovies = read.csv("MoviesByTitle.csv")

summary(aggmovies)

str(aggmovies)

summary(as.integer(aggmovies$WeeksOn))



md = read.csv("MovieRatingsFull.csv")

summary(md)

View(md)