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


#install.packages("lme4")
library(lme4)


#------------------------------------------------------------------------------
#                   START EDA
#------------------------------------------------------------------------------

#mclean = read.csv("MovieClean2.csv", stringsAsFactors = FALSE)

salesBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)
budgetBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

# mclean2 = mclean %>%
#   mutate(Wide_Release = totTheatreCount >= 2000) %>%
#   rename(
#     ID = X.1,
#     Theatres = totTheatreCount, 
#     Sales = totGross, 
#     Budget = totBudget, 
#     Weeks_Showing = WeeksOn, 
#     Year = startYear, 
#     Opening_Week = startWeek,
#     IMDB_Rating = IMDBRating,
#     RottenTomatoes_Rating = RTRating,
#     Metacritic_Rating = Metacritic
#   ) %>%
#   mutate(
#     G_Action = ifelse(is.na(G_Action), G_.Action, G_Action), 
#     G_Adventure = ifelse(is.na(G_Adventure), G_.Adventure, G_Adventure),  
#     G_Animation = ifelse(is.na(G_Animation), G_.Animation, G_Animation),  
#     G_Biography = ifelse(is.na(G_Biography), G_.Biography, G_Biography),  
#     G_Comedy = ifelse(is.na(G_Comedy), G_.Comedy, G_Comedy),     
#     G_Crime = ifelse(is.na(G_Crime), G_.Crime, G_Crime),
#     G_Drama = ifelse(is.na(G_Drama), G_.Drama, G_Drama),
#     G_Family = G_.Family,
#     G_Fantasy = ifelse(is.na(G_Fantasy), G_.Fantasy, G_Fantasy),    
#     G_History = G_.History,
#     G_Horror = ifelse(is.na(G_Horror), G_.Horror, G_Horror),     
#     G_Film_Noir = G_.Film.Noir,
#     G_Music = G_.Music,
#     G_Musical = ifelse(is.na(G_Musical), G_.Musical, G_Musical),    
#     G_Mystery = ifelse(is.na(G_Mystery), G_.Mystery, G_Mystery), 
#     G_News = G_.News,
#     G_Romance = ifelse(is.na(G_Romance), G_.Romance, G_Romance),   
#     G_Sci.Fi = ifelse(is.na(G_Sci.Fi), G_.Sci.Fi, G_Sci.Fi),
#     G_Short = ifelse(is.na(G_Short), G_.Short, G_Short),  
#     G_Sport = G_.Sport,
#     G_Thriller = ifelse(is.na(G_Thriller), G_.Thriller, G_Thriller),   
#     G_War = G_.War,
#     G_Western = ifelse(is.na(G_Western), G_.Western, G_Western)
#   ) %>%
#   select(-starts_with("G_.")) 
# 
# write.csv(mclean2, "MovieClean3.csv")

mclean2 = read.csv("MovieClean3.csv", stringsAsFactors = FALSE)

View(mclean2)

mclean2 = mclean2 %>%
            mutate(
              Studio = as.factor(Studio)
            )





#count number of genre title
mGenreCounts = mclean2 %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  group_by(Genre) %>%
  summarise(GenreTotal = sum(GValid)) %>%
  mutate(Genre = str_replace(Genre, "G_", "GC_")) %>%
  spread(Genre, GenreTotal)


mclean2 = cbind(mclean2, mGenreCounts)
View(mclean2)



#-----------------------theatre wide release
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Sales, x=as.factor(Wide_Release))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) + 
  labs(x="Wide Release")


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Budget, x=as.factor(Wide_Release))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) + 
  labs(x="Wide Release")


#-------------------------genre ---------------------------

#genre by sales
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  mutate(Genre = str_replace(Genre, "G_", "")) %>% 
  mutate(Highlight = Genre %in% c("Action", "Adventure", "Animation", "Fantasy", "Sci.Fi")) %>%
  ggplot(aes(y=Sales, x=as.factor(Genre), fill=Highlight)) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  scale_fill_manual(values=c("#808080", "#0FC3C7")) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Genre",
    y = "Box office Sales",
    title = "Relationship between Genre and Box Office Sales (2015-2017)",
    caption = "Data source: IMDb 2018"
  ) 


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  mutate(Genre = str_replace(Genre, "G_", "")) %>% 
  mutate(Highlight = Genre %in% c("Action", "Adventure", "Animation", "Fantasy", "Sci.Fi")) %>%
  ggplot(aes(y=Sales, x=Wide_Release, fill=as.factor(Genre))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  #scale_fill_manual(values=c("#808080", "#0FC3C7")) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Wide Release",
    y = "Box office Sales",
    title = "Relationship between Genre and Box Office Sales (2015-2017)",
    caption = "Data source: IMDb 2018"
  ) 

#genre by classification
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  mutate(Genre = str_replace(Genre, "G_", "")) %>% 
  mutate(Highlight = Genre %in% c("Action", "Adventure", "Animation", "Fantasy", "Sci.Fi")) %>%
  filter(!is.na(Rated)) %>% 
  mutate(Rated = replace(Rated, Rated %in% c("PG", "PG-13", "TV-PG", "TV-14"), "PG")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("G", "TV-Y7"), "G")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("R", "TV-MA"), "MA")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("NOT RATED", "UNRATED", "APPROVED", "PASSED"), "NOT RATED")) %>%
  mutate(Rated = factor(Rated, levels=c("G", "PG", "MA", "NOT RATED"))) %>% 
  mutate(Highlight = Rated %in% c("G", "PG")) %>% 
  ggplot(aes(x=as.factor(Genre), fill=factor(Rated))) +
    geom_bar(position="fill") + 
  theme_bw() + 
  theme(
    text = element_text(family = "Arial", color = "gray25"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  labs(
    x = "Genre",
    y = "Number of Movies",
    title = "Relationship between Genre and Classification (2015-2017)",
    caption = "Data source: IMDb 2018"
  ) 



#--------------------------------------------modelling


train = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(IMDB_Rating)) %>% 
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) %>%
  filter(!is.na(Budget))


test = mclean2 %>%
  filter(Year %in% c(2018)) %>%
  filter(!is.na(IMDB_Rating)) %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) %>%
  filter(!is.na(Budget))


#set levels
levels(test$Studio) = union(levels(train$Studio), levels(test$Studio)) 
levels(train$Studio) = levels(test$Studio)

levels(train$Studio) = unique(mclean2$Studio)
levels(test$Studio) = unique(mclean2$Studio)


unique(mclean2$Studio)
unique(test$Studio)
unique(train$Studio)

levels(test$Studio)
levels(train$Studio)

relevel(test$Studio, ref=1)
relevel(train$Studio, ref=1)



#movie.lmer.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres) + IMDB_Rating + Wide_Release + (1|Wide_Release)") #RMSE: 
movie.lmer.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres) + IMDB_Rating + (1|Wide_Release) + (1|Studio)") #RMSE: 0.5011694, 
movie.lmer.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres) + IMDB_Rating + (1|Wide_Release)") #RMSE: 0.6539358
                        # (1|G_Action) +
                        # (1|G_Adventure) +   
                        # (1|G_Animation) +   
                        # (1|G_Biography) +   
                        # (1|G_Comedy) +      
                        # (1|G_Crime) +       
                        # (1|G_Documentary) + 
                        # (1|G_Drama) +   
                        # (1|G_Family) +   
                        # (1|G_Fantasy) +   
                        # (1|G_History) + 
                        # (1|G_Horror) +    
                        # (1|G_Music) + 
                        # (1|G_Musical) +     
                        # (1|G_Mystery) +     
                        # (1|G_News) + 
                        # (1|G_Romance) +     
                        # (1|G_Sci.Fi) +   
                        # (1|G_Short) +  
                        # (1|G_Sport) + 
                        # (1|G_Thriller) + 
                        # (1|G_War) + 
                        # (1|G_Western) 
                        # ")
#define different intercepts (random effects) - tells te model there is going to be mulitple response per:
# - Wide Release - each movie could be treated differently if it's a wide release or not.
# - genere


movie.lmer.mod = lmer(movie.lmer.form, data=train)
summary(movie.lmer.mod)

plot(movie.lmer.mod)


train$pred_lmer = predict(movie.lmer.mod , newdata=train)
rmse(train$pred_lmer, log(train$Sales))
#0.6539358

train %>%
  ggplot(aes(x=pred_lmer, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")

test$pred_lmer = predict(movie.lmer.mod, newdata=test)
rmse(test$pred_lmer, log(test$Sales))
#0.5903622

test.rho = cor(test$pred_lmer, log(test$Sales))
test.rho2 = test.rho^2
test.rho2
#0.952218

test %>%
  ggplot(aes(x=pred_lmer, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")


#----------------------------------linear model

movie.lm.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres) + IMDB_Rating") #RMSE: 0.6547894
movie.lm.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres) + IMDB_Rating + Studio") #RMSE: 0.4793546

movie.lm.mod = lm(movie.lm.form, data=train)
summary(movie.lm.mod)



train$pred = predict(movie.lm.mod , newdata=train)
rmse(train$pred, log(train$Sales))
#0.6547894

sd(log(train$Sales))
#2.794472


train %>%
  ggplot(aes(x=pred, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")


#reset levels
movie.lm.mod$xlevels$Studio = union(movie.lm.mod$xlevels$Studio, levels(test$Studio))

test$pred = predict(movie.lm.mod, newdata=test)
rmse(test$pred, log(test$Sales))
#0.5878201

test.rho = cor(test$pred, log(test$Sales))
test.rho2 = test.rho^2
test.rho2
#0.952218

test %>%
  ggplot(aes(x=pred, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")







