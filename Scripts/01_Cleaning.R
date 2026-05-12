
library(tidyverse)
library(ggplot2) 
library(readxl) 
library(lubridate)
library(broom)
library(viridis)
library(tsModel)
library(scales)
library(grid) 
library(tseries) 
library(tsibble) 
library(fable)
library(feasts) 
library(forecast)  
getwd()
source("./Scripts/functions.r")


#Pre-2018 Data -- female
pre_18_f <- read.csv("./RawData/Repro_Assault_Pre_2018/Female.csv")
pre_18_f <- pre_18_f %>%  
  filter(Month != "") %>% 
  mutate(month_year = my(Month),
         month = yearmonth(month_year),
         Deaths = na_if(as.character(Deaths), "Suppressed"),
         Deaths = as.numeric(Deaths),) %>% 
  select(-Notes,-Population,-"Crude.Rate", -Month, -"Month.Code")  
head(pre_18_f) 
tail(pre_18_f) 
#228 observations

#we need to wait to make it a time series object until we've merged the data from before!! 

rawdata_female <- read_excel("./RawData/Repro_Assault_Post_2018/Female.xlsx")
f_post_18 <- rawdata_female %>% 
  mutate(month_year = my(Month),
         Deaths = na_if(as.character(Deaths), "Suppressed"),
         Deaths = as.numeric(Deaths)) %>% 
  filter(!is.na(month_year)) %>%   # removing the blank rows
  select(-Notes,-Population,-"Crude Rate", -Month, -"Month Code", 
         -"Crude Rate Lower 95% Confidence Interval", 
         -"Crude Rate Upper 95% Confidence Interval")
head(f_post_18) 
tail(f_post_18) 

f_post_18 <- f_post_18 %>% 
  mutate(month_year = ymd(month_year),
         month = yearmonth(month_year)) %>% 
  select(-sex,-time)
head(f_post_18)  
tail(f_post_18) 
#84 observations

#checking range of dates
range(pre_18_f$month_year)
range(f_post_18$month_year) 

#combining female pre-18 and post-18 data

female_full <- bind_rows(pre_18_f,f_post_18) 
female_full <- female_full %>% 
  mutate( time = row_number())
head(female_full)
tail(female_full) 
glimpse(female_full)  

#saving data 

write.csv(female_full, "./CleanData/female_full.csv")


# MALE DATA---- 
#cleaning data: 
pre_18_m <- read.csv("./RawData/Repro_Assault_Pre_2018/Male.csv")
pre_18_m <- pre_18_m %>%  
  filter(Month != "") %>% 
  mutate(month_year = my(Month),
         month = yearmonth(month_year),
         Deaths = na_if(as.character(Deaths), "Suppressed"),
         Deaths = as.numeric(Deaths),) %>% 
  select(-Notes,-Population,-"Crude.Rate", -Month, -"Month.Code")  
head(pre_18_m)
tail(pre_18_m) 
#228 observations

#we need to wait to make it a time series object until we've merged the data from before!! 
m_post_18 <- read.csv("./CleanData/monthly_m.csv") 
glimpse(m_post_18)
m_post_18 <- m_post_18 %>% 
  mutate(month_year = ymd(month_year),
         month = yearmonth(month_year)) %>% 
  select(-time)
head(m_post_18)  
tail(m_post_18)
#84 observations 


range(pre_18_m$month_year)
range(m_post_18$month_year)

male_full <- bind_rows(pre_18_m,m_post_18) 
male_full <- male_full %>% 
  mutate( time = row_number())
head(male_full)
tail(male_full) 
glimpse(male_full)
#correct number of observations  

write.csv(male_full, "./CleanData/male_full.csv")

