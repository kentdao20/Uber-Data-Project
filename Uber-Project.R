library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(reshape2)
library(shiny)
library(ggplot2)
library(lubridate)

rm(list=ls())
setwd("D:/DATA 332/Github Project 1/Uber-Data-Project/Raw data")

df1<-read.csv("uber-raw-data-apr14.csv")
df2<-read.csv("uber-raw-data-aug14.csv")
df3<-read.csv("uber-raw-data-jul14.csv")
df4<-read.csv("uber-raw-data-jun14.csv")
df5<-read.csv("uber-raw-data-may14.csv")
df6<-read.csv("uber-raw-data-sep14.csv")

df_combined <- rbind(df1, df2, df3, df4, df5, df6)

df_data<- separate(df_combined, col = Date.Time, into = c("Date", "Time"), sep = " ")

pivot_df <- table(df_data$Time)

view(pivot_df )

# add Month and day

df_data$Date <- mdy(df_data$Date)
  
df_data$month <- month(df_data$Date)

df_data$day <- day(df_data$Date)

#add hour

df_data$Time <- hms(df_data$Time)

df_data$hour <- hour(df_data$Time)

# Graph month vs amount

df_groupby_month <- df_data%>%
  group_by(month) %>% 
  summarise(count = n() )

ggplot(df_groupby_month, aes(x = month, y = count,fill = "amount")) + 
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Amount", title = "                               Drive per month")

#graph hour vs amount

df_groupby_hour <- df_data%>%
  group_by(hour) %>% 
  summarise(count = n() )

ggplot(df_groupby_hour, aes(x = hour , y = count,fill = "hour")) + 
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Amount", title = "                                   Drive each hour")

#Trips each month, separate by month

df_groupby_day <- df_data%>%
  group_by(day) %>% 
  summarise(count = n() )

ggplot(df_groupby_day, aes(x = day , y = count,fill = "day")) + 
  geom_bar(stat = "identity") +
  labs(x = "Day", y = "Amount", title = "Drive each day throughout 6 months")

#Trips by day and month



#Chart: X is hour (0,6,12,18,24), Y is amount in month (stack/ group)
#Charts: Trips by hour
