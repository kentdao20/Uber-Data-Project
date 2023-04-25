library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(reshape2)
library(shiny)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(DT)
library(scales)

rm(list=ls())

setwd("D:/DATA 332/Github Project 1/Uber-Data-Project/Raw data")

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

df_data <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
colors <- c("#CC1020", "#05a390","#0683d9","#f5e840", "#e075b6", "#cfcaff","#665566")

#formatting date time

df_data$Date.Time <- as.POSIXct(df_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

df_data$Time <- format(as.POSIXct(df_data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

df_data$Date.Time <- ymd_hms(df_data$Date.Time)

df_data$day <- factor(day(df_data$Date.Time))
df_data$month <- factor(month(df_data$Date.Time, label = TRUE))
df_data$year <- factor(year(df_data$Date.Time))
df_data$dayofweek <- factor(wday(df_data$Date.Time, label = TRUE))

df_data$hour <- factor(hour(hms(df_data$Time)))
df_data$minute <- factor(minute(hms(df_data$Time)))
df_data$second <- factor(second(hms(df_data$Time)))

#plotting data by hour

hour_data <- df_data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

write.csv(hour_data, "hour_data.csv", row.names = FALSE)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "violet", color = "steelblue") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#plotting data by hour and month

month_hour <- df_data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

write.csv(month_hour, "month_hour.csv", row.names = FALSE)

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

#plotting data by day

day_group <- df_data %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 

write.csv(day_group, "day_group.csv", row.names = FALSE)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "darkblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#plotting data by day & month

day_month_group <- df_data %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

write.csv(day_month_group, "day_month_group.csv", row.names = FALSE)

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#plotting data by month

month_group <- df_data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 

write.csv(month_group, "month_group.csv", row.names = FALSE)

ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#plotting data by day and month

month_weekday <- df_data %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

write.csv(month_weekday, "month_weekday.csv", row.names = FALSE)

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#plotting by bases

ggplot(df_data, aes(Base)) + 
  geom_bar(fill = "cyan2") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#by base and month

ggplot(df_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

#base and day of week

ggplot(df_data, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

#heatmap by day & hour

day_and_hour <- df_data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

write.csv(month_hour, "month_hour.csv", row.names = FALSE)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="pink", high="violet") +
  ggtitle("Heat Map by Hour and Day")

#month and day

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  ggtitle("Heat Map by Month and Day")

#month and day of week

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

#month and base

month_base <-  df_data %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  df_data %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="#06aa85", high="#0590f5") +
  ggtitle("Heat Map by Month and Bases")

#month and bases

ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

#prediction model

prediction_model <- df_data %>%
  group_by(hour, month, day, dayofweek) %>%
  summarise(Total_Trips = n())

write.csv(prediction_model, "prediction_model.csv", row.names = FALSE)

#graphing the model
ggplot(prediction_model, aes(x = month, y = Total_Trips, color = factor(day =="Sat"))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("darkblue", "violet"), guide = "none")+
  labs(color = "day == Sat")

