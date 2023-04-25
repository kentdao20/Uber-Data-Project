# UBER HALF YEAR REPORT

## Author: Kent Dao

![image](https://user-images.githubusercontent.com/118495124/232948616-b19e9d8a-2a2e-44a9-af0f-790f1a1525e1.png)


## Overview:
  I will be doing an analysis base on 6 raw datasets of the uber riders in a location and see when will be the times with the most riders, what time during the month and how uber can oiptimize to prepare for the rush hour.

### Cleaning data:

**1. Put all csv file into one single dataframe**

```r

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

df_data <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
```

**2. Fix the date and time format so that we can separate them by using POSIXct**

```r
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
```

### Data Analysis:
**1. Making pivot table for trips by hour and graph**
```r
hour_data <- df_data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
  
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "violet", color = "steelblue") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
```

**2. Making pivot table for trips by hour each month and graph**
```r
month_hour <- df_data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)
```

**3. Making pivot table for trips each day and graph**
```r
day_group <- df_data %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 


ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "darkblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
```

**4. Making pivot table for trips each day during the month and graph**

```r
day_month_group <- df_data %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

```

**5. Making pivot table for trips each month and graph**

```r
month_group <- df_data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 



ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)
```

**6. Making pivot table for trips each month with weekdays and graph**

```r
month_weekday <- df_data %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

write.csv(month_weekday, "month_weekday.csv", row.names = FALSE)

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)
```


**7. Making graph for each base**


```r
ggplot(df_data, aes(Base)) + 
  geom_bar(fill = "cyan2") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")
```

**8. Making graph for each base each month**

```r
ggplot(df_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)
```

**9. Making graph for each base each day of the week**


```r
ggplot(df_data, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)
```

**10. Making pivot and heat map for each day and hour**

```r
day_and_hour <- df_data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())


ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="pink", high="violet") +
  ggtitle("Heat Map by Hour and Day")
```

**11. Make heat map for each day and month**

```r
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  ggtitle("Heat Map by Month and Day")
```

**12. Make heat map for each day of the week and month**

```r
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

```


**13. Make heat map for bases and month**

```r
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

```

**14. Make heat map for bases and day of the week**

```r
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")
```

**15. Prediction model**


```r
prediction_model <- df_data %>%
  group_by(hour, month, day, dayofweek) %>%
  summarise(Total_Trips = n())

write.csv(prediction_model, "prediction_model.csv", row.names = FALSE)

#graphing the model
ggplot(prediction_model, aes(x = month, y = Total_Trips, color = factor(day =="Sat"))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("darkblue", "violet"), guide = "none")+
  labs(color = "day == Sat")
```
[ShinyApp] (https://kienkcp.shinyapps.io/UberAnalysis/)
