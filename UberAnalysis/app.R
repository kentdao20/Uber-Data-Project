#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(modelr)

getwd()

day_and_hour <- read.csv("day_and_hour.csv")
day_group <- read.csv("day_group.csv")
day_month_group <- read.csv("day_month_group.csv")
day0fweek_bases <- read.csv("day0fweek_bases.csv")
hour_data <- read.csv("hour_data.csv")
month_base <- read.csv("month_base.csv")
month_group <- read.csv("month_group.csv")
month_hour <- read.csv("month_hour.csv")
month_weekday <- read.csv("month_weekday.csv")
prediction_model <- read.csv("prediction_model.csv")


ui<-fluidPage( 
  
  titlePanel(title = "Uber Data"),
  h4(""),
  
  
  tabsetPanel(
    
    # First tab content
    tabPanel("Day and Hour",
             div(style = "text-align: center;",
                 h1("Uber Trips by Day and Hour"),
                 p(""),
                 plotOutput('plot1', height = "675px"),
                
             )
    ),
    tabPanel("Trips everyday",
             div(style = "text-align: center;",
                 h1("Uber Trips everyday "),
                 p(""),
                 plotOutput('plot2', height = "675px"),
                 
             )
    ),
    tabPanel("Each day each month",
             div(style = "text-align: center;",
                 h1("Uber Trips by each day each month"),
                 p(""),
                 plotOutput('plot3', height = "675px"),
                 
             )
    ),
    tabPanel("Heat map by Bases and Day of Week",
             div(style = "text-align: center;",
                 h1("Uber Trips by Bases and Day of Week"),
                 p(""),
                 plotOutput('plot4', height = "675px"),
                 
             )
    ),
    tabPanel("Hourly trips",
             div(style = "text-align: center;",
                 h1("Uber Trips hourly "),
                 p(""),
                 plotOutput('plot5', height = "675px"),
                 
             )
    ),
    tabPanel("Heat map by month and base",
             div(style = "text-align: center;",
                 h1("Uber Trips by month and base "),
                 p(""),
                 plotOutput('plot6', height = "675px"),
                 
             )
    ),
    tabPanel("Total trip each month",
             div(style = "text-align: center;",
                 h1("Total trip each month"),
                 p(""),
                 plotOutput('plot7', height = "675px"),
                 
             )
    ),
    tabPanel("Hour and Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Hour and Month"),
                 p(""),
                 plotOutput('plot8', height = "675px"),
                 
             )
    ),
    tabPanel("Each month each day",
             div(style = "text-align: center;",
                 h1("Uber Trips by each month each day"),
                 p(""),
                 plotOutput('plot9', height = "675px"),
                 
             )
    ),
    tabPanel("Heat map day month",
             div(style = "text-align: center;",
                 h1("Heat map for each day each month "),
                 p(""),
                 plotOutput('plot10', height = "675px"),
                 
             )
    ),
    tabPanel("Prediction model",
             div(style = "text-align: center;",
                 h1("Prediction Model for Saturday each month "),
                 p(""),
                 plotOutput('plot11', height = "675px"),
                 
             )
    ),
    tabPanel("Leaflet map",
             div(style = "text-align: center;",
                 h1("Top 10 busiest spot "),
                 p("")
    
    
  )
)
))


server<-function(input,output){

  
  # creating chart
  day_and_hour_chart <-
    ggplot(day_and_hour, aes(day, hour, fill = Total)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low="pink", high="violet")
  
  day_group_chart <-
    ggplot(day_group, aes(day, Total)) + 
    geom_bar( stat = "identity", fill = "darkblue") +
    ggtitle("Trips Every Day") +
    theme(legend.position = "none")

  day_month_group_chart <-
    ggplot(day_month_group, aes(day, Total, fill = month)) + 
    geom_bar( stat = "identity") +
    ggtitle("Trips by Day and Month") 
  
  
  
  day0fweek_bases_chart <-
    ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
    geom_tile(color = "white") +
    ggtitle("Heat Map by Bases and Day of Week")
  
  
  hour_data_chart <-
    ggplot(hour_data, aes(hour, Total)) + 
    geom_bar( stat = "identity", fill = "violet", color = "steelblue")
  
  month_base_chart <-
    ggplot(month_base, aes(Base, month, fill = Total)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low="#06aa85", high="#0590f5") +
    ggtitle("Heat Map by Month and Bases")
  
  month_group_chart <-
    ggplot(month_group , aes(month, Total, fill = month)) + 
    geom_bar( stat = "identity") +
    ggtitle("Trips by Month") +
    theme(legend.position = "none") 
  
  month_hour_chart <-
    ggplot(month_hour, aes(hour, Total, fill = month)) + 
    geom_bar( stat = "identity") 
  
  month_weekday_chart <-
    ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
    geom_bar( stat = "identity", position = "dodge") +
    ggtitle("Trips by Day and Month") 
  
  prediction_model_chart <-
    ggplot(prediction_model, aes(x = month, y = Total_Trips, color = factor(day =="Sat"))) +
    geom_point(size = 3) +
    scale_color_manual(values = c("darkblue", "violet"), guide = "none")+
    labs(color = "day == Sat")
  
  day_month_group_heat <-
    ggplot(day_month_group, aes(day, month, fill = Total)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low="lightblue", high="darkblue") +
    ggtitle("Heat Map by Month and Day")
  
  output$plot1 = renderPlot({day_and_hour_chart})
  output$plot2 = renderPlot({day_group_chart})
  output$plot3 = renderPlot({day_month_group_chart})
  output$plot4 = renderPlot({day0fweek_bases_chart})
  output$plot5 = renderPlot({hour_data_chart})
  output$plot6 = renderPlot({month_base_chart})
  output$plot7 = renderPlot({month_group_chart})
  output$plot8 = renderPlot({month_hour_chart})
  output$plot9 = renderPlot({month_weekday_chart})
  output$plot10 = renderPlot({day_month_group_heat})
  output$plot11 = renderPlot({prediction_model_chart})
}



shinyApp(ui=ui, server=server) 