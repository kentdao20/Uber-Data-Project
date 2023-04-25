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

ui <- fluidPage(
  titlePanel("Uber Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", "Select Data", choices = c("day_group", "day_month_group", "hour_data", "month_group", "month_hour", "month_weekday", "prediction_model"), selected = "day_group")
    ),
    mainPanel(
      plotOutput("bar_chart"),
      DT::dataTableOutput("table")
    )
  )
)


server <- function(input, output)
{
  
  

  selected_data <- reactive({
    switch(input$data_select,
           "day_group" = day_group,
           "day_month_group" = day_month_group,
           "hour_data" = hour_data,
           "month_group" = month_group,
           "month_hour" = month_hour,
           "month_weekday" = month_weekday,
           "prediction_model" = prediction_model)
  })
  
  # Render bar chart
  output$bar_chart <- renderPlot({
    #using if and else statement as per users selection
    #if the user selects base and day data then it display x and y depending on the table value
    if (input$data_select == "day_group`  ") {
      ggplot(selected_data(), aes(x = dayofweek, y = Total, fill = Base)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Day of Week", y = "Total") +
        theme_minimal()
    }
    #if the user selects base and month data then it display x and y depending on the table value
    else if (input$data_select == "df_base_month") {
      ggplot(selected_data(), aes(x = Month, y = Total, fill = Base)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
        theme_minimal()
    }
    #if the user selects day and hour data then it display x and y depending on the table value
    else if (input$data_select == "df_day_hour") {
      ggplot(selected_data(), aes(x = Hour, y = Total, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #if the user selects day of week and month data then it display x and y depending on the table value
    else if (input$data_select == "df_dayofweek_month") {
      ggplot(selected_data(), aes(x = dayofweek, y = Total, fill = Month)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "dayofweek", y = "Total") +
        theme_minimal()
    }
    #if the user selects base and day data then it display x and y depending on the table value
    else if (input$data_select == "df_hour_month") {
      ggplot(selected_data(), aes(x = Hour, y = Total, fill = Month)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #if the user selects month and day data then it display x and y depending on the table value
    else if (input$data_select == "df_month_day") {
      ggplot(selected_data(), aes(x = Month, y = Total, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
        theme_minimal()
    }
    #if the user selects month then it display x and y depending on the table value
    else if (input$data_select == "df_month") {
      ggplot(selected_data(), aes(x = Month, y = Total)) +
        geom_bar(stat = "identity", fill= "red") +
        labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
        theme_minimal()
    }
    #if the user selects hour then it display x and y depending on the table value
    else if (input$data_select == "df_hour") {
      ggplot(selected_data(), aes(x = Hour, y = Total)) +
        geom_bar(stat = "identity", fill="forestgreen") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #Prediction to know which month has the highest number of people use uber after or at 6 pm.
    else if (input$data_select == "prediction") {
      ggplot(df_hour_month, aes(x = Month, y = Total, color = factor(Hour > 18))) +
        geom_point(size = 4) +
        scale_color_manual(values = c("black", "red"), guide = FALSE) +
        labs(color = "Hour > 18")
  }
})
}
# Run the application 
shinyApp(ui = ui, server = server)
