library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(

  # Application title
  titlePanel("Upload Data"),

  # upload data

  fluidRow(
    column(
        6,
        offset=4,
        sidebarLayout(
            fileInput("file", "Choose CSV file", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton("next", "Next", align="right")
        )
    ),

  ),



  
))