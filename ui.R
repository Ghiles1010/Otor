library(shiny)
library(shinythemes)

source("tabs/dataset.R")

shinyUI(fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  # css id
  id="main-wrapper",

  theme = shinytheme("flatly"),  
  navbarPage(
    "Otor"
  ),

  # # tabs
  tabsetPanel(
    id="step-tabs",

    tabPanel("Dataset", dataset_tab), 
    tabPanel("Overview"),
    tabPanel("Descriptive Analysis"),
    tabPanel("Pretreatment"),
    tabPanel("Training"),
    tabPanel("Evaluation")
  
  ),

  actionButton("next", "Next")
  

  ))