library(shiny)
library(shinythemes)

source("tabs/dataset.R")
source("tabs/descriptive.R")

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
    tabPanel("Descriptive Analysis", descriptive_tab),
    tabPanel("Pretreatment"),
    tabPanel("Training"),
    tabPanel("Evaluation")
  
  ),

  actionButton("next", "Next")
  

  ))