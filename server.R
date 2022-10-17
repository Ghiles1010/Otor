library(shiny)
source("tabs/dataset.R")
source("tabs/preprocessing.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # read dataset on click next
  observeEvent(input$next_btn, {

    # switch statement in R is f***ing weird
    switch(input$step_tabs, 
      "Dataset" = {preprocess_action(input, output)},
      "Overview" = {overview_action(input, output)},
      "Descriptive Analysis" = {descriptive_analysis_action(input, output)},
      "Pretreatment" = {pretreatment_action(input, output)},
      "Training" = {training_action(input, output)},
      "Evaluation" = {evaluation_action(input, output)}
    )
  })

  
  
})
