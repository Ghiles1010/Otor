library(shiny)
source("tabs/dataset.R")
source("tabs/preprocessing.R")
source("tabs/descriptive.R")


shinyServer(function(input, output) {


  # read dataset on click next
  observeEvent(input$next_btn, {
    session <- shiny::getDefaultReactiveDomain()
    # switch statement in R is f***ing weird
    switch(input$step_tabs, 
      "Dataset" = {descriptive_analysis_action(input, output,session)},
      "Overview" = {overview_action(input, output)},
      "Descriptive Analysis" = {descriptive_analysis_action(input, output,session)},
      "Pretreatment" = {pretreatment_action(input, output)},
      "Training" = {training_action(input, output)},
      "Evaluation" = {evaluation_action(input, output)}
    )
  })

  
})
