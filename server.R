library(shiny)
source("tabs/dataset.R")
source("tabs/preprocessing.R")
source("tabs/descriptive.R")
source("tabs/overview.R")



shinyServer(function(input, output) {


  # read dataset on click next
  observeEvent(input$next_btn, {
    session <- shiny::getDefaultReactiveDomain()
    # switch statement in R is f***ing weird
    switch(input$step_tabs, 
      "Dataset" = {overview_action(input, output)},
      "Overview" = {descriptive_analysis_action(input, output,session)},
      "Descriptive Analysis" = {preprocess_action(input, output)},
      "Pretreatment" = {training_action(input, output)},
      "Training" = {evaluation_action(input, output)}
    )
  })

  
})
