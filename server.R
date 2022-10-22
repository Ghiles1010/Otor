library(shiny)

library(shinyalert)

source("tabs/dataset.R")
source("tabs/preprocessing.R")
source("tabs/descriptive.R")
source("tabs/overview.R")



shinyServer(function(input, output) {

  # read dataset on click back
  observeEvent(input$back_btn, {
    session <- shiny::getDefaultReactiveDomain()
    
    switch(input$step_tabs, 
      "Overview" = {updateTabsetPanel(session, "step_tabs", selected = "Dataset")},
      "Descriptive Analysis" = {updateTabsetPanel(session, "step_tabs", selected = "Overview")},
      "Pretreatment" = {updateTabsetPanel(session, "step_tabs", selected = "Descriptive Analysis")},
      "Training" = {updateTabsetPanel(session, "step_tabs", selected = "Pretreatment")},
      "Evaluation" = {updateTabsetPanel(session, "step_tabs", selected = "Training")}
    )
  })

  # read dataset on click next
  observeEvent(input$next_btn, {
    session <- shiny::getDefaultReactiveDomain()

    switch(input$step_tabs, 
      "Dataset" = {overview_action(input, output)},
      "Overview" = {descriptive_analysis_action(input, output,session)},
      "Descriptive Analysis" = {preprocess_action(input, output)},
      "Pretreatment" = {training_action(input, output)},
      "Training" = {evaluation_action(input, output)}
    )
  })

  
})
