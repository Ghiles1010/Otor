library(shiny)

library(shinyalert)

source("tabs/dataset.R")
source("tabs/preprocessing.R")
source("tabs/descriptive.R")
source("tabs/overview.R")

# global variables
session <- NULL

shinyServer(function(input, output) {
  session <<- shiny::getDefaultReactiveDomain()

  session$userData$info <<- list(df=NULL, target=NULL) 
  # read dataset on click back
  observeEvent(input$back_btn, {
    
    switch(input$step_tabs, 
      "Overview" = {updateTabsetPanel(session, "step_tabs", selected = "Dataset")},
      "Descriptive Analysis" = {updateTabsetPanel(session, "step_tabs", selected = "Overview")},
      "Preprocessing" = {updateTabsetPanel(session, "step_tabs", selected = "Descriptive Analysis")},
      "Training" = {updateTabsetPanel(session, "step_tabs", selected = "Preprocessing")},
    )
  })


  # read dataset on click next
  observeEvent(input$next_btn, {
    # session <- shiny::getDefaultReactiveDomain()

    switch(input$step_tabs, 
      "Dataset" = {overview_action(input, output,session)},
      # "Dataset" = {preprocess_action(input, output, session)},
      "Overview" = {descriptive_analysis_action(input, output, session)},
      "Descriptive Analysis" = {preprocess_action(input, output, session)},
      "Preprocessing" = {training_action(input, output, session)}
    )
  })

  
})
