library(shiny)
options(shiny.autoreload = TRUE)


library(shinythemes)



source("tabs/dataset.R")
source("tabs/descriptive.R")
source("tabs/overview.R")
source("tabs/preprocessing.R")

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
    id="step_tabs",

    tabPanel("Dataset", dataset_tab), 
    tabPanel("Overview", overview_tab),
    tabPanel("Descriptive Analysis", descriptive_tab),
    tabPanel("Preprocessing", preprocessing_tab),
    tabPanel("Training"),
    tabPanel("Evaluation")
  
  ),

  fluidRow(
    id="action_buttons",
    actionButton("back_btn", "Back"),
    actionButton("next_btn", "Next"),
  ),
  
  

  tags$script("

      $('#step_tabs li').addClass('disabled')

      $('#step_tabs li').click(function () {
        if ($(this).hasClass('disabled')) {
            return false;
        }
    });

    var currentTab = 0;




    $('#back_btn').hide()

  

    $('#next_btn').click(function () {

        var isfileUploaded = document.getElementById('file_progress').children[0].innerHTML == 'Upload complete'

        if (isfileUploaded){
          if (currentTab + 1 != 6){
            currentTab ++
            $('#back_btn').show()
          }
        }

        
        if(currentTab == 5 ){
          $('#next_btn').hide()
        }
    })
    
  
  $('#back_btn').click(function () {
    
      currentTab --;
      $('#next_btn').show()

      if (currentTab == 0 ) {
        $('#back_btn').hide()
      }
    })

  ")
  

  ))