library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  navbarPage(
    "Otor",

    tabPanel("Dataset",


    ),


    tabPanel("Overview",


    ),

    tabPanel("Descriptive Analysis",


    ),


    tabPanel("Pretreatment",


    ),

    tabPanel("Training",


    ),

    tabPanel("Evaluation",


    )
  ),


  verticalLayout(
    a(href="http://example.com/link1", "Link One"),
    a(href="http://example.com/link2", "Link Two"),
    a(href="http://example.com/link3", "Link Three")
  )


  # Application title
  

  #     # theme = "cerulean",  # <--- To use a theme, uncomment this
  #   "My first app",
  #   tabPanel("Navbar 1",
  #             sidebarPanel(
  #               tags$h3("Input:"),
  #               textInput("txt1", "Given Name:", ""),
  #               textInput("txt2", "Surname:", ""),
                
  #             ), # sidebarPanel
  #             mainPanel(
  #                         h1("Header 1"),
                          
  #                         h4("Output 1"),
  #                         verbatimTextOutput("txtout"),

  #             ) # mainPanel
               
  # ),
  
  
  
  # Sidebar with a slider input for number of observations
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("obs", 
  #                 "Number of observations:", 
  #                 min = 1, 
  #                 max = 1000, 
  #                 value = 500)
  #   ),
    
  #   # Show a plot of the generated distribution
  #   mainPanel(
  #     plotOutput("distPlot")
  #   )
  # )
))