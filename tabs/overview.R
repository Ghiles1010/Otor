overview_tab <- fluidRow(
        # add class
        class = "custom-tab",
        column(12, 
            # show dataset
            h3("Overview"),
            h6("Dataset"),
            htmlOutput("head_data"),
        )
    )




overview_action <- function(input, output){

    if (is.null(input$file)){
        useShinyalert()
        shinyalert("Error", "Please upload a dataset", type = "error")
        return()
    }

    session <- shiny::getDefaultReactiveDomain()
    updateTabsetPanel(session, "step_tabs", selected = "Overview")

    
    # read dataset on click next
    data <- read.csv(input$file$datapath)

    
    
    # show hea datast
    output$head_data <- renderTable({head(data)})
}