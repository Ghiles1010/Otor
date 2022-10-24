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

potential_targets <- function(data){
    
    # get the name of the columns with less than 2 unique values
    unique_values <- sapply(data, function(x) length(unique(x)))
    potential_targets <- names(unique_values[unique_values == 2])

    return(potential_targets)
}


overview_action <- function(input, output,session){

    if (is.null(input$file)){
        useShinyalert()
        shinyalert("Error", "Please upload a dataset", type = "error")
        return()
    }

    data <- read.csv(input$file$datapath)

    possible_targets <- potential_targets(data)

    if (length(possible_targets) == 0){
        useShinyalert()
        shinyalert("Error", "No possible target column found", type = "error",
                   closeOnClickOutside = TRUE,
                   callbackJS = "function(){location.reload();}")
        return()
    }

    session <- shiny::getDefaultReactiveDomain()
    updateTabsetPanel(session, "step_tabs", selected = "Overview")

    
    # read dataset on click next
    data <- read.csv(input$file$datapath)

    
    
    # show hea datast
    output$head_data <- renderTable({head(data)})
}