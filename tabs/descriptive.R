source('logic/univariate.R')
source('logic/bivariate.R')
library(plotly)
library(ggcorrplot)
library(reshape2)



descriptive_tab <-tabsetPanel(      
                    tabPanel(
                        "Correlation Matrix",
                        fluidRow(
                            plotOutput("heatmapCorrelation", width = "100%", height="600px")
                        )
                    ),
                    tabPanel(
                        "Univariate analysis",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(
                                    "univariateSelect",
                                    label = h4("Select a feature for univariate analysis"),
                                    choices = NULL,
                                    selected=1
                                )
                                # uiOutput('select_input')
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "occurencesPlot")),
                                    column(6, plotlyOutput(outputId = "frequencyPlot"))
                                ),
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "cumulativeOccurencesPlot")),
                                    column(6, tableOutput(outputId = "tabStat"))
                                )
                            )
                        )
                    ),
                    tabPanel("Bivariate analysis",
                             sidebarLayout(
                                 sidebarPanel(
                                     h4("Select 2 features for bivariate analysis"),
                                     selectInput(
                                         "bivariateFirstFeature",
                                         label = "First feature",
                                         choices = NULL,
                                         selected=1
                                     ),
                                     selectInput(
                                         "bivariateSecondFeature",
                                         label="Second feature",
                                         choices = NULL,
                                         selected = 1
                                     )
                                 ),
                                 mainPanel(
                                     fluidRow(
                                         column(
                                             6,
                                             fluidRow(
                                                 column(12, plotlyOutput(outputId = "bivariateCloudPoints")),
                                                 column(4, offset = 3, textOutput("correlation"))
                                             )
                                         ),
                                         column(6, plotlyOutput(outputId = "bivariateBoxplot"))
                                     )
                                 )
                             )
                    )
                )



descriptive_analysis_action <- function(input, output, session){
    # read dataset on click next
    data <- read.csv(input$file$datapath)
    # session <- shiny::getDefaultReactiveDomain()
    data_columns <-sort(names(data))
    
    updateSelectInput(session=session, inputId="univariateSelect",
        choices = data_columns)
    updateSelectInput(session=session, inputId="bivariateFirstFeature",
        choices = data_columns)

    updateSelectInput(session=session, inputId="bivariateSecondFeature",
        choices = data_columns)

    qualitativeTabStats <- reactive({
        table.tmp <- as.data.frame(table(data[, input$univariateSelect]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(
            table.tmp,
            table.tmp[[2]] / nrow(data) * 100,
            table.tmp[[3]] / nrow(data) * 100
        )
        colnames(table.tmp) <- c(
            input$univariateSelect, "Effectifs", "Effectifs Cum.",
            "Fréquences", "Fréquences Cum."
        )
        table.tmp
    })


    # Quantitative statistic table
    quantitativeTabStats <- reactive({
        data.frame(
            stat = c("min", "quantile 25%", "median", "quantile 75%", "max", "avg", "standard dev."),
            values = c(quantile(data[, input$univariateSelect]), mean(data[, input$univariateSelect]), sd(data[, input$univariateSelect]))
        )
    })
    # Reactive variable giving frequency measures on modalities of qualitative variables
   

    # Occurences (effectifs) plot for univariate analyzis
    # Depending on the variable type, it is either a boxplot or a barplot
    output$occurencesPlot <- renderPlotly({
        plotOccurences(input$univariateSelect,data)
    })

    # Frequencies plot
    # Depending on variable type, it is either a barplot or a pie chart
    output$frequencyPlot <- renderPlotly({
        plotFrequencies(input$univariateSelect,data)
    })

    # Cumulative occurences plot
    output$cumulativeOccurencesPlot <- renderPlotly({
        plotCumulativeOccurences(input$univariateSelect,data)
    })

    # Quantitative statistics summary
    output$tabStat <- renderTable({
        if (!is.numeric(data[, input$univariateSelect])) {
            return(NULL)
        }

        quantitativeTabStats()
    })

    # Bivariate cloud points
    output$bivariateCloudPoints <- renderPlotly({
        plotBivariateCloudPoints(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature,data
        )
    })

    # Correlation
    output$correlation <- renderText({
        computeCorrelation(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature,data
        )
    })

    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(data, is.numeric))
        corrMatrix.tmp <- round(cor(data[, nums.tmp]), 2)
        ggcorrplot(corrMatrix.tmp, hc.order = TRUE,
   lab = TRUE)

    })

    # Bivariate analysis boxplot plot
    output$bivariateBoxplot <- renderPlotly({
        bivariateBoxPlot(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature,data
        )
    })

    # Statistic measures based on variable type
    output$statsTableOut <- renderTable({
        if (is.numeric(data[, input$univariateSelect])) {
            quantitativeTabStats()
        } else {
            qualitativeTabStats()
        }
    })


    # select next tab
   
    updateTabsetPanel(session, "step_tabs", selected = "Descriptive Analysis")
    
}