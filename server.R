library(shiny)



shinyServer(function(input, output) {
    df <- read.csv("state.csv")

    # Reactive variable giving frequency measures on modalities of qualitative variables
    qualitativeTabStats <- reactive({
        table.tmp <- as.data.frame(table(df[, input$univariateSelect]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(
            table.tmp,
            table.tmp[[2]] / nrow(df) * 100,
            table.tmp[[3]] / nrow(df) * 100
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
            values = c(quantile(df[, input$univariateSelect]), mean(df[, input$univariateSelect]), sd(df[, input$univariateSelect]))
        )
    })

    # Occurences (effectifs) plot for univariate analyzis
    # Depending on the variable type, it is either a boxplot or a barplot
    output$occurencesPlot <- renderPlotly({
        plotOccurences(input$univariateSelect)
    })

    # Frequencies plot
    # Depending on variable type, it is either a barplot or a pie chart
    output$frequencyPlot <- renderPlotly({
        plotFrequencies(input$univariateSelect)
    })

    # Cumulative occurences plot
    output$cumulativeOccurencesPlot <- renderPlotly({
        plotCumulativeOccurences(input$univariateSelect)
    })

    # Quantitative statistics summary
    output$tabStat <- renderTable({
        if (!is.numeric(df[, input$univariateSelect])) {
            return(NULL)
        }

        quantitativeTabStats()
    })

    # Bivariate cloud points
    output$bivariateCloudPoints <- renderPlotly({
        plotBivariateCloudPoints(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    # Correlation
    output$correlation <- renderText({
        computeCorrelation(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(df, is.numeric))
        corrMatrix.tmp <- round(cor(df[, nums.tmp]), 2)
        ggcorrplot(corrMatrix.tmp, hc.order = TRUE,
   lab = TRUE)

    })

    # Bivariate analysis boxplot plot
    output$bivariateBoxplot <- renderPlotly({
        bivariateBoxPlot(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    # Statistic measures based on variable type
    output$statsTableOut <- renderTable({
        if (is.numeric(df[, input$univariateSelect])) {
            quantitativeTabStats()
        } else {
            qualitativeTabStats()
        }
    })
})