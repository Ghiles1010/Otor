library(ROCR)
library(class)
library(dplyr)
library(caTools)
library(pROC)
library(caret)
library(Metrics)
library(e1071)
library(plotly)
source("logic/trainingHelper.R")



training_tab <- fluidRow(
    tabsetPanel(
        tabPanel(
            "Classification: K Nearest neighbors",
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        "k",
                        strong("Select the Number K of Nearest Neighbours"),
                        value = 6,
                        min = 1,
                        max = 100
                    ),
                ),
                mainPanel(
                    fluidRow(
                        column(6, plotlyOutput(outputId = "accuracyBoxplot")),
                        column(6, tableOutput(outputId = "table_KNN"))
                    ),
                    fluidRow(
                        column(6, plotOutput(outputId = "ROC"))
                    )
                )
            )
        ),
        tabPanel(
            "Logistic regression",
            sidebarLayout(
                sidebarPanel(),
                mainPanel(
                    fluidRow(
                        column(6, plotlyOutput(outputId = "LRBoxplotRMSE")),
                        column(6, tableOutput("table_LR")),
                    ),
                    fluidRow(
                        column(6, plotOutput(outputId = "ROC_LR"))
                    )
                )
            )
        ),
        tabPanel(
            "Support Vector Machines",
            sidebarLayout(
                sidebarPanel(),
                mainPanel(
                    fluidRow(
                        column(6, plotlyOutput(outputId = "SVMBoxPlot")),
                        column(6, tableOutput("table_SVM")),
                    ),
                    fluidRow(
                        column(6, plotOutput(outputId = "ROC_SVM"))
                    )
                )
            )
        )
    )
)

training_action <- function(input, output, session){

    updateTabsetPanel(session, "step_tabs", selected = "Training")

    output$distPlot <- renderPlot({
        # generate an rnorm distribution and plot it
        dist <- rnorm(input$obs)
        hist(dist)
    })

  # KNN Accuracy boxplot
  output$accuracyBoxplot <- renderPlotly({
    modelOutput <- trainModel("KNN", session$userData$info$df, session$userData$info$target, input)

    output$table_KNN <- renderTable(modelOutput$df_table)
    return(renderValidationGraph(modelOutput$validation, "KNN validation accuracy (10-fold CV)"))
  })

  # KNN ROC Curve plot.
  output$ROC <- renderPlot({
    modelOutput <- trainModel("KNN", session$userData$info$df, session$userData$info$target, input)

    return(renderRoc("KNN", modelOutput$testClass, modelOutput$data$pred))
  })

  # Logistic Regression BoxPlot
  output$LRBoxplotRMSE <- renderPlotly({
    modelOutput <- trainModel("LR", session$userData$info$df, session$userData$info$target, input)
    # Calcul et affichage des résultas du modèle
    output$table_LR <- renderTable(modelOutput$df_table)
    return(renderValidationGraph(modelOutput$validation, "LR Accuracy (10-fold CV)"))
  })

  # LR ROC Curve plot.
  output$ROC_LR <- renderPlot({
    modelOutput <- trainModel("LR", session$userData$info$df, session$userData$info$target, input)

    return(renderRoc("LR", modelOutput$testClass, modelOutput$data$pred))
  })

  # SVM Accuracy box plot
  output$SVMBoxPlot <- renderPlotly({
    modelOutput <- trainModel("SVM", session$userData$info$df, session$userData$info$target, input)

    output$table_SVM <- renderTable(modelOutput$df_table)
    return(renderValidationGraph(modelOutput$validation, "SVM validation accuracy (10-fold CV)"))
  })

  # SVM ROC Curve plot.
  output$ROC_SVM <- renderPlot({
    modelOutput <- trainModel("SVM", session$userData$info$df, session$userData$info$target, input)

    return(renderRoc("SVM", modelOutput$testClass, modelOutput$data$pred))
  })

}