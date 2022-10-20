library(ROCR)
library(class)
library(dplyr)
library(caTools)
library(pROC)
library(caret)
library(Metrics)
library(e1071)
df_encoded <- read.csv('df_encoded.csv')


binarize <- function(x)
{
    if (x<10) return("0") else return("1")
}

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
                        column(6, plotOutput(outputId = "ROC")),
                        column(6, plotlyOutput(outputId = "accuracyBoxplot")),
                        column(6, tableOutput(outputId = 'table_KNN'))
                    )
                )
            )
        ),
        tabPanel(
            "Logistic regression",
            sidebarLayout(
                sidebarPanel(
                ),
                mainPanel(
                    fluidRow(
                        column(6, plotlyOutput(outputId = "LRBoxplotRMSE")),
                        column(6, tableOutput('table_LR')), 
                    ),
                    fluidRow(
                        plotlyOutput(outputId = "barplot_diff")
                    )
                )
            )
        ),
        tabPanel(
            "Support Vector Machines",
            sidebarLayout(
                sidebarPanel(
                ),
                mainPanel(
                    fluidRow(
                        column(6, plotlyOutput(outputId = "SVMBoxPlot")),
                        column(6, tableOutput('table_SVM')), 
                    ),
                     fluidRow(
                         plotlyOutput(outputId = "SVMbarplot_diff")
                     )
                )
            )
        )
    )
)