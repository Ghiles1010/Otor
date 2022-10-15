source('logic/univariate.R')
source('logic/bivariate.R')
library(plotly)
library(ggcorrplot)

df <- read.csv('state.csv')


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
                                    choices = sort(names(df)),
                                    selected=1
                                )
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
                                         choices = names(df),
                                         selected=1
                                     ),
                                     selectInput(
                                         "bivariateSecondFeature",
                                         label="Second feature",
                                         choices = sort(names(df)),
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