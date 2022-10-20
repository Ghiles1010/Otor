library(shiny)
source('trainingHelper.R')
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$distPlot <- renderPlot({

    # generate an rnorm distribution and plot it
    dist <- rnorm(input$obs)
    hist(dist)
  })


  # KNN Accuracy boxplot
  output$accuracyBoxplot <- renderPlotly({
    modelOutput <- trainModel('KNN',df_encoded,'G3', input)
    
    output$table_KNN <- renderTable(calculateMetrics(modelOutput$data$pred,modelOutput$data$testClass))
    
    return (renderValidationGraph(modelOutput$validation,"KNN validation accuracy (10-fold CV)"))
  })

  # KNN ROC Curve plot.
  output$ROC <- renderPlot({
    modelOutput <- trainModel('KNN',df_encoded,'G3',input)
    
    return (renderRoc(modelOutput$testClass, modelOutput$data$pred))
  
  })

  # Logistic Regression BoxPlot
  output$LRBoxplotRMSE <- renderPlotly({
    modelOutput<-trainModel('LR',df_encoded,'G3',input)
    
    #Calcul et affichage des résultas du modèle
    
    output$table_LR <- renderTable(calculateMetrics(modelOutput$data$pred,modelOutput$data$testClass))
   
    return (renderValidationGraph(modelOutput$validation,"LR validation accuracy (10-fold CV)"))
  })

  # Plot of difference between predicted and real grades.
  output$barplot_diff <- renderPlotly({
    set.seed(1)
    row.number <- sample(seq_len(nrow(df_encoded)), 0.65 * nrow(df_encoded))
    train <- df_encoded[row.number, ]

    train$G3 <- sapply(train$G3, binarize)
    train$G3 <- as.numeric(train$G3)

    test <- subset(df_encoded[-row.number, ], select = -c(G3))
  
    testClass <- df_encoded[-row.number, ]$G3
    testClass <- as.numeric(testClass)

    model <- glm(G3 ~ .,
      data = train,
      family = "binomial",
    )
    pred <- predict(model, newdata = test, type = "response")
  
    fig <- plot_ly(
      x = seq_len(nrow(test)),
      y = testClass,
      type = "scatter",
      mode = "lines+markers",
      name = "Real grade"
    )

    fig <- fig %>% add_trace(
      x = seq_along(pred),
      y = pred * max(testClass),
      type = "scatter",
      mode = "lines+markers",
      name = "Predicted grade"
    )

    return(fig)
  })


  # SVM Accuracy box plot
  output$SVMBoxPlot <- renderPlotly({
    
    modelOutput <- trainModel('SVM',df_encoded,'G3',input)
    print(modelOutput)
    return (renderValidationGraph(modelOutput$validation))
    
  })

  # Plot of difference between predicted and real grades.
  output$SVMbarplot_diff <- renderPlotly({
    set.seed(1)
    row.number <- sample(seq_len(nrow(df_encoded)), 0.65 * nrow(df_encoded))
    train <- df_encoded[row.number, ]

    train$G3 <- sapply(train$G3, binarize)

    train$G3 <- as.numeric(train$G3)
    # train <- sapply(train$G3, binarize)

    test <- subset(df_encoded[-row.number, ], select = -c(G3))
    # testClass <- sapply(df_encoded[-row.number, ]$G3, binarize)
    testClass <- df_encoded[-row.number, ]$G3
    testClass <- sapply(testClass, binarize)

    testClass <- as.numeric(testClass)

    svmModel <- svm(G3 ~ .,
      data = train,
      type = "C-classification",
      kernel = "linear"
    )

    # Predict test data based on model
    pred <- predict(svmModel,
      test,
      type = "response"
    )
    # result <- confusionMatrix(as.factor(pred), as.factor(testClass))
    # output$text <- renderText({pred})

    fig <- plot_ly(
      x = seq_len(nrow(test)),
      y = testClass,
      type = "scatter",
      mode = "lines+markers",
      name = "Real class"
    )

    fig <- fig %>% add_trace(
      x = seq_along(pred),
      y = pred,
      type = "scatter",
      mode = "lines+markers",
      name = "Predicted class"
    )

    return(fig)
  })
})
