library(shiny)

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
    B <- 10
    acc_valid <- rep(NA, 10)
    df_encoded$G3 <- sapply(df_encoded$G3, binarize)

    for (b in 1:B)
    {
      smp_size <- floor(0.75 * nrow(df_encoded))
      tr <- sample(seq_len(nrow(df_encoded)), smp_size)


      train <- df_encoded[tr, ]
      trainClass <- train$G3

      test <- df_encoded[-tr, ]
      testClass <- test$G3

      ka <- input$k

      pred <- knn(
        train[, -ncol(df_encoded)],
        test[, -ncol(df_encoded)],
        trainClass,
        k = ka
      )
       acc_valid[b] <- mean(pred == testClass)
       #  predict_reg <- ifelse(pred > 0.5, 1, 0)

       #Calcul et affichage des résultas du modèle
    df_pred = data.frame(pred, testClass)
    prec <- round(precision( df_pred$pred, df_pred$testClass),4)
    recall <- round(recall(df_pred$pred, df_pred$testClass),4)
    f1_score <- round(2 *  (prec * recall) / (prec + recall),4)

    df_output = data.frame(Resultats = c("Precision"), Valeur = c(prec))
    df_output <- rbind(df_output, c("Recall", recall), c("F1_score", f1_score))
    

    output$table_KNN <- renderTable(df_output)
    }
    fig <- plot_ly(y = acc_valid, type = "box", name = "accuracy (succeed or fail)")
    fig <- fig %>% layout(title = "KNN validation accuracy (10-fold CV)")

    return(fig)
  })

  # KNN ROC Curve plot.
  output$ROC <- renderPlot({
    smp_size <- floor(0.75 * nrow(df_encoded))
    tr <- sample(seq_len(nrow(df_encoded)), smp_size)

    df_encoded$G3 <- sapply(df_encoded$G3, binarize)

    train <- subset(df_encoded[tr, ], select = -c(G3))
    trainClass <- df_encoded[tr, ]$G3

    test <- subset(df_encoded[-tr, ], select = -c(G3))

    testClass <- as.numeric(df_encoded[-tr, ]$G3)

    prob <- rep(NA, nrow(test))
    ka <- input$k

    res <- knn(
      train,
      test,
      trainClass,
      k = ka,
      prob = TRUE
    )

    prob[res == 1] <- attr(res, "prob")[res == 1]
    prob[res == 0] <- 1 - attr(res, "prob")[res == 0]

    pred <- prediction(prob, testClass)
    perf <- performance(pred, "tpr", "fpr")

    # knn AUC Curve plot
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]

    plot(perf, main = "ROC Curve", col = "red")
    abline(a = 0, b = 1)

    auc <- round(auc, 4)
    legend(.6, .4, auc, title = "AUC", cex = 1)
    

  })

  # Logistic Regression BoxPlot
  output$LRBoxplotRMSE <- renderPlotly({
    B <- 10
    rmse_valid <- rep(NA, 10)

    df_encoded$G3 <- sapply(df_encoded$G3, binarize)

    for (b in 1:B)
    {
      smp_size <- floor(0.75 * nrow(df_encoded))
      tr <- sample(seq_len(nrow(df_encoded)), smp_size)

      train <- df_encoded[tr, ]
      trainClass <- df_encoded[tr, ]$G3

      # test <- df_encoded[-tr, ]
      test <- subset(df_encoded[-tr, ], select = -c(G3))
      testClass <- as.numeric(df_encoded[-tr, ]$G3)

      #ka <- input$k

      train$G3 <- as.numeric(train$G3)

      logistic_model <- glm(G3 ~ .,
        data = train,
        family = "binomial"
      )

      # Predict test data based on model
      pred <- predict(logistic_model,
        test,
        type = "response"
      )
      predict_reg <- ifelse(pred > 0.5, 1, 0)

      rmse_valid[b] <- rmse(testClass, predict_reg)

    
    #Calcul et affichage des résultas du modèle
    df_pred = data.frame(predict_reg, testClass)
    prec <- round(precision(  df_pred$predict_reg, df_pred$testClass),4)
    recall <- round(recall(df_pred$predict_reg, df_pred$testClass),4)
    f1_score <- round(2 *  (prec * recall) / (prec + recall),4)

    df_output = data.frame(Resultats = c("Precision"), Valeur = c(prec))
    df_output <- rbind(df_output, c("Recall", recall), c("F1_score", f1_score))
    

    output$table_LR <- renderTable(df_output)
   
    }

    fig <- plot_ly(y = rmse_valid, type = "box", name = "rmse on grade prediction")
    fig <- fig %>% layout(title = "Logistic Regression validation RMSE (10-fold CV)")

    return(fig)
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
    B <- 10
    accuracy_valid <- rep(NA, 10)

    df_encoded$G3 <- sapply(df_encoded$G3, binarize)

    for (b in 1:B)
    {
      smp_size <- floor(0.75 * nrow(df_encoded))
      tr <- sample(seq_len(nrow(df_encoded)), smp_size)

      train <- df_encoded[tr, ]
      trainClass <- df_encoded[tr, ]$G3
      train$G3 <- as.numeric(train$G3)

      test <- subset(df_encoded[-tr, ], select = -c(G3))
      testClass <- as.numeric(df_encoded[-tr, ]$G3)

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
      #predict_reg <- ifelse(pred > 0.5, 1, 0)
      

      accuracy_valid[b] <- mean(pred == testClass)

    #    #Calcul et affichage des résultas du modèle
    # df_pred = data.frame(pred, testClass)
    # prec <- round(precision(  df_pred$pred, df_pred$testClass),4)
    # recall <- round(recall(df_pred$pred, df_pred$testClass),4)
    # f1_score <- round(2 *  (prec * recall) / (prec + recall),4)

    # df_output = data.frame(Resultats = c("Precision"), Valeur = c(prec))
    # df_output <- rbind(df_output, c("Recall", recall), c("F1_score", f1_score))
    

    # output$table_svm <- renderTable(df_output)
    }

    fig <- plot_ly(y = accuracy_valid, type = "box", name = "accuracy (succeed or fail)")
    fig <- fig %>% layout(title = "SVM validation accuracy (10-fold CV)")

    return(fig)
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
