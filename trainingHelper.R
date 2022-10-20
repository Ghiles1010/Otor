
renderValidationGraph<-function(val, description){

    fig <- plot_ly(y = val, type = "box", name = "accuracy (succeed or fail)")
    fig <- fig %>% layout(title = description)

    return(fig)
}

calculateMetrics<-function(pred, testClass){
    prob <- rep(NA, nrow(data.frame(testClass)))
    prob[pred == 1] <- attr(pred, "prob")[pred == 1]
    prob[pred == 0] <- 1 - attr(pred, "prob")[pred == 0]
    prec <- round(precision(prob , as.numeric(testClass) ),4)
    recall <- round(recall(prob, as.numeric(testClass)),4)
    f1_score <- round(2 *  (prec * recall) / (prec + recall),4)

    df_output = data.frame(Resultats = c("Precision"), Valeur = c(prec))
    df_output <- rbind(df_output, c("Recall", recall), c("F1_score", f1_score))
    
    return(df_output)
    
}

renderRoc<-function(testClass, pred){
    prob <- rep(NA, nrow(testClass))
    
    prob[pred == 1] <- attr(pred, "prob")[pred == 1]
    prob[pred == 0] <- 1 - attr(pred, "prob")[pred == 0]

    pred <- prediction(prob, testClass)
    perf <- performance(pred, "tpr", "fpr")

    # knn AUC Curve plot
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]

    plot(perf, main = "ROC Curve", col = "red")
    abline(a = 0, b = 1)

    auc <- round(auc, 4)
    legend(.6, .4, auc, title = "AUC", cex = 1)
}


trainModel<-function(type,data,target, input){
    B <- 10
    valid <- rep(NA, 10)
    # We should remove this 

    data[[target]] <- sapply(data[[target]], binarize)
    for (b in 1:B)
    {
      smp_size <- floor(0.75 * nrow(data))
      tr <- sample(seq_len(nrow(data)), smp_size)

      train <- data[tr, ]
      trainClass <- train[[target]]
      test <- data[-tr, ]
      testClass <- test[[target]]
      
      if(type == 'KNN')
      {
        ka <- input$k

        pred <- knn(
          train[, -ncol(data)],
          test[, -ncol(data)],
          trainClass,
          k = ka,
          prob = TRUE
        )
        valid[b] <- mean(pred == testClass)
      
      }
      if(type == 'LR')
      {

        testClass <- as.numeric(data[-tr, ][[target]])

        attr(train,target) <- as.numeric( train[[target]])

        model <- glm(as.formula(paste(target,' ~ .')),
          data = train,
          family = "binomial"
        )

        # Predict test data based on model
        pred <- predict(model,
          test,
          type = "response"
        )
        predict_reg <- ifelse(pred > 0.5, 1, 0)

        valid[b] <- rmse(testClass, predict_reg)
      }
      if(type == 'SVM')
      {
        train[[target]] <- as.numeric(train[[target]])

        test <- subset(data[-tr, ], select = -c(paste(target)))
        testClass <- as.numeric(data[-tr, ][[target]])
        
        model <- svm(as.formula(paste(target),'~ .'),
        data = train,
        type = "C-classification",
        kernel = "linear"
      )
       pred <- predict(model,
        test,
        type = "response"
      )
      valid[b] <- mean(pred == testClass)
      }
      
  }
    return(list(data = data.frame(pred, testClass), validation = valid, test = data.frame(test),testClass= data.frame(testClass)))
    
  }