
renderValidationGraph <- function(val, description) {
  fig <- plot_ly(y = val, type = "box", name = "accuracy (succeed or fail)")
  fig <- fig %>% layout(title = description)

  return(fig)
}

calculateMetrics <- function(pred, testClass, model) {
  if (model == "KNN") {
    prob <- rep(NA, nrow(data.frame(testClass)))
    prob[pred == 1] <- attr(pred, "prob")[pred == 1]
    prob[pred == 0] <- 1 - attr(pred, "prob")[pred == 0]
    prob <- ifelse(prob > 0.5, 1, 0)
  } else if (model == "LR") {
    prob <- pred
  } else {
    pred <- as.numeric(pred)
    pred <- ifelse(pred == 2.0, 1.0, 0.0)
    prob <- pred
  }
  prec <- round(precision(as.numeric(prob), as.numeric(testClass)), 4)
  recall <- round(recall(as.numeric(prob), as.numeric(testClass)), 4)
  f1_score <- round(2 * (prec * recall) / (prec + recall), 4)

  df_output <- data.frame(Resultats = c("Precision"), Valeur = c(prec))
  df_output <- rbind(df_output, c("Recall", recall), c("F1_score", f1_score))

  return(df_output)
}

renderRoc <- function(type, testClass, pred) {
  prob <- rep(NA, nrow(testClass))

  if (type == "KNN") {
    prob[pred == 1] <- attr(pred, "prob")[pred == 1]
    prob[pred == 0] <- 1 - attr(pred, "prob")[pred == 0]
  } else if (type == "LR") {
    prob <- pred
  } else {
    pred <- as.numeric(pred)
    pred <- ifelse(pred == 2.0, 1.0, 0.0)
    prob <- pred
  }
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


trainModel <- function(type, data, target, input) {
  B <- 10
  valid <- rep(NA, 10)
  # We should remove this

  data[[target]] <- sapply(data[[target]], binarize)
  data[[target]] <- as.numeric(data[[target]])

  for (b in 1:B)
  {
    smp_size <- floor(0.75 * nrow(data))
    tr <- sample(seq_len(nrow(data)), smp_size)


    train <- data[tr, ]
    trainClass <- train[[target]]
    test <- data[-tr, ]
    testClass <- test[[target]]

    if (type == "KNN") {
      ka <- input$k

      pred <- knn(
        train[, -ncol(data)],
        test[, -ncol(data)],
        trainClass,
        k = ka,
        prob = TRUE
      )

      valid[b] <- mean(pred == testClass)
      df_output <- calculateMetrics(pred, testClass, "KNN")
    }
    if (type == "LR") {
      test <- subset(data[-tr, -grep(toString(target), colnames(data))])

      model <- glm(as.formula(paste(target, " ~ .")),
        data = train,
        family = "binomial"
      )

      # Predict test data based on model
      pred <- predict(model,
        test,
        type = "response"
      )
      predict_reg <- ifelse(pred > 0.5, 1, 0)

      valid[b] <- mean(as.numeric(testClass) == predict_reg)
      df_output <- calculateMetrics(predict_reg, testClass, "LR")
    }
    if (type == "SVM") {
      test <- subset(data[-tr, -grep(toString(target), colnames(data))])

      model <- svm(as.formula(paste(target, " ~ .")),
        data = train,
        type = "C-classification",
        kernel = "linear"
      )
      pred <- predict(model,
        test,
        type = "response"
      )
      valid[b] <- mean(pred == testClass)
      df_output <- calculateMetrics(pred, testClass, "SVM")
    }
  }
  return(list(data = data.frame(pred, testClass), validation = valid, test = data.frame(test), testClass = data.frame(testClass), df_table = df_output))
}