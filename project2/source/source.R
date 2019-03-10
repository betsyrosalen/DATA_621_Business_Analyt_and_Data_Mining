# Q1
raw.data <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj2/master/classification-output-data.csv', stringsAsFactors = F, header = T)
data <- as.data.table(raw.data)

# Q2

q2.tbl <- table(data$scored.class,
             data$class)

# Q3 - Q8

ClassCalc <- function(tbl) {
  # with tbl, get TP, FP, FN, TN
  tp <- tbl[1,1];
  fp <- tbl[1,2];
  fn <- tbl[2,1];
  tn <- tbl[2,2];

  # calculate summary stats
  accuracy <- (tp+tn) / (tp+fp+tn+fn)
  error.rate <- (fp+fn) / (tp+fp+tn+fn)
  precision <- tp / (tp+fp)
  sensitivity <- tp / (tp+fn)
  specificity <- tn / (tn+fp)
  f1 <- (2 * precision * sensitivity) / (precision + sensitivity)

  # create dataframe
  df <- data.frame(accuracy = accuracy,
                   error.rate = error.rate,
                   precision = precision,
                   sensitivity = sensitivity,
                   specificity = specificity,
                   f1 = f1)
  return(df)
}

q3.q8.output <- ClassCalc(q2.tbl)

# Q 10

ROC_func <- function(labels, scores){
    labels <- labels[order(scores, decreasing=TRUE)]
    result <- data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
    dFPR <- c(diff(result$FPR), 0)
    dTPR <- c(diff(result$TPR), 0)
    AUC <- round(sum(result$TPR * dFPR) + sum(dTPR * dFPR)/2,4)
    plot(result$FPR,result$TPR,type="l",ylab="Sensitivity",xlab="1-Specificity",   xlim = c(-.5,1.5))
    legend(.6,.4,AUC,title = "AUC: ")
}
q10roc <- ROC_func(data$class,data$scored.probability)


#Q12

# Create dataframe for Q12, subsetting key columns: class, scored.class, scored.probability
q12.data <- data %>%
    dplyr::select(class, scored.class, scored.probability)

# Create confusion matrix using caret package
q12.confmtrx <- caret::confusionMatrix(data = factor(q12.data$scored.class), # predicted classes of observations in factor form
                                       reference = factor(q12.data$class)) # actual classes of observations in factor form

# reference doc: https://stackoverflow.com/questions/34842837/saving-output-of-confusionmatrix-as-a-csv-table

# Extract classification results of interest from caret::confusionMatrix object for comparison to custom function results
q12.output <- cbind(t(q12.confmtrx$overall),t(q12.confmtrx$byClass)) %>%
    data.frame() %>%
    mutate(error.rate = (1 - Accuracy)) %>%
    select(Accuracy, error.rate, Precision, Sensitivity, Specificity, F1) %>%
    janitor::clean_names() %>%
    rename(error.rate = error_rate)

# Table of custom function results
knitr::kable(q3.q8.output)

# Table of caret::confusionMatrix results
knitr::kable(q12.output)

# Compare results - evaluates as equal
all.equal(q12.output, q3.q8.output)


# Q 13

library(pROC)
rocCurve <- roc(data$class , data$scored.probability)
q13roc <- plot(rocCurve , legacy.axes = TRUE,   main = "pROC Package")

auc13 <- auc(rocCurve)
ci13 <- ci(rocCurve)