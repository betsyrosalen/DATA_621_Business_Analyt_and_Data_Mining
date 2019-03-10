raw.data <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj2/master/classification-output-data.csv', stringsAsFactors = F, header = T)
data <- data.table::as.data.table(raw.data)
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

# Q 13

library(pROC)
rocCurve <- roc(data$class , data$scored.probability)
q13roc <- plot(rocCurve , legacy.axes = TRUE,   main = "pROC Package")

auc13 <- auc(rocCurve)
ci13 <- ci(rocCurve)