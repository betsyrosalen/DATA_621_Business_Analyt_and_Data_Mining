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