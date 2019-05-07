library(stats)

set.seed(123)

impute.data <- mice(train, m = 5, maxit = 5, print = FALSE)

density.plot <- densityplot(impute.data)

train_scaled <- train %>% 
  dplyr::select(Alcohol,
                CitricAcid, 
                FixedAcidity, 
                FreeSulfurDioxide, 
                ResidualSugar, 
                Sulphates, 
                TotalSulfurDioxide, 
                VolatileAcidity)

  
positive_scale <- function(x) {
  lower_bound <- mean(x) - (stats::IQR(train$FixedAcidity) * 1.5)
  x <- x + lower_bound
  if(x > 0) {
    x } else {
     0 
    }
}
  
lapply(train_scaled, positive_scale)