set.seed(123)

impute.data <- mice::mice(train, m = 5, maxit = 5, print = FALSE)

density.plot <- densityplot(impute.data)

train_for_scaling <- train %>% 
  dplyr::select(Alcohol,
                CitricAcid, 
                FixedAcidity, 
                FreeSulfurDioxide, 
                ResidualSugar, 
                Sulphates, 
                TotalSulfurDioxide, 
                VolatileAcidity)

positive_scale <- function(x) {
  low_bound <- mean(x, na.rm = TRUE) - stats::IQR(x, na.rm = TRUE) * 1.5
  if(x < low_bound) {
    x = 0
  } else {
    x = x + abs(low_bound) 
  }
}
  
train_scaled <- lapply(train_for_scaling, positive_scale) %>% 
  as.data.frame()

hist_lt_scaled <- train_scaled %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())
