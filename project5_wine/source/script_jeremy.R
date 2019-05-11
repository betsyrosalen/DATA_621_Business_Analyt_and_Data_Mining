set.seed(123)

train_mice <- mice::mice(train, m = 5, maxit = 5, print = FALSE)
train_imputed <- mice::complete(train_mice)

#train_imputed %>% 
#is.na() %>% 
#colSums()

density.plot <- densityplot(train_mice)

train_for_scaling <- train_imputed %>% 
  dplyr::select(Alcohol,
                CitricAcid, 
                FixedAcidity, 
                FreeSulfurDioxide, 
                ResidualSugar, 
                Sulphates, 
                TotalSulfurDioxide, 
                VolatileAcidity)

positive_scale <- function(x) {
  low_bound <- mean(x, na.rm = TRUE) - (stats::IQR(x, na.rm = TRUE) * .5) * 1.5
  if(is.na(x)) {
    x = NA
  } else if(x < low_bound) {
    x = 0
  } else {
    x = x + abs(low_bound) 
  }
}
  
train_scaled <- lapply(train_for_scaling, positive_scale) %>% 
  as.data.frame()

#hist_lt_scaled <- train_scaled %>%
  #gather() %>%
  #ggplot(aes(value)) +
  #facet_wrap(~ key, scales = "free") +
  #geom_histogram(fill = "#58BFFF") +
  #xlab("") +
  #ylab("") +
  #theme(panel.background = element_blank())
