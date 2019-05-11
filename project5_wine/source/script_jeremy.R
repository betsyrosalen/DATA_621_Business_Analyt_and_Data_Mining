# DATA PREPARATION

# impute NAs using MICE for all variables with exception of STARS
train_mice <- dplyr::select(train, -STARS) %>% 
  mice::mice(m = 5, 
             maxit = 5, 
             print = FALSE)

# map MICE values back to df
train_imputed <- mice::complete(train_mice) %>% 
  cbind(train$STARS) %>% 
  dplyr::rename(STARS = 'train$STARS')

# impute STARS NAs to 0
# check correlation plot to affirm
train_imputed <- train_imputed %>% 
  mutate_if(is.factor,
            fct_explicit_na,
            na_level = "0")

# confirm no additional NAs
#train_imputed %>% 
  #is.na() %>% 
  #colSums()

density.plot <- densityplot(train_mice)

# subset and rename variables with count values for concentrations / amounts that are < 0 
train_scaling_subset <- train_imputed %>% 
  dplyr::select(Alcohol,
                CitricAcid, 
                FixedAcidity, 
                FreeSulfurDioxide, 
                ResidualSugar, 
                Sulphates, 
                TotalSulfurDioxide, 
                VolatileAcidity) %>% 
  dplyr::rename_all(paste0, '_addscaled')

# function intended to additively scale values by amount equivalent to lower bound of 1.5 * IQR, then drop anything below 0 
# not working as intended
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

# rescale subset of variables with values < 0
train_scaled_subset <- lapply(train_scaling_subset, positive_scale) %>% 
  as.data.frame()

# reattach index for join with other variables
train_scaled_subset <- cbind(train_imputed$`ï..INDEX`, train_scaled_subset)
names(train_scaled_subset)[1] <- 'ï..INDEX'

# combined rescaled variables with original frame, marked as '_addscaled'
train_imputed <- train_imputed %>% 
  left_join(train_scaled_subset, by = 'ï..INDEX')

train_imputed$STARS <- as.factor(train_imputed$STARS)

hist_lt_scaled <- train_imputed %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())


# BUILD MODELS
# ZERO-INFLATED NEGATIVE BINOMIAL MODEL

nb_vars <- c('TARGET',
             'FixedAcidity',
             'VolatileAcidity',
             'CitricAcid',
             'ResidualSugar',
             'Chlorides',
             'FreeSulfurDioxide',
             'TotalSulfurDioxide',
             'Density', 
             'pH', 
             'Sulphates',
             'Alcohol',
             #'LabelAppeal', 
             'AcidIndex' 
)

zinb_vars <- c('TARGET', 
               'Chlorides', 
               'Density', 
               'pH', 
               'Sulphates', 
               #'LabelAppeal', 
               'AcidIndex', 
               #'STARS',
               colnames(train_scaling_subset)
)


# original train data, not imputed or scaled values
# LabelAppeal removed, treat as factor?
mod_nb1 <- glm.nb(formula = TARGET ~ 
                    FixedAcidity +
                    VolatileAcidity +
                    CitricAcid +
                    ResidualSugar +
                    Chlorides +
                    FreeSulfurDioxide +
                    TotalSulfurDioxide +
                    Density +
                    pH +
                    Sulphates +
                    Alcohol +
                    # LabelAppeal +
                    AcidIndex,
                  data = dplyr::select(train, nb_vars))

summary(mod_nb1)


# train imputed data, not scaled values
# LabelAppeal removed, treat as factor?
mod_nb2 <- glm.nb(formula = TARGET ~ ., 
                  data = dplyr::select(train_imputed, zinb_vars))

summary(mod_nb2)


# throws error
mod_zinb <- zeroinfl(formula = TARGET ~ ., 
                     data = dplyr::select(train_imputed, zinb_vars), 
                     dist = 'negbin')


# throws error
mod_zinb_unscaled <- zeroinfl(formula = TARGET ~ ., 
                     data = dplyr::select(train, -STARS), 
                     dist = 'negbin')