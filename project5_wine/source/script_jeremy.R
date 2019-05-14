# DATA PREPARATION


# 1) Prepare baseline dataframe: train_imputed

# # Impute NAS using MICE for all variables with exception of STARS
# train_mice <- dplyr::select(train, -STARS) %>% 
#   mice::mice(m = 5, 
#              maxit = 5, 
#              print = FALSE)
# 
# # Map MICE values back to df
# train_imputed <- mice::complete(train_mice) %>% 
#   cbind(train$STARS) %>% 
#   dplyr::rename(STARS = 'train$STARS')
# 
# # Impute STARS NAs to 0
# # [JO TO CHECK CORRELATION PLOT TO AFFIRM]
# train_imputed <- train_imputed %>% 
#   mutate_if(is.factor,
#             fct_explicit_na,
#             na_level = "0")

# Double-check to confirm no additional NAs
# train_imputed %>% is.na() %>% colSums()


# 2) Prepare dataframe with negative values shifted to center minimum value at 0: train_minscaled





# 3) Prepare dataframe so negative values are arithmetically scaled from lower bound 
# of IQR*1.5 to 0, and lesser values dropped: train_minscaled

# # Subset variables with values for frequencies / concentrations / amounts that are < 0 
# train_scaling_subset <- train_imputed %>% 
#   dplyr::select(FixedAcidity,
#                 VolatileAcidity,
#                 CitricAcid,
#                 ResidualSugar,
#                 Chlorides,
#                 FreeSulfurDioxide,
#                 TotalSulfurDioxide,
#                 Sulphates)
#   # dplyr::rename_all(paste0, '_scaled')
# 
# # Function to additively scale values by amount equivalent to lower bound of 1.5 * IQR
# # then drop anything below 0 and leaves NAs as they are
# positive_scale <- function(x) {
#   low_bound <- mean(x, na.rm = TRUE) - (stats::IQR(x, na.rm = TRUE) * .5) * 1.5
#   if(is.na(x)) {
#     x = NA
#   } else if(x < low_bound) {
#     x = 0
#   } else {
#     x = x + abs(low_bound) 
#   }
# }
# 
# # Rescale subset of variables with values < 0
# train_iqrscaled_subset <- lapply(train_scaling_subset, 
#                               FUN = function(x) sapply(x, FUN = positive_scale)) %>% 
#   as.data.frame()
# 
# # Join scaled subset back to other variables
# train_plusiqr15 <- train_imputed %>% 
#   dplyr::select(?..INDEX,
#                  TARGET,
#                  Density,
#                  pH,
#                  Alcohol,
#                  LabelAppeal,
#                  AcidIndex,
#                  STARS) %>% 
#   cbind(train_iqrscaled_subset)
# 
# # Rescale discrete label appeal variable and factorize
# train_plusiqr15$LabelAppeal <- train_imputed %>% 
#   select(LabelAppeal) %>% 
#   sapply(FUN = function(x) x + 2) %>% 
#   as.factor()

# levels(train_scaled_subset$LabelAppeal)

# Create histogram of arithmetically scaled variables
hist_lt_scaled <- train_plusiqr15 %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())


# 4) Prepare dataframe taking absolute value of negative values 
# and then log scaling all variables

# # Convert subset of variables to absolute value
# train_scaling_subset2 <- train_imputed %>% 
#   dplyr::select(FixedAcidity,
#                 VolatileAcidity,
#                 CitricAcid,
#                 ResidualSugar,
#                 Chlorides,
#                 FreeSulfurDioxide,
#                 TotalSulfurDioxide,
#                 Sulphates,
#                 Alcohol)
# 
# train_absscaled_subset <- lapply(train_scaling_subset2, 
#                               FUN = function(x) sapply(x, FUN = abs)) %>% 
#                   as.data.frame()
# 
# # lapply(train_absscaled_subset, min)
# 
# # Join absolute value-scaled subset back to other continuous variables
# train_abs <- train_imputed %>% 
#   dplyr::select(Density,
#                  pH,
#                  AcidIndex) %>% 
#   cbind(train_absscaled_subset)
# 
# # Log-scale all continuous variables, adding constant of 1
# train_abslog <- lapply(train_abs, FUN = function(x) 
#                          sapply(x, FUN = function(x) log(x+1))) %>% 
#   as.data.frame()
# 
# # Rescale discrete label appeal variable and factorize
# train_abslog$LabelAppeal <- train_imputed %>% 
#   select(LabelAppeal) %>% 
#   sapply(function(x) x + 2) %>% 
#   as.factor()
# 
# # Map remaining variables to dataframe
# train_abslog$?..INDEX <- train_imputed$?..INDEX
# train_abslog$TARGET <- train_imputed$TARGET
# train_abslog$STARS <- train_imputed$STARS

# Create histogram of log-transformed absolute value of variables
hist_lt_scaled <- train_abslog %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())



# Create density plot for imputed values
density.plot <- densityplot(train_mice)


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
             'LabelAppeal', 
             'AcidIndex' 
)

zinb_vars <- c('TARGET', 
               'Chlorides', 
               'Density', 
               'pH', 
               'Sulphates', 
               'LabelAppeal', 
               'AcidIndex', 
               'STARS',
               colnames(train_scaling_subset)
)


# 1st approach with data = train_imputed
#nb.model.jeremy.1 <- glm.nb(formula = TARGET ~ .,
 #                           data = dplyr::select(train_selected, nb_vars))
#summary(nbmj1 <- MASS::stepAIC(nb.model.jeremy.1))
# AIC = 51493

# 2nd approach with data = train_plus,min
nb.model.jeremy.2 <- glm.nb(formula = TARGET ~ ., 
                  data = train_plusmin)
summary(nbmj2 <- MASS::stepAIC(nb.model.jeremy.2))
# AIC = 45620
# Alcohol, LabelAppeal, AcidIndex, STARS, Volatile Acidity, Chlorides, FreeS02, TotalS02, Sulphates all stat sig
# Coefficients make intuitive sense, with sales drivers like more STARS, followed by LabelAppeal, then more acidity

# 3rd approach with data = train_plusiqr15
nb.model.jeremy.3 <- glm.nb(formula = TARGET ~ ., 
                            data = train_plusiqr15)
summary(nbmj3 <- MASS::stepAIC(nb.model.jeremy.3))
# AIC = 45620
# Same covariates, same coefficients

# 4th approach with data = train_abslog
nb.model.jeremy.4 <- glm.nb(formula = TARGET ~ ., 
                            data = train_abslog)
summary(nbmj4 <- MASS::stepAIC(nb.model.jeremy.4))
# AIC = 46319