# Proj 5

# jeremy = negative_binomial
# rose = negative_binomial
# lidiia = poisson
# betsy = poisson
# gabby = 2 * mlr

# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

set.seed(500)

# load data
train <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project5_wine/data/wine-training-data.csv',
                   stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project5_wine/data/wine-evaluation-data.csv',
                 stringsAsFactors = F, header = T)

# remove index
train$INDEX <- NULL
test$INDEX <- NULL

vars <- rbind(c('TARGET','Number of Cases Purchased','count response'),
                 c('AcidIndex','Method of testing total acidity by using a weighted avg','continuous numerical predictor'),
                 c('Alcohol','Alcohol Content','continuous numerical predictor'),
                 c('Chlorides','Chloride content of wine','continuous numerical predictor'),
                 c('CitricAcid','Citric Acid Content','continuous numerical predictor'),
                 c('Density','Density of Wine','continuous numerical predictor'),
                 c('FixedAcidity','Fixed Acidity of Wine','continuous numerical predictor'),
                 c('FreeSulfurDioxide','Sulfur Dioxide content of wine','continuous numerical predictor'),
                 c('LabelAppeal','Marketing Score indicating the appeal of label design','categorical predictor'),
                 c('ResidualSugar','Residual Sugar of wine','continuous numerical predictor'),
                 c('STARS','Wine rating by a team of experts. 4 = Excellent, 1 = Poor','categorical predictor'),
                 c('Sulphates','Sulfate conten of wine','continuous numerical predictor'),
                 c('TotalSulfurDioxide','Total Sulfur Dioxide of Wine','continuous numerical predictor'),
                 c('VolatileAcidity','Volatile Acid content of wine','continuous numerical predictor'),
                 c('pH','pH of wine','continuous numerical predictor') )

colnames(vars) <- c('VARIABLE','DEFINITION','TYPE')


# Summary Statistics

train_num <- train[, c( 'AcidIndex','Alcohol', 'Density',
                           'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                           'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity', 'TARGET')]

train_cat <- train[, c('STARS', 'LabelAppeal')]
train_cat$STARS <- as.factor(train_cat$STARS )
train_cat$LabelAppeal <- as.factor(train_cat$LabelAppeal )

train_num_stats <- describe(train_num)[,c(2,8,3,5,9,4)]
train_cat_stats <- summary(train_cat[, c('STARS', 'LabelAppeal')])

# Data Distribution
hist <- train %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())

h.train <- train
h.train$STARS[is.na(h.train$STARS)] <- 0
cbPalette <- c("#58BFFF", "#3300FF", "#E69F00", "#009E73", "#CC79A7")
hist.STARS <- h.train[, c('TARGET', 'STARS')] %>%
  gather(-STARS, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(STARS))) +
  geom_bar(alpha=0.8) +
  facet_wrap(~ STARS, scales = "free") +
  scale_fill_manual("STARS", values = cbPalette) +
  xlab("TARGET") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top")
hist.STARS

hist.LabelAppeal <- h.train[, c('TARGET', 'LabelAppeal')] %>%
  gather(-LabelAppeal, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(LabelAppeal))) +
  geom_bar(alpha=0.8) +
  facet_wrap(~ LabelAppeal, scales = "free") +
  scale_fill_manual("LabelAppeal", values = cbPalette) +
  xlab("TARGET") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top")
hist.LabelAppeal
#c("#58BFFF", "#3300FF")

# Boxplot

scaled.train.num <- as.data.table(scale(train[, c('AcidIndex','Alcohol', 'Density', "LabelAppeal", 'STARS',
                                                  'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                                                  'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity', 'TARGET')]))


melt.train <- melt(scaled.train.num)

scaled_boxplots <- ggplot(melt.train, aes(variable, value)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  coord_flip() +
  #scale_y_continuous(labels = scales::comma,
  #                   breaks = seq(0, 110, by = 10)) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("#9900FF", "#3300FF")) +
  theme(panel.background=element_blank(), legend.position="top")
scaled_boxplots

#Scatter plot between numeric predictors and the TARGET
linearity <- train %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())

#Scatter plot between log transformed predictors and the log transformed TARGET filtered for rows where TARGET is greater than 0
logged_vals <- train[,c('AcidIndex','Alcohol', 'Density',
                           'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                           'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity', 'TARGET')]
logged_vals <- logged_vals %>%
  filter(TARGET>0) %>%
  log()

linearity_log <- logged_vals %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())

#Box Cox
#bc <- train[train[, 'TARGET'] > 0, ]



## Square Root Transformed Predictors and Log transformed Target Linearity Plot
X <- train[train[, 'TARGET']>0,
              c('AcidIndex','Alcohol', 'Density',
                'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity')]
sqroot_vals <- data.table(cbind(log(train[train[, 'TARGET']>0,'TARGET']),
                                   sapply(X, sqrt)))

colnames(sqroot_vals)[1] <- 'TARGET'

linearity_root_2 <- sqroot_vals %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())


# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# STARS NA<- 0
train$STARS[is.na(train$STARS)] <- 0
test$STARS[is.na(test$STARS)] <- 0
test$STARS <- as.factor(test$STARS)
test$LabelAppeal <- as.factor(test$LabelAppeal)


# DATA SET TO USE::
## |--------------------------------------------------------------
## | 1. Data as is :                    `train_imputed`
## | 2. Data by shifted by min value:  `train_plusmin`
## | 3. Data by Jeremy's method:       `train_plusiqr15`
## | 4. Data by ABS and Log:            `train_abslog`
## |--------------------------------------------------------------


# We create 4 dataset.
##1. Data as is : train_imputed
##2. Data by shifted by min value:
##3. Data by Jeremy's method
##4. Data by ABS and Log

#----------------------------------------------------------------------------------------
##1. Data as is : train_imputed
# Then we run imputation
# impute NAs using MICE for all variables with exception of STARS
train_mice <- mice::mice(train, m = 2, method='cart', maxit = 2, print = FALSE)
train_imputed <- mice::complete(train_mice)

train_imputed_raw <- train_imputed

train_imputed$STARS <- as.factor(train_imputed$STARS)
train_imputed$LabelAppeal <- as.factor(train_imputed$LabelAppeal)
#----------------------------------------------------------------------------------------
##2. Data by shifted by min value:

train_plusmin <- train_imputed_raw

# list of columns that will be transformed
cols <- c("FixedAcidity","VolatileAcidity",
          "CitricAcid","ResidualSugar",
          "Chlorides","FreeSulfurDioxide",
          "TotalSulfurDioxide","Sulphates","Alcohol")

# Transformation of train_plusmin by adding the minimum value plus one
for (col in cols) {
  train_plusmin[, col] <- train_plusmin[, col] + abs(min(train_plusmin[, col])) + 1
}

train_plusmin$STARS <- as.factor(train_plusmin$STARS)
train_plusmin$LabelAppeal <- as.factor(train_plusmin$LabelAppeal)
#----------------------------------------------------------------------------------------
##3. Data by Jeremy's method
# arithmetically scaled from lower bound of IQR*1.5 to 0, and lesser values dropped: train_minscaled
# Subset variables with values for frequencies / concentrations / amounts that are < 0
train_scaling_subset <- train_imputed_raw %>%
  dplyr::select(FixedAcidity,
                VolatileAcidity,
                CitricAcid,
                ResidualSugar,
                Chlorides,
                FreeSulfurDioxide,
                TotalSulfurDioxide,
                Sulphates)
# dplyr::rename_all(paste0, '_scaled')

# Function to additively scale values by amount equivalent to lower bound of 1.5 * IQR
# then drop anything below 0 and leaves NAs as they are
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

# Rescale subset of variables with values < 0
train_iqrscaled_subset <- lapply(train_scaling_subset,
                                 FUN = function(x) sapply(x, FUN = positive_scale)) %>%
  as.data.frame()

# Join scaled subset back to other variables
train_plusiqr15 <- train_imputed_raw %>%
  dplyr::select(TARGET,
                Density,
                pH,
                Alcohol,
                LabelAppeal,
                AcidIndex,
                STARS) %>%
  cbind(train_iqrscaled_subset)

# Rescale discrete label appeal variable and factorize
train_plusiqr15$LabelAppeal <- train_imputed_raw %>%
  select(LabelAppeal) %>%
  sapply(FUN = function(x) x + 2) %>%
  as.factor()

train_plusiqr15$STARS <- as.factor(train_plusiqr15$STARS)
#----------------------------------------------------------------------------------------
##4. Data by ABS and Log

# Convert subset of variables to absolute value
train_scaling_subset2 <- train_imputed_raw %>%
  dplyr::select(FixedAcidity,
                VolatileAcidity,
                CitricAcid,
                ResidualSugar,
                Chlorides,
                FreeSulfurDioxide,
                TotalSulfurDioxide,
                Sulphates,
                Alcohol)

train_absscaled_subset <- lapply(train_scaling_subset2,
                                 FUN = function(x) sapply(x, FUN = abs)) %>%
  as.data.frame()

# lapply(train_absscaled_subset, min)

# Join absolute value-scaled subset back to other continuous variables
train_abs <- train_imputed_raw %>%
  dplyr::select(Density,
                pH,
                AcidIndex) %>%
  cbind(train_absscaled_subset)

# Log-scale all continuous variables, adding constant of 1
train_abslog <- lapply(train_abs, FUN = function(x)
  sapply(x, FUN = function(x) log(x+1))) %>%
  as.data.frame()

# Rescale discrete label appeal variable and factorize
train_abslog$LabelAppeal <- train_imputed_raw %>%
  select(LabelAppeal) %>%
  sapply(function(x) x + 2) %>%
  as.factor()

# Map remaining variables to dataframe
#train_abslog$INDEX <- train_imputed$INDEX
train_abslog$TARGET <- train_imputed_raw$TARGET
train_abslog$STARS <- train_imputed_raw$STARS
train_abslog$STARS <- as.factor(train_abslog$STARS)

# DATA SET TO USE::
## |--------------------------------------------------------------
## | 1. Data as is :                    `train_imputed`
## | 2. Data by shifted by min value:  `train_plusmin`
## | 3. Data by Jeremy's method:       `train_plusiqr15`
## | 4. Data by ABS and Log:            `train_abslog`
## |--------------------------------------------------------------

str(train_imputed)
str(train_plusmin)
str(train_plusiqr15)
str(train_abslog)

# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# import script_<nickname>
# PLEASE COMPLETE THIS IN YOUR OWN SCRIPT FILE USING THE 4 datasets above.



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Model Evaluations
