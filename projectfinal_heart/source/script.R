
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
data <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/projectfinal_heart/data/heart.csv', 
                   stringsAsFactors = F, header = T)

variable_descriptions <- rbind(c('var_name','def','type'),
                               c('var_name','def','type'),
                               c('var_name','def','type'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')


# bootstrap surrogate data using synthpop
# https://cran.r-project.org/web/packages/synthpop/vignettes/synthpop.pdf

# split data into test and train using cross-validation

folds <- 10  # set to 10 provisionally
train_bootstrap <- caret::trainControl(method = 'boot', number = folds)  # bootstrap resampling approach
train_kfold <- caret::trainControl(method = 'cv', number = folds)  # k-fold cross validation
train_kfoldrpt <- caret::trainControl(method = 'repeatedcv', number = folds, repeats = 3)  # k-fold cross validation, provisionally set to 3 repeats but explore setting
train_loocv <- caret::trainControl(method = 'LOOCV')

# Summary Statistics


# Outliers


#  Missing Values
na.barplot <- plot_missing(data)


# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Model 1

#======================================================================================#

## Model 2

#======================================================================================#

## Model 3

#======================================================================================#

## Model 4

#======================================================================================#

## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

