
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
# https://cran.r-project.org/web/packages/synthpop/synthpop.pdf
# https://www.r-bloggers.com/generating-synthetic-data-sets-with-synthpop-in-r/
# https://www.geos.ed.ac.uk/homes/graab/synthpop.pdf


syn_obj <- synthpop::syn(data = data, m = 200)  # creates 10 synthetic datasets based on original dataset and its variables distributions
syn_dflist <- syn_obj$syn  # extract list of synthesized data frames from synds object
syn_df <- dplyr::bind_rows(syn_dflist, .id = 'column_label')

# address outliers, treating as NAs and imputing

# nrow(syn_df)
# compare(synth.obj, data)  # visually compare of synthetic datasets vs original data
# syn_csv <- write.syn(syn_obj, 'csv')



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

## Model 1 - logistic regression model (see notes)

#======================================================================================#

## Model 2 - decision tree model (see notes)

#======================================================================================#

## Model 3 - random forest model (see notes)

#======================================================================================#

## Model 4 - support vector machines model (see notes)

#======================================================================================#

## Model 5 - naive bayes model (see notes)

#======================================================================================#


## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

