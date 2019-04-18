
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
train <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance_training_data.csv', 
                   stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance-evaluation-data.csv', 
                 stringsAsFactors = F, header = T)


variable_descriptions <- rbind(c('var_name','def','type'),
                               c('var_name','def','type'),
                               c('var_name','def','type'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

# Summary Statistics


# Outliers


#  Missing Values
na.barplot <- plot_missing(train)


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

