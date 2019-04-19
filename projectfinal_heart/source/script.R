
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
data <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/projectfinal_heart/data/heart.csv', 
                   stringsAsFactors = F, header = T)

variable_descriptions <- rbind(c('var_name','def','type'),
                               c('var_name','def','type'),
                               c('var_name','def','type'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

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

