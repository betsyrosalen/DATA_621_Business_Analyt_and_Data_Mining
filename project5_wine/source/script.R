# Proj 5

# jeremy = negative_binomial
# rose = negative_binomial 
# lidiia = poisson
# betsy = poisson
# gabby = 2 * mlr

# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
train <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project5_wine/data/wine-training-data.csv', 
                   stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project5_wine/data/wine-evaluation-data.csv', 
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
# import script_<nickname>




# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Model Evaluations
