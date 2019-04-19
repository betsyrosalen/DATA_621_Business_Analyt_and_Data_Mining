# Proj 4
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
train <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance_training_data.csv', 
                   stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance-evaluation-data.csv', 
                 stringsAsFactors = F, header = T)

variable_descriptions <- rbind(c('TARGET_FLAG','car crash = 1, no car crash = 0','response'),
                               c('TARGET_AMT','car crash cost = >0, no car crash = 0','response'),
                               c('AGE',"driver's age - very young/old tend to be risky",'numerical predictor'),
                               c('BLUEBOOK','$ value of vehicle','numerical predictor'),
                               c('CAR_AGE','age of vehicle','numerical predictor'),
                               c('CAR_TYPE','type of car (6types)','categorical predictor'),
                               c('CAR_USE','usage of car (commercial/private)','categorical predictor'),
                               c('CLM_FREQ','number of claims past 5 years','numerical predictor'),
                               c('EDUCATION','max education level (5types)','categorical predictor'),
                               c('HOMEKIDS','number of children at home','numerical predictor'),
                               c('HOME_VAL','$ value of home - home owners tend to drive more responsibly','numerical predictor'),
                               c('INCOME','$ income - rich people tend to get into fewer crashes','numerical predictor'),
                               c('JOB','job category (8types, 1missing )- white collar jobs tend to be safer','categorical predictor'),
                               c('KIDSDRIV','number of driving children - teenagers likely get into crashes','numerical predictor'),
                               c('MSTATUS','maritial status - married people drive more safely','catogerical predictor'),
                               c('MVR_PTS','number of traffic tickets','numerical predictor'),
                               c('OLDCLAIM','$ total claims in the past 5 years','numerical predictor'),
                               c('PARENT1','single parent','categorical predictor'),
                               c('RED_CAR','a red car','categorical predictor'),
                               c('REVOKED','license revoked (past 7 years) - more risky driver','categorical predictor'),
                               c('SEX','gender - woman may have less crashes than man','categorical predictor'),
                               c('TIF','time in force - number of years being customer','numerical predictor'),
                               c('TRAVTIME','distance to work','numerical predictor'),
                               c('URBANCITY','urban/rural','categorical predictor'),
                               c('YOJ','years on job - the longer they stay more safe','numerical predictor'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

# Clean Data
# Clean data
## change BLUEBOOK, HOME_VAL, INCOME, OLDCLAIM $ to numerical value
cleanUSD <- function(num) { 
  n <- gsub(",", "", num) # replace , with ""
  n <- as.numeric(gsub("[\\$,]", "", n)) # replace $ with "" 
  return(n) }

train$INCOME <- cleanUSD(train$INCOME)
train$BLUEBOOK <- cleanUSD(train$BLUEBOOK)
train$HOME_VAL <- cleanUSD(train$HOME_VAL)
train$OLDCLAIM <- cleanUSD(train$OLDCLAIM)

test$INCOME <- cleanUSD(test$INCOME)
test$BLUEBOOK <- cleanUSD(test$BLUEBOOK)
test$HOME_VAL <- cleanUSD(test$HOME_VAL)
test$OLDCLAIM <- cleanUSD(test$OLDCLAIM)

# ------------------------------------------------------------------------------
# Change some categorical values Yes/No into dummy variables
## change PARENT1 , Yes-> 2 No ->1
train$PARENT1 <- ifelse(train$PARENT1 == "Yes", 2, 1)
names(train)[names(train) == "PARENT1"] <- "NumParents"

## change RED_CAR , yes ->1 no ->0
train$RED_CAR <- ifelse(train$RED_CAR == "yes", 1, 0)

## change SEX into MALE and if M change into 1 and z_F into 0
names(train)[names(train) == "SEX"] <- "MALE"
train$MALE <- ifelse(train$MALE == "M", 1, 0)

## REVOKED Yes ->1 No -> 0
train$REVOKED <- ifelse(train$REVOKED == "Yes", 1, 0)

## Manually create dummy variables
## split URBANICITY to RURAL and URBAN
train$URBAN <- ifelse(train$URBANICITY == "Highly Urban/ Urban", 1, 0)
train$RURAL <- ifelse(train$URBANICITY == "Rural/ Rural", 1, 0)

## MSTATUS into Married, Single
train$Single <- ifelse(train$MSTATUS == "z_No", 1, 0)
train$Married <- ifelse(train$MSTATUS == "Yes", 1, 0)

## Car Use
train$Commercial <- ifelse(train$CAR_USE == 'Commercial', 1, 0)

## RiskAge <-Young, Old
train$RiskAge <- ifelse((train$AGE >= 60) | (train$AGE <= 18), 1, 0)

# Remove Columns
train[ ,c('INDEX', 'MSTATUS', 'CAR_USE', 'URBANICITY')] <- list(NULL)
str(train)
# ------------------------------------------------------------------------------

make.dummy <- train[, c('EDUCATION', 'JOB', 'CAR_TYPE')]
dummies <- fastDummies::dummy_cols(make.dummy)


# Summary Statistics
#summary.stat <- describe(train)

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

