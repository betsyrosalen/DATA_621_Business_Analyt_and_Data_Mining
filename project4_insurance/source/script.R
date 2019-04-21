# Proj 4
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
train.raw <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance_training_data.csv', 
                   stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance-evaluation-data.csv', 
                 stringsAsFactors = F, header = T)
train.raw <- as.data.table(train.raw)

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

# Quick Look in Data

#str(train)

#table(train$TARGET_FLAG)
#sum(train$TARGET_AMT ==0)

# ------------------------------------------------------------------------------
# Clean Data
## change BLUEBOOK, HOME_VAL, INCOME, OLDCLAIM $ to numerical value
cleanUSD <- function(num) { 
  n <- gsub(",", "", num) # replace , with ""
  n <- as.numeric(gsub("[\\$,]", "", n)) # replace $ with "" 
  return(n) }

train.raw$INCOME <- cleanUSD(train.raw$INCOME)
train.raw$BLUEBOOK <- cleanUSD(train.raw$BLUEBOOK)
train.raw$HOME_VAL <- cleanUSD(train.raw$HOME_VAL)
train.raw$OLDCLAIM <- cleanUSD(train.raw$OLDCLAIM)

test$INCOME <- cleanUSD(test$INCOME)
test$BLUEBOOK <- cleanUSD(test$BLUEBOOK)
test$HOME_VAL <- cleanUSD(test$HOME_VAL)
test$OLDCLAIM <- cleanUSD(test$OLDCLAIM)

# ------------------------------------------------------------------------------

# Summary Statistics
# split.train <- split_columns(train)
# plot_histogram(split.train$continuous)
# plot_bar(split.train$discrete)

train.num <- train.raw[, c('KIDSDRIV', 'AGE', 'HOMEKIDS',
                       'YOJ','INCOME','HOME_VAL', 'TRAVTIME', 'BLUEBOOK',
                       'TIF','OLDCLAIM', 'CLM_FREQ', 'MVR_PTS',
                       'CAR_AGE')]
train.disc <- train.raw[, c('TARGET_FLAG', 'PARENT1', 'SEX', 'MSTATUS',
                         'EDUCATION', 'JOB', 'CAR_TYPE', 'CAR_USE', 
                         'RED_CAR', 'REVOKED', 'URBANICITY')]

summary.stat <- describe(train.num)[,c(2:5,8,9,11,12)]
# ------------------------------------------------------------------------------

# Histogram

train.num.graph <- train.raw[, c('TARGET_FLAG', 'KIDSDRIV', 'AGE', 'HOMEKIDS',
                       'YOJ','INCOME','HOME_VAL', 'TRAVTIME', 'BLUEBOOK',
                       'TIF','OLDCLAIM', 'CLM_FREQ', 'MVR_PTS','CAR_AGE')]

hist.new<- train.num.graph %>%
  gather(-TARGET_FLAG, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(TARGET_FLAG))) +
  geom_histogram(position="dodge", bins=10, alpha=0.5) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual("TARGET_FLAG",values = c("#58BFFF", "#3300FF")) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())
# ------------------------------------------------------------------------------

# BoxPlot

melt.train <- melt(train.num)

outlier.boxplot <- ggplot(melt.train, aes(variable, value)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  coord_flip(ylim = c(0, 110), expand = TRUE) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 110, by = 10)) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("#9900FF", "#3300FF")) +
  theme(panel.background=element_blank(), legend.position="top")

# ------------------------------------------------------------------------------

boxplots.target <- train.num.graph %>%
  gather(-TARGET_FLAG,key = "var", value = "val") %>%
  ggplot(aes(x=factor(TARGET_FLAG), y=val)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  facet_wrap(~ var, scales = "free", ncol=4) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("#9900FF", "#3300FF")) +
  theme(panel.background=element_blank())

# ------------------------------------------------------------------------------

# ggplot(train, aes(HOMEKIDS, TARGET_AMT)) +
#   geom_point() +
#   facet_grid(~HOMEKIDS)
# 
# ggplot(train, aes(INCOME, TARGET_AMT)) +
#   geom_point()
# 
# ggplot(train, aes(REVOKED, TARGET_AMT)) +
#   geom_point()

# ------------------------------------------------------------------------------

# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

train <- train.raw # COPY train data
## change CAR_AGE -3 to 0
train[CAR_AGE == -3, CAR_AGE := 0]

## Change the categorical values into factor and clean
train$TARGET_FLAG <- as.factor(train$TARGET_FLAG)

## change PARENT1 , Yes-> 2 No ->1
train$PARENT1 <- factor(ifelse(train$PARENT1 == "Yes", 2, 1))
names(train)[names(train) == "PARENT1"] <- "NumParents"

## change RED_CAR , yes ->1 no ->0
train$RED_CAR <- factor(ifelse(train$RED_CAR == "yes", 1, 0))

## change SEX into MALE and if M change into 1 and z_F into 0
names(train)[names(train) == "SEX"] <- "MALE"
train$Male <- factor(ifelse(train$MALE == "M", 1, 0))

## REVOKED Yes ->1 No -> 0
train$REVOKED <- factor(ifelse(train$REVOKED == "Yes", 1, 0))

## URBANICITY if URBAN 1, RURAL 0
train$Urban <- ifelse(train$URBANICITY == "Highly Urban/ Urban", 1, 0)

## MSTATUS if single 1, married 0
train$Single <- ifelse(train$MSTATUS == "z_No", 1, 0)

## Car Use if commercial 1, private 0
train$Commercial <- ifelse(train$CAR_USE == 'Commercial', 1, 0)

## RiskAge <-Young, Old -- since very young/old are more risky, we will create a new variable.
#train$RiskAge <- ifelse((train$AGE >= 60) | (train$AGE <= 18), 1, 0)

# Remove Columns
train[ ,c('INDEX', 'MSTATUS', 'CAR_USE', 'URBANICITY')] <- list(NULL)

# Factorize
train$EDUCATION <- as.factor(train$EDUCATION)
train$JOB <- as.factor(train$JOB)
train$CAR_TYPE <- as.factor(train$CAR_TYPE)
train$REVOKED <- as.factor(train$REVOKED)
str(train)

train[train$CAR_AGE == "-3"]$CAR_AGE

#str(train)
# ------------------------------------------------------------------------------
# Missing Values

#table(is.na(train))
#sapply(train, function(x) sum(is.na(x)))

set.seed(123)
impute.data <- mice(train, m = 2, maxit = 2, print = FALSE)

age.med <- median(train$AGE, na.rm = T)
train$AGE[is.na(train$AGE)] <- age.med

train.mice <- mice(train, m = 1, maxit = 1, print = FALSE) 
train <- mice::complete(train.mice)

# ------------------------------------------------------------------------------
# make.dummy <- train[, c('EDUCATION', 'JOB', 'CAR_TYPE')]
# dummies <- fastDummies::dummy_cols(make.dummy)

train.num.a <- train[, c('TARGET_AMT', 'KIDSDRIV', 'AGE', 'HOMEKIDS',
                         'YOJ','INCOME','HOME_VAL', 'TRAVTIME', 'BLUEBOOK',
                         'TIF','OLDCLAIM', 'CLM_FREQ', 'MVR_PTS',
                         'CAR_AGE')]

train.disc.a <- train [, c('TARGET_FLAG', 'NumParents', 'Male',
                           'EDUCATION', 'JOB', 'CAR_TYPE', 'RED_CAR',
                           'REVOKED', 'Urban', 'Single', 'Commercial')]

# ------------------------------------------------------------------------------

## Linearity

linearity <- train %>%
  gather(-TARGET_FLAG, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_FLAG)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET_FLAG") +
  xlab("") +
  theme(panel.background = element_blank())

cor.table <- train.num.a %>%
  dplyr::select(-TARGET_AMT) %>%
  cor() %>%
  round(2) %>%
  corrplot(method = "circle")

## Correlation


# BUILD MODELS<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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

