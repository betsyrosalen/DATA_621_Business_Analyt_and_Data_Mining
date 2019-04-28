# Proj 4
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

if (!require('car')) (install.packages('car'))
if (!require('caret')) (install.packages('caret'))
if (!require('corrplot')) (install.packages('corrplot'))
if (!require('data.table')) (install.packages('data.table'))
if (!require('dplyr')) (install.packages('dplyr'))
if (!require('DataExplorer')) (install.packages('DataExplorer'))
if (!require('faraway')) (install.packages('faraway'))
#if (!require('fastDummies')) (install.packages('fastDummies'))
if (!require('gridExtra')) (install.packages('gridExtra'))
if (!require('ggplot2')) (install.packages('ggplot2'))
if (!require('GGally')) (install.packages('GGally'))
if (!require('huxtable')) (install.packages('huxtable'))
if (!require('jtools')) (install.packages('jtools'))
if (!require('kableExtra')) (install.packages('kableExtra'))
if (!require('MASS')) (install.packages('MASS'))
if (!require('mice')) (install.packages('mice'))
if (!require('psych')) (install.packages('psych'))
if (!require('pROC')) (install.packages('pROC'))
if (!require('pscl')) (install.packages('pscl'))
if (!require('tidyverse')) (install.packages('tidyverse'))
if (!require('tidyr')) (install.packages('tidyr'))


# load data
train.raw <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance_training_data.csv',
                   stringsAsFactors = T, header = T)
test <- read.csv('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/project4_insurance/data/insurance-evaluation-data.csv',
                 stringsAsFactors = T, header = T)
train.raw <- as.data.table(within(train.raw, rm('INDEX')))

vars <- rbind(c('TARGET_FLAG','car crash = 1, no car crash = 0','binary categorical response'),
               c('TARGET_AMT','car crash cost = >0, no car crash = 0','continuous numerical response'),
               c('AGE',"driver's age - very young/old tend to be risky",'continuous numerical predictor'),
               c('BLUEBOOK','$ value of vehicle','continuous numerical predictor'),
               c('CAR_AGE','age of vehicle','continuous numerical predictor'),
               c('CAR_TYPE','type of car (6types)','categorical predictor'),
               c('CAR_USE','usage of car (commercial/private)','binary categorical predictor'),
               c('CLM_FREQ','number of claims past 5 years','discrete numerical predictor'),
               c('EDUCATION','max education level (5types)','categorical predictor'),
               c('HOMEKIDS','number of children at home','discrete numerical predictor'),
               c('HOME_VAL','$ home value - home owners tend to drive more responsibly','continuous numerical predictor'),
               c('INCOME','$ income - rich people tend to get into fewer crashes','continuous numerical predictor'),
               c('JOB','job category (8types, 1missing) - white collar tend to be safer','categorical predictor'),
               c('KIDSDRIV','number of driving children - teenagers more likely to crash','discrete numerical predictor'),
               c('MSTATUS','maritial status - married people drive more safely','catogerical predictor'),
               c('MVR_PTS','number of traffic tickets','continuous numerical predictor'),
               c('OLDCLAIM','$ total claims in the past 5 years','continuous numerical predictor'),
               c('PARENT1','single parent','binary categorical predictor'),
               c('RED_CAR','a red car','binary categorical predictor'),
               c('REVOKED','license revoked (past 7 years) - more risky driver','binary categorical predictor'),
               c('SEX','gender - woman may have less crashes than man','binary categorical predictor'),
               c('TIF','time in force - number of years being customer','continuous numerical predictor'),
               c('TRAVTIME','distance to work','continuous numerical predictor'),
               c('URBANCITY','urban/rural','binary categorical predictor'),
               c('YOJ','years on job - the longer they stay more safe','continuous numerical predictor'))

colnames(vars) <- c('VARIABLE','DEFINITION','TYPE')

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

# Convert 'CLM_FREQ','HOMEKIDS', and 'KIDSDRIV' to Factors
train.raw[, c('CLM_FREQ','HOMEKIDS','KIDSDRIV')] <- lapply(train.raw[, c('CLM_FREQ','HOMEKIDS','KIDSDRIV')], as.factor)
test[, c('CLM_FREQ','HOMEKIDS','KIDSDRIV')] <- lapply(test[, c('CLM_FREQ','HOMEKIDS','KIDSDRIV')], as.factor)

# Fix factor levels
levels(train.raw$URBANICITY) <- list(Urban="Highly Urban/ Urban", Rural="z_Highly Rural/ Rural")
levels(test$URBANICITY) <- list(Urban="Highly Urban/ Urban", Rural="z_Highly Rural/ Rural")

cleanLEVELS <- function(level) {
    l <- gsub("z_", "", levels(level)) # replace z_ with ""l
    return(l) }

levels(train.raw$EDUCATION) <- levels(train.raw$EDUCATION)
levels(test$EDUCATION) <- levels(test$EDUCATION)
levels(train.raw$JOB) <- levels(train.raw$JOB)
levels(test$JOB) <- levels(test$JOB)
levels(train.raw$CAR_TYPE) <- cleanLEVELS(train.raw$CAR_TYPE)
levels(test$CAR_TYPE) <- cleanLEVELS(test$CAR_TYPE)
levels(train.raw$SEX) <- cleanLEVELS(train.raw$SEX)
levels(test$SEX) <- cleanLEVELS(test$SEX)
levels(train.raw$MSTATUS) <- cleanLEVELS(train.raw$MSTATUS)
levels(test$MSTATUS) <- cleanLEVELS(test$MSTATUS)

## change CAR_AGE -3 to 0
train.raw[CAR_AGE == -3, CAR_AGE := 0]

# ------------------------------------------------------------------------------

# Summary Statistics

train.num <- train.raw[, c('TARGET_AMT', 'AGE', 'YOJ','INCOME','HOME_VAL',
                           'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                           'CAR_AGE')]
train.cat <- train.raw[, c('TARGET_FLAG', 'PARENT1', 'SEX', 'MSTATUS', 'EDUCATION',
                           'JOB', 'CAR_TYPE', 'CAR_USE', 'RED_CAR', 'REVOKED',
                           'URBANICITY', 'KIDSDRIV', 'HOMEKIDS', 'CLM_FREQ')]

summary.stat.num <- describe(train.num)[,c(2,8,3,5,9,4)]

summary.stat.cat <- describe(train.cat)[,c(2,8,3,5,9,4)]

summary.num <- summary(train.num)

summary.cat1 <- summary(train.cat[, c('EDUCATION', 'JOB', 'CAR_TYPE', 'KIDSDRIV', 'HOMEKIDS', 'CLM_FREQ')])
summary.cat2 <- summary(train.cat[, c('PARENT1', 'SEX', 'MSTATUS', 'CAR_USE', 'RED_CAR', 'REVOKED', 'URBANICITY')])


# ------------------------------------------------------------------------------

# Histograms

train.num.graph <- train.raw[, c('TARGET_FLAG', 'AGE', 'YOJ','INCOME','HOME_VAL',
                                 'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                                 'CAR_AGE')]

hist.num <- train.num.graph %>%
    gather(-TARGET_FLAG, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(TARGET_FLAG))) +
    geom_histogram(position="dodge", bins=10, alpha=0.5) +
    facet_wrap(~ var, scales = "free") +
    scale_fill_manual("TARGET_FLAG",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank())

bar.cat <- train.cat %>%
    gather(-TARGET_FLAG, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(TARGET_FLAG))) +
    geom_bar(position="dodge", alpha=0.5) +
    facet_wrap(~ var, scales = "free") +
    scale_fill_manual("TARGET_FLAG",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ------------------------------------------------------------------------------

# BoxPlot

melt.train <- melt(train.num)

outlier.boxplots <- ggplot(melt.train, aes(variable, value)) +
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

# Scaled BoxPlots
scaled.train.num <- as.data.table(scale(train.num[, c('AGE', 'YOJ','INCOME','HOME_VAL',
                                                      'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                                                      'CAR_AGE')]))
melt.train <- melt(scaled.train.num)

scaled.boxplots <- ggplot(melt.train, aes(variable, value)) +
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


## Linearity
linearity <- train.raw[,-1] %>%
    select_if(is.numeric) %>%
    filter(TARGET_AMT>0) %>%
    gather(-TARGET_AMT, key = "var", value = "value") %>%
    ggplot(aes(x = value, y = TARGET_AMT)) +
    geom_point(alpha=0.1) +
    stat_smooth() +
    facet_wrap(~ var, scales = "free", ncol=3) +
    ylab("TARGET_AMT") +
    xlab("") +
    theme(panel.background = element_blank())

## Linearity
linearity.log <- train.raw[,-1] %>%
    select_if(is.numeric) %>%
    filter(TARGET_AMT>0) %>%
    log() %>%
    gather(-TARGET_AMT, key = "var", value = "value") %>%
    ggplot(aes(x = value, y = TARGET_AMT)) +
    geom_point(alpha=0.1) +
    stat_smooth() +
    facet_wrap(~ var, scales = "free", ncol=3) +
    ylab("TARGET_AMT") +
    xlab("") +
    theme(panel.background = element_blank())

# ------------------------------------------------------------------------------

# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# ------------------------------------------------------------------------------
# Missing Values

#table(is.na(train))
#sapply(train, function(x) sum(is.na(x)))

train <- data.table(train.raw)

set.seed(123)
impute.data <- mice(train, m = 2, maxit = 2, print = FALSE)

age.med <- median(train$AGE, na.rm = T)
train$AGE[is.na(train$AGE)] <- age.med

train.mice <- mice(train, m = 1, maxit = 1, print = FALSE)
train <- mice::complete(train.mice)

density.plot <- densityplot(impute.data)

# ------------------------------------------------------------------------------
# make.dummy <- train[, c('EDUCATION', 'JOB', 'CAR_TYPE')]
# dummies <- fastDummies::dummy_cols(make.dummy)

# Divide numeric/categorical data AFTER imputing data

train.num.a <- train[, c('TARGET_FLAG', 'TARGET_AMT', 'AGE', 'YOJ','INCOME','HOME_VAL',
                           'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                           'CAR_AGE')]

train.cat.a <- train[, c('TARGET_FLAG', 'PARENT1', 'SEX', 'MSTATUS', 'EDUCATION',
                           'JOB', 'CAR_TYPE', 'CAR_USE', 'RED_CAR', 'REVOKED',
                           'URBANICITY', 'KIDSDRIV', 'HOMEKIDS', 'CLM_FREQ')]

# ------------------------------------------------------------------------------

## Correlation

corr.table <- ggpairs(train.num.a %>% dplyr::select(-c(TARGET_AMT, TARGET_FLAG)))

plot.data <- train.num.a
plot.data$TARGET_FLAG <- factor(plot.data$TARGET_FLAG)
corr.plot2 <- plot.data %>% dplyr::select(-TARGET_AMT) %>%
    ggscatmat(color="TARGET_FLAG", alpha=0.1) +
    scale_color_manual(values=c("#58BFFF", "#3300FF")) +
    theme(panel.background=element_blank(), legend.position="top",
          axis.text.x = element_text(angle=-40, vjust=1, hjust=0))

# correl <- ggpairs(train)
# This plot doesn't work in the script file.  Moved code to our .Rmd file
# The code works to create  correlation table though!
corr.train <- train.num.a %>%
  dplyr::select(-TARGET_FLAG) %>%
  dplyr::select(-TARGET_AMT) %>%
  cor() %>%
  round(2) %>%
  corrplot(method = "circle")

corr.plot <- ggcorrplot::ggcorrplot(corr.train,
                                    type = 'lower',
                                    lab=T,
                                    lab_size=2)

#pairs.plot <- pairs(train.num.a, col=train.num.a$TARGET_FLAG)

# BUILD MODELS<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Model 1

model.1 <- train(TARGET_FLAG ~ .,
                 data=train,
                 method='glm',
                 family='binomial',
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod.1 <- glm(TARGET_FLAG~.,
             family='binomial',
             data = train.cat.a)

#======================================================================================#

## Model 2

model.2 <- train(TARGET_FLAG ~ KIDSDRIV+ AGE+ HOMEKIDS +
                   YOJ+INCOME+HOME_VAL+ TRAVTIME+ BLUEBOOK+
                   TIF+OLDCLAIM+ CLM_FREQ+ MVR_PTS+ CAR_AGE +
                   PARENT1+ SEX+ EDUCATION+ JOB+ CAR_TYPE+
                   REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
                 data=train,
                 method='glm',
                 family='binomial',
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod.2 <- glm(TARGET_FLAG~KIDSDRIV+ AGE+ HOMEKIDS +
               YOJ+INCOME+HOME_VAL+ TRAVTIME+ BLUEBOOK+
               TIF+OLDCLAIM+ CLM_FREQ+ MVR_PTS+ CAR_AGE +
               PARENT1+ SEX+ EDUCATION+ JOB+ CAR_TYPE+
               REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
             family='binomial',
             data = train)

#======================================================================================#

## Model 3

model.3 <- train(TARGET_FLAG ~ KIDSDRIV+ HOMEKIDS +
                   YOJ+INCOME+HOME_VAL+ TRAVTIME+ BLUEBOOK+
                   TIF+OLDCLAIM+ CLM_FREQ+ MVR_PTS+
                   PARENT1+ EDUCATION+ JOB+ CAR_TYPE+
                   REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
                 data=train,
                 method='glm',
                 family='binomial',
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod.3 <- glm(TARGET_FLAG ~ KIDSDRIV+ HOMEKIDS +
               YOJ+INCOME+HOME_VAL+ TRAVTIME+ BLUEBOOK+
               TIF+OLDCLAIM+ CLM_FREQ+ MVR_PTS+
               PARENT1+ EDUCATION+ JOB+ CAR_TYPE+
               REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
             family='binomial',
             data = train)

#======================================================================================#

## Model 4

mod.4.raw <- glm(TARGET_FLAG~ KIDSDRIV+ log(AGE)+ AGE +  HOMEKIDS +
               YOJ + log(INCOME+0.00000000000001)+ INCOME + HOME_VAL+ log(TRAVTIME)+ TRAVTIME+ log(BLUEBOOK)+ BLUEBOOK +
               TIF+log(OLDCLAIM+0.00000000000001)+ OLDCLAIM + CLM_FREQ+ MVR_PTS+ CAR_AGE +
               PARENT1+ SEX+ EDUCATION+ JOB+ CAR_TYPE+
               REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
             family='binomial',
             data = na.omit(train))
backward.mod.4 <- step(mod.4.raw, direction = "backward", trace=FALSE)

model.4 <- glm(formula = TARGET_FLAG ~ KIDSDRIV + log(AGE) + YOJ +
      log(INCOME + 1e-14) + HOME_VAL + log(TRAVTIME) + log(BLUEBOOK) +
      TIF + log(OLDCLAIM + 1e-14) + MVR_PTS + PARENT1 +
      EDUCATION + JOB + CAR_TYPE + REVOKED + URBANICITY + MSTATUS + CAR_USE,
    family = "binomial", data = na.omit(train))

mod4_summary <- summ(model.4, vifs = TRUE)
#======================================================================================#

## Model 5


train_5 <- filter(train, TARGET_AMT > 0)
mod.5.raw <- lm(TARGET_FLAG~ KIDSDRIV+ log(AGE)+ AGE +  HOMEKIDS +
                   YOJ + log(INCOME+0.00000000000001)+ INCOME + HOME_VAL+ log(TRAVTIME)+ TRAVTIME+ log(BLUEBOOK)+ BLUEBOOK +
                   TIF+log(OLDCLAIM+0.00000000000001)+ OLDCLAIM + CLM_FREQ+ MVR_PTS+ CAR_AGE +
                   PARENT1+ SEX+ EDUCATION+ JOB+ CAR_TYPE+
                   REVOKED+ URBANICITY+ MSTATUS+ CAR_USE, data = na.omit(train_5))
backward.mod.5 <- step(mod.5.raw, direction = "backward", trace=FALSE)
mod5_summary <- summ(backward.mod.5)

#======================================================================================#

## Model 6
mod.6.raw <- lm(TARGET_FLAG~ KIDSDRIV+ log(AGE)+ AGE +  HOMEKIDS +
                  YOJ + log(INCOME+0.00000000000001)+ INCOME + HOME_VAL+ log(TRAVTIME)+ TRAVTIME+ log(BLUEBOOK)+ BLUEBOOK +
                  TIF+log(OLDCLAIM+0.00000000000001)+ OLDCLAIM + CLM_FREQ+ MVR_PTS+ CAR_AGE +
                  PARENT1+ SEX+ EDUCATION+ JOB+ CAR_TYPE+
                  REVOKED+ URBANICITY+ MSTATUS+ CAR_USE, data = na.omit(train))
backward.mod.6 <- step(mod.6.raw, direction = "backward", trace=FALSE)
mod6_summary <- summ(backward.mod.6)


#======================================================================================#


## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

