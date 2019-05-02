# Proj 4
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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

levels(train.raw$EDUCATION) <- cleanLEVELS(train.raw$EDUCATION)
levels(test$EDUCATION) <- cleanLEVELS(test$EDUCATION)
levels(train.raw$JOB) <- cleanLEVELS(train.raw$JOB)
levels(test$JOB) <- cleanLEVELS(test$JOB)
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

train.num.graph <- train.raw[, c('TARGET_FLAG', 'TARGET_AMT', 'AGE', 'YOJ','INCOME','HOME_VAL',
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
    theme(panel.background = element_blank(), legend.position="top")

bar.cat <- train.cat %>%
    gather(-TARGET_FLAG, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(TARGET_FLAG))) +
    geom_bar(position="dodge", alpha=0.5) +
    facet_wrap(~ var, scales = "free") +
    scale_fill_manual("TARGET_FLAG",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank(), legend.position="top") +
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

## Log Transformed Linearity
logged_vals <- train.raw[,c('TARGET_AMT', 'INCOME','HOME_VAL',
                            'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                            'CAR_AGE')] + 1
logged_vals <- logged_vals %>%
    filter(TARGET_AMT>1) %>%
    log()

linearity.log <- logged_vals %>%
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
train <- train%>%
  filter(CAR_AGE >= 0)
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

# Does imputed data show linearity?

## Linearity Plot
linearity.new <- train.num.a[,-1] %>%
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

## Log Transformed Linearity Plot
logged_vals <- train.num.a[,c('TARGET_AMT', 'AGE', 'YOJ','INCOME','HOME_VAL',
                            'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                            'CAR_AGE')] + 1
logged_vals <- logged_vals %>%
    filter(TARGET_AMT>1) %>%
    log()

linearity.log.new <- logged_vals %>%
    gather(-TARGET_AMT, key = "var", value = "value") %>%
    ggplot(aes(x = value, y = TARGET_AMT)) +
    geom_point(alpha=0.1) +
    stat_smooth() +
    facet_wrap(~ var, scales = "free", ncol=3) +
    ylab("TARGET_AMT") +
    xlab("") +
    theme(panel.background = element_blank())

# Box-Cox
bc <- train.num.a[train.num.a[, 'TARGET_AMT'] > 0, ]
# Code below added to .Rmd file
#bc_plot <- boxcox(TARGET_AMT~., data=test, lambda=seq(-0.2,0.2,by=0.1))

# Does square root transformation show linearity?

## Square Root Transformed Predictors and Log transformed Target Linearity Plot
X <- train.num.a[train.num.a[, 'TARGET_AMT']>0,
                 c('AGE', 'YOJ','INCOME','HOME_VAL',
                    'TRAVTIME', 'BLUEBOOK', 'TIF','OLDCLAIM', 'MVR_PTS',
                    'CAR_AGE')]
sqroot_vals <- data.table(cbind(log(train.num.a[train.num.a[, 'TARGET_AMT']>0,'TARGET_AMT']),
                     sapply(X, sqrt)))
colnames(sqroot_vals)[1] <- 'TARGET_AMT'

linearity.root <- sqroot_vals %>%
    gather(-TARGET_AMT, key = "var", value = "value") %>%
    ggplot(aes(x = value, y = TARGET_AMT)) +
    geom_point(alpha=0.1) +
    stat_smooth() +
    facet_wrap(~ var, scales = "free", ncol=3) +
    ylab("TARGET_AMT") +
    xlab("") +
    theme(panel.background = element_blank())

## Correlation

#corr.table <- ggpairs(train.num.a %>% dplyr::select(-c(TARGET_AMT, TARGET_FLAG)))

plot.data <- train.num.a
plot.data$TARGET_FLAG <- factor(plot.data$TARGET_FLAG)
corr.plot2 <- plot.data %>% # dplyr::select(-TARGET_AMT) %>%
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

model.1 <- train(TARGET_FLAG ~ PARENT1 + SEX + MSTATUS + EDUCATION + JOB + CAR_TYPE +
                     CAR_USE + REVOKED + URBANICITY + KIDSDRIV + HOMEKIDS + CLM_FREQ,
                 data=train,
                 method='glm',
                 family='binomial',
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod.1 <- glm(TARGET_FLAG ~ PARENT1 + SEX + MSTATUS + EDUCATION + JOB + CAR_TYPE +
                 CAR_USE + REVOKED + URBANICITY + KIDSDRIV + HOMEKIDS + CLM_FREQ,
             family='binomial',
             data = train.cat.a)

mod1_summary <- summ(mod.1, vifs = TRUE)

# Code below added to .Rmd file
#mod1_plot <- par(mfrow=c(2,2)); plot(mod.1)

### Model 1 Summary Statistics
pred.1.raw <- predict(mod.1, newdata = train)
pred.1 <- as.factor(ifelse(pred.1.raw < .5, 0, 1))
mod1.conf.mat <- confusionMatrix(pred.1, as.factor(train$TARGET_FLAG), mode = "everything")


#======================================================================================#

## Model 2 REMOVED from .Rmd file

#model.2 <- train(TARGET_FLAG ~ PARENT1 + MSTATUS + EDUCATION + JOB + CAR_TYPE +
#                     CAR_USE + REVOKED + URBANICITY + KIDSDRIV + HOMEKIDS +
#                     CLM_FREQ + YOJ + INCOME + HOME_VAL + TRAVTIME + BLUEBOOK +
#                     TIF + OLDCLAIM + MVR_PTS,
#                 data=train,
#                 method='glm',
#                 family='binomial',
#                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd
#
#mod.2 <- glm(TARGET_FLAG ~ PARENT1 + MSTATUS + EDUCATION + JOB + CAR_TYPE +
#                     CAR_USE + REVOKED + URBANICITY + KIDSDRIV + HOMEKIDS +
#                     CLM_FREQ + YOJ + INCOME + HOME_VAL + TRAVTIME + BLUEBOOK +
#                     TIF + OLDCLAIM + MVR_PTS,
#             family='binomial',
#             data = train)
#
#mod2_summary <- summ(mod.2, vifs = TRUE)

# Code below added to .Rmd file
#mod2_plot <- par(mfrow=c(2,2)); plot(mod.2)

### Model 2 Summary Statistics
#pred.2.raw <- predict(mod.2, newdata = train)
#pred.2 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
#mod2.conf.mat <- confusionMatrix(pred.2, as.factor(train$TARGET_FLAG), mode = "everything")


#======================================================================================#

## Model 2 (USED TO BE 3)

train$EDUCATION_Bachelors <- train$EDUCATION == "Bachelors"
train$JOB_Manager <- train$JOB == "Manager"
train$JOB_Clerical <- train$JOB == "Clerical"

model.3 <- train(TARGET_FLAG ~ MSTATUS + EDUCATION_Bachelors + JOB_Clerical +
                    JOB_Manager + CAR_TYPE + CAR_USE + REVOKED + URBANICITY +
                    KIDSDRIV + HOMEKIDS + CLM_FREQ + INCOME + HOME_VAL +
                    TRAVTIME + BLUEBOOK + TIF + OLDCLAIM + MVR_PTS,
                 data=train,
                 method='glm',
                 family='binomial',
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod.3 <- glm(TARGET_FLAG ~ MSTATUS + EDUCATION_Bachelors + JOB_Clerical +
                    JOB_Manager + CAR_TYPE + CAR_USE + REVOKED + URBANICITY +
                    KIDSDRIV + HOMEKIDS + CLM_FREQ + INCOME + HOME_VAL +
                    TRAVTIME + BLUEBOOK + TIF + OLDCLAIM + MVR_PTS,
             family='binomial',
             data = train)

mod3_summary <- summ(mod.3, vifs = TRUE)

# Code below added to .Rmd file
#mod3_plot <- par(mfrow=c(2,2)); plot(mod.3)

### Model 3 Summary Statistics
pred.3.raw <- predict(mod.3, newdata = train)
pred.3 <- as.factor(ifelse(pred.3.raw < .5, 0, 1))
mod3.conf.mat <- confusionMatrix(pred.3, as.factor(train$TARGET_FLAG), mode = "everything")


#======================================================================================#

## Model 3 (USED TO BE 4)

mod.4.raw <- glm(TARGET_FLAG ~ MSTATUS + EDUCATION_Bachelors + JOB_Clerical +
                    JOB_Manager + CAR_TYPE + CAR_USE + REVOKED + URBANICITY +
                    KIDSDRIV + HOMEKIDS + CLM_FREQ + BLUEBOOK + CAR_AGE +
                    HOME_VAL + INCOME + MVR_PTS + OLDCLAIM + TIF + TRAVTIME +
                    log(BLUEBOOK) + log(CAR_AGE+1) + log(HOME_VAL+1) +
                    log(INCOME+1) + log(MVR_PTS+1) + log(OLDCLAIM+1) + log(TIF) +
                    log(TRAVTIME),
             family='binomial',
             data = na.omit(train))

backward.mod.4 <- step(mod.4.raw, direction = "backward", trace=FALSE)

mod.4 <- glm(TARGET_FLAG ~ MSTATUS + EDUCATION_Bachelors + JOB_Clerical +
                 JOB_Manager + CAR_TYPE + CAR_USE + REVOKED + URBANICITY +
                 KIDSDRIV + HOMEKIDS + CAR_AGE + HOME_VAL + INCOME + MVR_PTS +
                 OLDCLAIM + log(BLUEBOOK) + log(INCOME+1) +
                 log(OLDCLAIM+1) + log(TIF) + log(TRAVTIME),
            family = "binomial", data = na.omit(train))

mod4_summary <- summ(mod.4, vifs = TRUE)

# Code below added to .Rmd file
#mod4_plot <- par(mfrow=c(2,2)); plot(mod.4)

### Model 4 Summary Statistics
pred.4.raw <- predict(mod.4, newdata = train)
pred.4 <- as.factor(ifelse(pred.4.raw < .5, 0, 1))
mod4.conf.mat <- confusionMatrix(pred.4, as.factor(train$TARGET_FLAG), mode = "everything")

#======================================================================================#

## Model 4 (USED TO BE 5)

train_5 <- train%>%
  filter(TARGET_FLAG == 1) %>%
  filter(TARGET_AMT<45000) %>%
  filter(CAR_AGE >= 0)
train_5$mileage <- train_5$TRAVTIME*(train_5$CAR_AGE+0.0000000000000000000000001)*440.0

model.5 <- lm(TARGET_AMT~ KIDSDRIV + log(AGE)+ AGE +  HOMEKIDS +
                YOJ  + log(INCOME+0.00000000000001)+INCOME + CAR_AGE +log(mileage)+  log(BLUEBOOK)+ BLUEBOOK +
                TIF+log(OLDCLAIM+0.00000000000001)+ OLDCLAIM + CLM_FREQ+ MVR_PTS+ CAR_AGE +
                PARENT1+ SEX+ EDUCATION_Bachelors + JOB_Clerical + JOB_Manager +
                CAR_TYPE+ REVOKED+ URBANICITY+ MSTATUS+ CAR_USE, data =na.omit(train_5))

mod.5 <- step(model.5, direction = "forward", trace=FALSE)

mod5_summary <- summ(mod.5, vifs = TRUE)

mod5_plot <- autoplot(mod.5, which = 1:6, colour = "#58BFFF",
                        smooth.colour = 'red', smooth.linetype = 'solid',
                        ad.colour = 'black',
                        label.size = 3, label.n = 5, label.colour = "#3300FF",
                        ncol = 2) +
                theme(panel.background=element_blank())

### Model 5 Predictions
pred.5.raw <- predict(mod.5, newdata = train_5)

#======================================================================================#

## Model 5 (USED TO BE 6)

train_6 <- train %>%
  filter(TARGET_AMT < 45000)

train_6$mileage <- train_6$TRAVTIME*(train_6$CAR_AGE+0.00000000001)*440

model.6.raw <- lm(TARGET_AMT~ TARGET_FLAG + KIDSDRIV + log(AGE) + AGE +  HOMEKIDS +
                    YOJ + log(INCOME+1) + INCOME + CAR_AGE + log(mileage) +
                    log(BLUEBOOK)+ BLUEBOOK + TIF + log(OLDCLAIM+1) + OLDCLAIM +
                    CLM_FREQ + MVR_PTS + CAR_AGE + PARENT1 + SEX +
                    EDUCATION_Bachelors + JOB_Clerical + JOB_Manager +
                    CAR_TYPE+ REVOKED+ URBANICITY+ MSTATUS+ CAR_USE,
                  data =na.omit(train_6))

forward.mod.6 <- step(model.6.raw, direction = "forward", trace=FALSE)
mod.6 <- step(model.6.raw, direction = "backward", trace=FALSE)

mod6_summary <- summ(mod.6, vifs = TRUE)

mod6_plot <- autoplot(mod.6, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
                theme(panel.background=element_blank())

### Model 6 Predictions
pred.6.raw <- predict(mod.6, newdata = train_6)


#======================================================================================#

## Model 6 (USED TO BE 7)

model.7.raw <- lm(log(TARGET_AMT+1) ~ MSTATUS + EDUCATION_Bachelors +
                      JOB_Clerical + JOB_Manager + SEX + PARENT1 +
                      CAR_TYPE + CAR_USE + REVOKED + URBANICITY + KIDSDRIV +
                      HOMEKIDS + CLM_FREQ + BLUEBOOK + AGE + YOJ + CAR_AGE +
                      HOME_VAL + INCOME + MVR_PTS + OLDCLAIM + TIF + TRAVTIME +
                      log(BLUEBOOK) + log(CAR_AGE+1) + log(HOME_VAL+1) +
                      log(INCOME+1) + log(MVR_PTS+1) + log(OLDCLAIM+1) + log(TIF) +
                      log(TRAVTIME),
                  data=train)

forward.mod.7 <- step(model.7.raw, direction = "forward", trace=FALSE)
mod.7 <- step(model.7.raw, direction = "backward", trace=FALSE)

mod7_summary <- summ(mod.7, vifs = TRUE)

mod7_plot <- autoplot(mod.7, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
    theme(panel.background=element_blank())

### Model 7 Predictions
pred.7.raw <- predict(mod.7, newdata = train)

#======================================================================================#

## Model Evaluations


eval_mods <- data.frame(mod1.conf.mat$byClass,
                        mod3.conf.mat$byClass,
                        mod4.conf.mat$byClass) # add additional model stats

eval_mods <- data.frame(t(eval_mods))
row.names(eval_mods) <- c("Model.1", "Model.2", "Model.3") # add additional models

eval_mods <- dplyr::select(eval_mods, Sensitivity, Specificity, Precision, Recall, F1)


# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Pseudo R2

pseudo.r2 <- data.frame(pscl::pR2(mod.1),
                        pscl::pR2(mod.3),
                        pscl::pR2(mod.4))

pseudo.r2 <- data.frame(t(pseudo.r2))

row.names(pseudo.r2) <- c("Model.1", "Model.2", "Model.3")

# Prep test data for predictions
test$EDUCATION_Bachelors <- test$EDUCATION == "Bachelors"
test$JOB_Manager <- test$JOB == "Manager"
test$JOB_Clerical <- test$JOB == "Clerical"

# Predictions
test$TARGET_FLAG <- ifelse(predict(mod.4, newdata = test) < .5, 0, 1)
test$TARGET_AMT_Mod5 <- predict(mod.6, newdata = test)
test$TARGET_AMT_Mod6 <- predict(mod.7, newdata = test)

# Prediction Histograms

summary.pred.amt <- describe(test[, c('TARGET_AMT_Mod5', 'TARGET_AMT_Mod6')])[,c(2,8,3,5,9,4)]

summary.pred.flag <- summary(factor(test$TARGET_FLAG))

hist.pred1 <- test[, c('TARGET_AMT_Mod5', 'TARGET_AMT_Mod6')] %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(fill = "#58BFFF") +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank())

hist.pred2 <- test[, c('TARGET_FLAG', 'TARGET_AMT_Mod5', 'TARGET_AMT_Mod6')] %>%
    gather(-TARGET_FLAG, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(TARGET_FLAG))) +
    geom_histogram(position="dodge", bins=10, alpha=0.5) +
    facet_wrap(~ var, scales = "free") +
    scale_fill_manual("TARGET_FLAG_Mod3",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank(), legend.position="top")


# Betsy's testing area <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Keep code for possible future use

#```{r, fig.height=9, eval=FALSE}
#pairs.train <- train.num.a %>%
#    dplyr::select(-TARGET_AMT)
#group <- NA
#group[pairs.train$TARGET_FLAG == 0] <- 1
#group[pairs.train$TARGET_FLAG == 1] <- 2
#pairs(pairs.train, col=c("#58BFFF", "#3300FF")[group], pch = c(3, 1)[group])
#```