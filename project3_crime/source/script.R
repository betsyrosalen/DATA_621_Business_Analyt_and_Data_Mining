
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# load data
train <- read.csv ('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-training-data_modified.csv', stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-evaluation-data_modified.csv', stringsAsFactors = F, header = T)

#train$target <- as.factor(train$target)
#train$chas <- as.factor(train$chas)

variable_descriptions <- rbind(c('target','whether the crime rate is above the median crime rate (1) or not (0)','response'),
                               c('zn','proportion of residential land zoned for large lots (over 25000 square feet) ','predictor'),
                               c('indus','proportion of non-retail business acres per suburb','predictor'),
                               c('chas','a dummy var. for whether the suburb borders the Charles River (1) or not (0)','predictor'),
                               c('nox','nitrogen oxides concentration (parts per 10 million)','predictor'),
                               c('rm','average number of rooms per dwelling','predictor' ),
                               c('age','proportion of owner-occupied units built prior to 1940','predictor'),
                               c('dis','weighted mean of distances to five Boston employment centers','predictor'),
                               c('rad','index of accessibility to radial highways','predictor'),
                               c('tax','full-value property-tax rate per $10,000','predictor'),
                               c('ptratio','pupil-teacher ratio by town','predictor'),
                               c('black','$1000(B_k - 0.63)^2$ where $B_k$ is the proportion of blacks by town','predictor'),
                               c('lstat','lower status of the population (percent)','predictor'),
                               c('medv','median value of owner-occupied homes in $1000s','predictor'))
colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

# Summary Statistics
sum_stat <- describe(train)[,c(2,8,3,5,9,4)]

# Shape of Predictor Distributions
#Hist <- train[,-13] %>%
#  gather() %>%
#  ggplot(aes(x = value)) +
#  facet_wrap(~ key, scales = "free") +
#  geom_histogram(fill = "#58BFFF") +
#  xlab("") +
#  ylab("") +
#  theme(panel.background = element_blank())

Hist_new <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(target))) +
    geom_histogram(position="dodge", bins=10, alpha=0.5) +
    facet_wrap(~ var, scales = "free") + 
    scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank())

# Outliers
boxplot_train <- train[,-13]
boxplot_train$tax <- boxplot_train$tax/10
melt.train <- melt(boxplot_train)

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


#  Missing Values
na.barplot <- plot_missing(train)


# Linearity
#linearity <- train %>%
#  gather(-target, key = "var", value = "value") %>%
#  ggplot(aes(x = value, y = target)) +
#  geom_point(alpha=0.1) +
#  stat_smooth() +
#  facet_wrap(~ var, scales = "free", ncol=3) +
#  ylab("target") +
#  xlab("") +
#  theme(panel.background = element_blank())


# Boxplots
boxplots <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x=factor(target), y=val)) +
    geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
    stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
                 size=2, show.legend=TRUE) +
    stat_summary(aes(colour="median"), fun.y=median, geom="point",
                 size=2, show.legend=TRUE) +
    facet_wrap(~ var, scales = "free", ncol=4) +
    labs(colour="Statistics", x="", y="") +
    scale_colour_manual(values=c("#9900FF", "#3300FF")) +
    theme(panel.background=element_blank())

# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Correlation
# correl <- ggpairs(train)
# This plot doesn't work in the script file.  Moved code to our .Rmd file
# The code works to create  correlation table though!
correl2 <- train %>% 
  select(-target) %>% 
  cor() %>% 
  round(2) %>% 
  corrplot(method = "circle")

# Shape of Predictor Distributions after log transformation
#Hist_log <- train[,-13] %>%
#    gather() %>%
#    ggplot(aes(x = value)) +
#    scale_y_continuous(trans = "log") + 
#    facet_wrap(~ key, scales = "free") +
#    geom_histogram(fill = "#58BFFF") +
#    xlab("") +
#    ylab("") +
#    theme(panel.background = element_blank())

# https://stackoverflow.com/questions/38722202/how-do-i-change-the-number-of-decimal-places-on-axis-labels-in-ggplot2
# Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)

Hist_log_new <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x = val, fill=factor(target))) +
    geom_histogram(position="dodge", bins=10, alpha=0.5) +
    facet_wrap(~ var, scales = "free") + 
    scale_y_continuous(trans = "log", label=scaleFUN) + 
    scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank())

# Linearity at log10 scale
#linearity_log <- train %>%
#  gather(-target, key = "var", value = "value") %>%
#  ggplot(aes(x = value, y = target)) +
#  geom_point(alpha=0.1) +
#  scale_x_log10() + 
#  stat_smooth() +
#  facet_wrap(~ var, scales = "free", ncol=3) +
#  ylab("target") +
#  xlab("") +
#  theme(panel.background = element_blank())

linearity_log_new <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x=factor(target), y=val)) +
    geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
    scale_y_continuous(trans = "log", label=scaleFUN) + 
    stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
                 size=2, show.legend=TRUE) +
    stat_summary(aes(colour="median"), fun.y=median, geom="point",
                 size=2, show.legend=TRUE) +
    facet_wrap(~ var, scales = "free", ncol=4) +
    labs(colour="Statistics", x="", y="") +
    scale_colour_manual(values=c("#9900FF", "#3300FF")) +
    theme(panel.background=element_blank())


# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Model 1

## Build the model
model.1 <- train(target ~., data = train,
                 method = "glm", 
                 family = "binomial",
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd


### Model 1 Summary Statistics
pred.1.raw <- predict(model.1, newdata = train)
pred.1 <- as.factor(ifelse(pred.1.raw < .5, 0, 1))
mod1.conf.mat <- confusionMatrix(pred.1, 
                                 as.factor(train$target), mode = "everything")

#====================================================================================================================#
## Model 2
train_subset <- subset(train, select = c('nox', 'age', 'dis', 'rad', 'ptratio', 'medv'))
train_log <- log(train_subset)
train_log <- cbind(train$target, train$zn, train_log)
colnames(train_log) <- c('target', 'zn', 'log_nox', 'log_age', 'log_dis', 'log_rad', 'log_ptratio', 'log_medv')

## Build the model
model.2 <- train(target ~., data = train,
                 method = "glm", 
                 family = "binomial",
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

### Model 2 Summary Statistics
pred.2.raw <- predict(model.2, newdata = train)
pred.2 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
mod2.conf.mat <- confusionMatrix(pred.2, 
                                 as.factor(train$target), mode = "everything")

#====================================================================================================================#

## Model 3
train_subset <- subset(train, select = c('dis', 'age', 'rad', 'ptratio', 'medv'))
train_log <- log(train_subset)
train_log <- cbind(train$target, train_log)
colnames(train_log) <- c('target','log_dis', 'log_age', 'log_rad', 'log_ptratio', 'log_medv')


## Build the model
model.3 <- train(target ~., data = train,
                 method = "glm", 
                 family = "binomial",
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

### Model 3 Summary Statistics
pred.3.raw <- predict(model.3, newdata = train)
pred.3 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
mod3.conf.mat <- confusionMatrix(pred.2, 
                                 as.factor(train$target), mode = "everything")

#### Step Approach

backward <- step(multinom(target ~ ., train), direction = "backward", trace=FALSE)
backward <- summary(backward)

forward <- step(multinom(target ~ ., train), direction = "forward", trace=FALSE)
forward <- summary(forward)

#====================================================================================================================#

## Model 4

model4 <- glm(formula = target ~., family = binomial(logit), data = train)
model.4 <- step(model4, direction="backward")
mod4 <- train(target ~., data = train, method = "glm", family = "binomial")

### Model 4 Summary Statistics
pred.4.raw <- predict(mod4, newdata = train)
pred.4 <- as.factor(ifelse(pred.4.raw < .5, 0, 1))
mod4.conf.mat <- confusionMatrix(pred.4,as.factor(train$target), mode = "everything")

#====================================================================================================================#

## Model 5

#big_mod5 <- glm(target ~ (zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat + medv)^2, 
#               data = train, family = binomial)

#small_mod5 <- step(big_mod5, trace=FALSE)

# The above code is VERY resource intensive.  
# Here's the result so it doesn't need to be run again.
result_small_mod5 <-  glm(formula = target ~ zn + indus + chas + nox + rm + age + dis +
                    rad + tax + ptratio + lstat + medv + zn:age + zn:tax + zn:ptratio +
                    zn:lstat + indus:chas + indus:rad + indus:ptratio + indus:medv +
                    nox:age + nox:tax + nox:ptratio + nox:lstat + nox:medv +
                    rm:age + age:tax + age:ptratio + dis:tax + dis:ptratio+
                    dis:lstat + dis:medv + rad:tax + tax:medv + lstat:medv, family = binomial(),
                    data = train)
mod5_summary <- summary(result_small_mod5)

mod5 <- train(target ~ zn + indus + chas + nox + rm + age + dis +
                rad + tax + ptratio + lstat + medv + zn:age + zn:tax + zn:ptratio +
                zn:lstat + indus:chas + indus:rad + indus:ptratio + indus:medv +
                nox:age + nox:tax + nox:ptratio + nox:lstat + nox:medv +
                rm:age + age:tax + age:ptratio + dis:tax + dis:ptratio+
                dis:lstat + dis:medv + rad:tax + tax:medv + lstat:medv, 
              family = binomial(),
              data = train,
              method = "glm")
resid_plot_5 <- residual.plots(result_small_mod5, exclude = 4, layout = c(2, 2)) # please add library

marg_mod_plot_5 <- mmps(result_small_mod5, span = 3/4, layout = c(2, 2)) # please add library

### Model 5 Summary Statistics
pred.5.raw <- predict(mod5, newdata = train)
pred.5 <- as.factor(ifelse(pred.5.raw < .5, 0, 1))
mod5.conf.mat <- confusionMatrix(pred.5, as.factor(train$target), mode = "everything")
#====================================================================================================================#

## Model Evaluations

eval_mods <- data.frame(mod1.conf.mat$byClass, 
                   mod2.conf.mat$byClass,
                   mod3.conf.mat$byClass,
                   mod4.conf.mat$byClass,
                   mod5.conf.mat$byClass) # add additional model stats

eval_mods <- data.frame(t(eval_mods))
row.names(eval_mods) <- c("Model.1", "Model.2", "Model.3", "Model.4", "Model.5") # add additional models

eval_mods <- dplyr::select(eval_mods, Sensitivity, Specificity, Precision, Recall, F1)


# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
