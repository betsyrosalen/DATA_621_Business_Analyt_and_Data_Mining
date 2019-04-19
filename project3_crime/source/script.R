# Proj3: crime
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
                               c('black','1000(B_k - 0.63)^2 where B_k is the proportion of blacks by town','predictor'),
                               c('lstat','lower status of the population (percent)','predictor'),
                               c('medv','median value of owner-occupied homes in $1000s','predictor'))
colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

# Summary Statistics
sum_stat <- describe(train)[,c(2,8,3,5,9,4)]

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
model.1 <- glm(target ~ .,
               family = binomial,
               data = train) # + 0 removes intercept
#
mod.1 <- train(target ~., data = train,
                 method = "glm",
                 family = "binomial",
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

mod1_summary <- summary(mod.1)
mod1_summary_a <- summ(model.1, vifs = TRUE)

### Model 1 Summary Statistics
pred.1.raw <- predict(mod.1, newdata = train)
pred.1 <- as.factor(ifelse(pred.1.raw < .5, 0, 1))
mod1.conf.mat <- confusionMatrix(pred.1,
                                 as.factor(train$target), mode = "everything")

#====================================================================================================================#
## Model 2

## Build the model
model.2.raw <- glm(target ~ zn + indus + chas + nox + rm + age + dis + rad + tax +
                     ptratio + lstat + medv + log(age) + log(dis) + log(nox) +
                     log(rad) + log(tax) + log(indus) + log(ptratio),
               family = binomial,
               data = train)

mod2_summary_raw <- summ(model.2.raw, vifs = TRUE)

model.2.step <- step(model.2.raw, trace=FALSE)
mod2_summary_step <- summ(model.2.step, vifs = TRUE)

model.2 <- glm(target ~ indus + nox + rm + age + dis + rad + tax + ptratio + medv +
                     log(age) + log(dis) + log(tax) + log(ptratio),
                     family = binomial, data = train)

mod2_summary <- summary(model.2)
mod2_summary_a <- summ(model.2, vifs = TRUE)

#marg_mod_plot_2 <- mmps(model.2, layout=c(5,4), key=NULL) # library car

### Model 2 Summary Statistics
pred.2.raw <- predict(model.2, newdata = train)
pred.2 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
mod2.conf.mat <- confusionMatrix(pred.2,
                                 as.factor(train$target), mode = "everything")

#====================================================================================================================#

## Model 3

## Build the model
#model.3 <- glm(target ~., data = train,
#                 family = binomial)

### Model 3 Summary Statistics
#pred.3.raw <- predict(model.3, newdata = train)
#pred.3 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
#mod3.conf.mat <- confusionMatrix(pred.2,
#                                 as.factor(train$target), mode = "everything")

#### Step Approach

#backward <- step(multinom(target ~ ., train), direction = "backward", trace=FALSE)
#backward <- summary(backward)

#forward <- step(multinom(target ~ ., train), direction = "forward", trace=FALSE)
#forward <- summary(forward)

#====================================================================================================================#

## Model 4

#model4 <- glm(formula = target ~., family = binomial(logit), data = train)
#model.4 <- step(model4, direction="backward")

### Model 4 Summary Statistics
#pred.4.raw <- predict(model.4, newdata = train)
#pred.4 <- as.factor(ifelse(pred.4.raw < .5, 0, 1))
#mod4.conf.mat <- confusionMatrix(pred.4,as.factor(train$target), mode = "everything")

#====================================================================================================================#

## Model 5

#big_mod5 <- glm(target ~ (zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat + medv)^2,
#               data = train, family = binomial)

#small_mod5 <- step(big_mod5, trace=FALSE)

# The above code is VERY computationally expensive
# Here's the result so it doesn't need to be run again.
model.5 <-  glm(formula = target ~ zn + indus + chas + nox + rm + age + dis +
                    rad + tax + ptratio + lstat + medv + zn:age + zn:tax + zn:ptratio +
                    zn:lstat + indus:chas + indus:rad + indus:ptratio + indus:medv +
                    nox:age + nox:tax + nox:ptratio + nox:lstat + nox:medv +
                    rm:age + age:tax + age:ptratio + dis:tax + dis:ptratio+
                    dis:lstat + dis:medv + rad:tax + tax:medv + lstat:medv, family = binomial,
                    data = train)

mod5_summary <- summary(model.5)
mod5_summary_a <- summ(model.5, vifs = TRUE)

#resid_plot_5 <- residual.plots(model.5, exclude = 4, layout = c(2, 2)) # library car

#marg_mod_plot_5 <- mmps(model.5, span = 3/4, layout = c(2, 2)) # library car

### Model 5 Summary Statistics
pred.5.raw <- predict(model.5, newdata = train)
pred.5 <- as.factor(ifelse(pred.5.raw < .5, 0, 1))
mod5.conf.mat <- confusionMatrix(pred.5, as.factor(train$target), mode = "everything")


#====================================================================================================================#
## Model 6

## Build the model
less_than_five <- function(x) ifelse(x < 5, x, 0)
five_and_over <- function(x) ifelse(x >= 5, x, 0)

model.6.raw <- glm(target ~ (less_than_five(rad) + five_and_over(rad)) + zn + indus + chas + nox +
                       rm + age + dis + tax + ptratio + lstat + medv + log(age) +
                       log(dis) + log(tax) + log(ptratio) + indus:nox + indus:dis +
                       indus:tax+ nox:age + nox:dis + rm:medv + dis:age,
                   family = binomial,
                   data = train)
mod6_summary_raw <- summ(model.6.raw, vifs = TRUE)
backward.mod <- step(model.6.raw, direction = "backward", trace=FALSE)
backward_sum <- summary(backward.mod)

#forward.mod <- step(model.6.raw, direction = "forward", trace=FALSE)
#forward_sum <- summary(forward.mod)

model.6 <- glm(target ~ less_than_five(rad) + five_and_over(rad) +
                    zn + indus + nox + rm + age + tax + ptratio +
                    log(dis) + log(tax) + log(ptratio) + indus:dis + rm:medv +
                    age:dis + 0, family = binomial, data = train) # + 0 removes intercept

mod6_summary <- summary(model.6)
mod6_summary_a <- summ(model.6, vifs = TRUE)
### Model 6 Summary Statistics
pred.6.raw <- predict(model.6, newdata = train)
pred.6 <- as.factor(ifelse(pred.6.raw < .5, 0, 1))
mod6.conf.mat <- confusionMatrix(pred.6, as.factor(train$target), mode = "everything")

mod.6 <- train(target ~ less_than_five(rad) + five_and_over(rad) +
                 zn + indus + nox + rm + age + tax + ptratio +
                 log(dis) + log(tax) + log(ptratio) + indus:dis + rm:medv +
                 age:dis + 0,
               family = binomial,
               data = train,
               method = 'glm') # + 0 removes intercept

#====================================================================================================================#

## Model Evaluations

eval_mods <- data.frame(mod1.conf.mat$byClass,
                   mod2.conf.mat$byClass,
                   mod5.conf.mat$byClass,
                   mod6.conf.mat$byClass) # add additional model stats

eval_mods <- data.frame(t(eval_mods))
row.names(eval_mods) <- c("Model.1", "Model.2", "Model.3", "Model.4") # add additional models

eval_mods <- dplyr::select(eval_mods, Sensitivity, Specificity, Precision, Recall, F1)


# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Pseudo R2

pseudo.r2 <- data.frame(pscl::pR2(model.1),
                        pscl::pR2(model.2),
                        pscl::pR2(model.5),
                        pscl::pR2(model.6))

pseudo.r2 <- data.frame(t(pseudo.r2))

row.names(pseudo.r2) <- c("Model.1", "Model.2", "Model.3", "Model.4")
