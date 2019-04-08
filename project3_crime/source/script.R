
# load data
train <- read.csv ('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-training-data_modified.csv', stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-evaluation-data_modified.csv', stringsAsFactors = F, header = T)

#train$target <- as.factor(train$target)
#train$chas <- as.factor(train$chas)

# Summary Statistics
sum_stat<- describe(train)[,c(2,8,3,5,9,4)]

# Shape of Predictor Distributions
Hist <- train[,-13] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
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
linearity <- train %>%
  gather(-target, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = target)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("target") +
  xlab("") +
  theme(panel.background = element_blank())


# Boxplots
boxplots <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x=factor(target), y=val)) +
    geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
    stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
                 size=2, show.legend=TRUE) +
    stat_summary(aes(colour="median"), fun.y=median, geom="point",
                 size=2, show.legend=TRUE) +
    facet_wrap(~ var, scales = "free", ncol=3) +
    labs(colour="Statistics", x="", y="") +
    scale_colour_manual(values=c("#9900FF", "#3300FF")) +
    theme(panel.background=element_blank())

# Correlation
# correl <- ggpairs(train)
correl2 <- train %>% 
  select(-target) %>% 
  cor() %>% 
  round(2) %>% 
  corrplot(method = "circle")

# Shape of Predictor Distributions after log transformation
Hist_log <- train[,-13] %>%
    gather() %>%
    ggplot(aes(x = value)) +
    scale_y_continuous(trans = "log") + 
    facet_wrap(~ key, scales = "free") +
    geom_histogram(fill = "#58BFFF") +
    xlab("") +
    ylab("") +
    theme(panel.background = element_blank())

# Linearity at log10 scale
linearity_log <- train %>%
  gather(-target, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = target)) +
  geom_point(alpha=0.1) +
  scale_x_log10() + 
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("target") +
  xlab("") +
  theme(panel.background = element_blank())


linearity_log_new <- train %>%
    gather(-target, key = "var", value = "val") %>%
    ggplot(aes(x=factor(target), y=val)) +
    geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
    scale_y_continuous(trans = "log") + 
    stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
                 size=2, show.legend=TRUE) +
    stat_summary(aes(colour="median"), fun.y=median, geom="point",
                 size=2, show.legend=TRUE) +
    facet_wrap(~ var, scales = "free", ncol=3) +
    labs(colour="Statistics", x="", y="") +
    scale_colour_manual(values=c("#9900FF", "#3300FF")) +
    theme(panel.background=element_blank())

# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


## Model 1

## Split the dataset
split <- caret::createDataPartition(train$target, p=0.8, list=FALSE)
split.train <- train[split, ]
split.validation <- train[-split, ]

## Build the model
model.1 <- train(target ~., data = split.train,
                 method = "glm", 
                 family = "binomial",
                 trControl = trainControl(method = "cv", number = 10, 
                                          savePredictions = TRUE),
                 tuneLength = 5, 
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd


### Model 1 Summary Statistics
pred.1.raw <- predict(model.1, newdata = split.validation)
pred.1 <- as.factor(ifelse(pred.1.raw < .5, 0, 1))
mod1.conf.mat <- confusionMatrix(pred.1, 
                                 as.factor(split.validation$target), mode = "everything")

#====================================================================================================================#
## Model 2
train_subset <- subset(train, select = c('nox', 'age', 'dis', 'rad', 'ptratio', 'medv'))
train_log <- log(train_subset)
train_log <- cbind(train$target, train$zn, train_log)
colnames(train_log) <- c('target', 'zn', 'log_nox', 'log_age', 'log_dis', 'log_rad', 'log_ptratio', 'log_medv')

## Split the dataset
split <- caret::createDataPartition(train_log$target, p=0.8, list=FALSE)
split.train <- train_log[split, ]
split.validation <- train_log[-split, ]

## Build the model
model.2 <- train(target ~., data = split.train,
                 method = "glm", 
                 family = "binomial",
                 trControl = trainControl(method = "cv", number = 10, 
                                          savePredictions = TRUE),
                 tuneLength = 5, 
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

### Model 2 Summary Statistics
pred.2.raw <- predict(model.2, newdata = split.validation)
pred.2 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
mod2.conf.mat <- confusionMatrix(pred.2, 
                                 as.factor(split.validation$target), mode = "everything")

#====================================================================================================================#

## Model 3
train_subset <- subset(train, select = c('dis', 'age', 'rad', 'ptratio', 'medv'))
train_log <- log(train_subset)
train_log <- cbind(train$target, train_log)
colnames(train_log) <- c('target','log_dis', 'log_age', 'log_rad', 'log_ptratio', 'log_medv')

## Split the dataset
split <- caret::createDataPartition(train_log$target, p=0.8, list=FALSE)
split.train <- train_log[split, ]
split.validation <- train_log[-split, ]

## Build the model
model.3 <- train(target ~., data = split.train,
                 method = "glm", 
                 family = "binomial",
                 trControl = trainControl(method = "cv", number = 10, 
                                          savePredictions = TRUE),
                 tuneLength = 5, 
                 preProcess = c("center", "scale")) # center and scale data based on the mean and sd

### Model 3 Summary Statistics
pred.3.raw <- predict(model.3, newdata = split.validation)
pred.3 <- as.factor(ifelse(pred.2.raw < .5, 0, 1))
mod3.conf.mat <- confusionMatrix(pred.2, 
                                 as.factor(split.validation$target), mode = "everything")

#### Step Approach
#====================================================================================================================#

## Model 4

model4 <- glm(formula = target ~., family = binomial(logit), data = split.train)
model.4<- step(model4,direction="backward")

### Model 4 Summary Statistics
pred.4.raw <- predict(model.4, newdata = split.validation)
pred.4 <- as.factor(ifelse(pred.4.raw < .5, 0, 1))
mod4.conf.mat <- confusionMatrix(pred.4,as.factor(split.validation$target), mode = "everything")




#====================================================================================================================#

## Model Evaluations

eval <- data.frame(mod1.conf.mat$byClass, 
                   mod2.conf.mat$byClass,
                   mod3.conf.mat$byClass,
                   mod4.conf.mat$byClass) # add additional model stats

eval <- data.frame(t(eval))
row.names(eval) <- c("Model.1", "Model.2", "Model.3", "Model.4") # add additional models

eval <- dplyr::select(eval, Sensitivity, Specificity, Precision, Recall, F1)


# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
