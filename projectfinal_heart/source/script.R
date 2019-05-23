
set.seed(147)

# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load data

data <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/projectfinal_heart/data/heart.csv', 
                   stringsAsFactors = F, header = T)

variable_descriptions <- rbind(c('var_name','def','type'),
                               c('var_name','def','type'),
                               c('var_name','def','type'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

data$target <- as.factor(data$target)

vars <- rbind(c('age','Age','continuous numerical predictor'),
              c('sex','Sex. Female = 0, Male = 1 ','categorical predictor'),
              c('cp','Chest pain type. Scale of 0 to 4','categorical predictor'),
              c('trestbps','Diastolic blood pressure in mmHg','continuous numerical predictor'),
              c('chol','Serum cholesterol (mg/dl)','continuous numerical predictor'),
              c('fbs','Fasting blood sugar. Greater than 120mg/dl, value of 0 or 1','categorical predictor'),
              c('restecg','Resting ECG. Value of 0, 1, or 2','categorical predictor'),
              c('thalach','Maximum heartrate achieved from thallium test','continuous numerical predictor'),
              c('exang','Exercise-induced angina. Value of 0 or 1','categorical predictor'),
              c('oldpeak','Old-peak.ST depression induced by exercise relative to rest','continuous numerical predictor'),
              c('slope','Slope of peak exercise ST segment, value of 1, 2, or 3','categorical predictor'),
              c('ca','Number of major vessels (0-3) colored by fluoroscopy','categorical predictor'),
              c('thal','Exercise thallium scintigraphic defects','categorical predictor'),
              c('target','Response Variable','categorical predictor') )

colnames(vars) <- c('VARIABLE','DEFINITION','TYPE')

# Bootstrap surrogate data using synthpop

# https://cran.r-project.org/web/packages/synthpop/vignettes/synthpop.pdf
# https://cran.r-project.org/web/packages/synthpop/synthpop.pdf
# https://www.r-bloggers.com/generating-synthetic-data-sets-with-synthpop-in-r/
# https://www.geos.ed.ac.uk/homes/graab/synthpop.pdf

syn_obj <- synthpop::syn(data = data, m = 20)  # creates 20 (to start, elevate after testing) synthetic datasets based on original dataset and its variables distributions
syn_dflist <- syn_obj$syn  # extract list of synthesized data frames from synds object
syn_df <- dplyr::bind_rows(syn_dflist, .id = 'column_label')  # unlist synds object and configure as df


# Summary Statistics

orig_data <- data
syn_data <-  syn_df
syn_data$column_label <- NULL
colnames(orig_data)[colnames(orig_data)=="?..age"] <- "age"
colnames(syn_data)[colnames(syn_data)=="?..age"] <- "age"

orig_data$sex <- as.factor(orig_data$sex)
orig_data$cp <- as.factor(orig_data$cp)
orig_data$ca <- as.factor(orig_data$ca)
orig_data$fbs <- as.factor(orig_data$fbs)
orig_data$restecg <- as.factor(orig_data$restecg)
orig_data$slope <- as.factor(orig_data$slope)
orig_data$target <- as.factor(orig_data$target)
orig_data$thal <- as.factor(orig_data$thal)
orig_data$exang <- as.factor(orig_data$exang)

syn_data$sex <- as.factor(syn_data$sex)
syn_data$cp <- as.factor(syn_data$cp)
syn_data$ca <- as.factor(syn_data$ca)
syn_data$fbs <- as.factor(syn_data$fbs)
syn_data$restecg <- as.factor(syn_data$restecg)
syn_data$slope <- as.factor(syn_data$slope)
syn_data$target <- as.factor(syn_data$target)
syn_data$thal <- as.factor(syn_data$thal)
syn_data$exang <- as.factor(syn_data$exang)


data_num <- orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak')]
data_cat <- orig_data[, c('sex', 'cp','ca', 'exang','fbs', 'restecg', 'slope','target', 'thal' )]
syn_num <- syn_data[, c( 'age','trestbps', 'chol','thalach', 'oldpeak')]
syn_cat <- syn_data[, c('sex', 'cp','ca', 'exang', 'fbs', 'restecg', 'slope','target', 'thal')]

orig_data_stats <- describe(data_num)[,c(2,8,3,5,9,4)]
syn_data_stats <- describe(syn_num)[,c(2,8,3,5,9,4)]
data_cat_stats <- summary(data_cat[, c( 'cp','ca',  'restecg', 'slope', 'thal')])
syn_cat_stats <- summary(syn_cat[, c('cp','ca',   'restecg', 'slope', 'thal')])
data_cat_stats_b <- summary(data_cat[, c('exang', 'fbs', 'sex', 'target')])
syn_cat_stats_b <- summary(syn_cat[, c('exang', 'fbs', 'sex', 'target')])


# Outliers

# Data Distribution

data_num_hist <- orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak', 'target')]
hist.num <- data_num_hist %>%
  gather(-target, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(target))) +
  geom_histogram(position="dodge", bins=10, alpha=0.5) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top")

bar.cat <- data_cat %>%
  gather(-target, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(target))) +
  geom_bar(position="dodge", alpha=0.5) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Scaled BoxPlots

scaled.train.num <- as.data.table(scale(orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak')]))
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


#Linear relationship between each numeric predictor and the target

linear_graph_data <- orig_data[, c('age','trestbps', 'chol', 'thalach', 'oldpeak', 'target')]
boxplots.target <- linear_graph_data  %>%
  gather(-target,key = "var", value = "val") %>%
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


## Correlation

plot.data <- linear_graph_data
plot.data$target<- factor(plot.data$target)
corr.plot2 <- plot.data %>% 
  ggscatmat(color="target", alpha=0.1) +
  scale_color_manual(values=c("#58BFFF", "#3300FF")) +
  theme(panel.background=element_blank(), legend.position="top",
        axis.text.x = element_text(angle=-40, vjust=1, hjust=0))


## Heart disease by gender
#gender_data <-  orig_data
#levels(gender_data$target) = c("No disease","Disease")
#levels(gender_data$sex) = c("female","male","")
#gender_hist <-  mosaicplot(gender_data$sex ~ gender_data$target,
                           #main="",shade=FALSE,color = c("#58BFFF", "#3300FF"),
                           #xlab="Gender", ylab="Heart disease") 

## Heart disease by Fasting blood sugar
#levels(gender_data$target) = c("No disease","Disease")
#levels(gender_data$fbs) = c("0","1","")
#fbs_hist <-  mosaicplot(gender_data$fbs ~ gender_data$target,
                        #main="",shade=FALSE,color = c("#58BFFF", "#3300FF"),
                        #xlab="fbs - Fasting blood sugar", ylab="Heart disease") 

## Heart disease by Fasting blood sugar
#levels(gender_data$target) = c("No disease","Disease")
#levels(gender_data$exang) = c("0","1","")
#exang_hist <-  mosaicplot(gender_data$exang ~ gender_data$target,
                          #main="",shade=FALSE,color = c("#58BFFF", "#3300FF"),
                          #xlab="exang - Exercise-induced angina ", ylab="Heart disease") 



#bar_cp <- ggplot(orig_data,aes(factor(cp)))+geom_bar(aes(fill = target), position = "dodge")


# Address outliers, treating as NAs and imputing


# Compare distribution of original data and synthesized data
# compare(synth.obj, data)  # visually compare of synthetic datasets vs original data
# syn_csv <- write.syn(syn_obj, 'csv')


# Split data into test and train using cross-validation (Jeremy: should we replace this section and just kfold when building models?)

folds <- 10  # set to 10 provisionally
train_bootstrap <- caret::trainControl(method = 'boot', number = folds)  # bootstrap resampling approach
train_kfold <- caret::trainControl(method = 'cv', number = folds)  # k-fold cross validation
train_kfoldrpt <- caret::trainControl(method = 'repeatedcv', number = folds, repeats = 3)  # k-fold cross validation, provisionally set to 3 repeats but explore setting
train_loocv <- caret::trainControl(method = 'LOOCV')
syn_df$column_label <- NULL



#Missing Data

# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Divide syn_data into train and test 




# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Model 1 - logistic regression model (see notes)

#======================================================================================#

## Model 2 - decision tree model (see notes)

#======================================================================================#

## Model 3 - random forest model (see notes)

#======================================================================================#

## Model 4 - support vector machines model (see notes)

# https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/

# Create test and train datasets
train_flag_orig <- caret::createDataPartition(orig_data$target, 
                                              p = .8, 
                                              list = FALSE)
svm_train_orig <- orig_data[train_flag_orig,]
svm_test_orig <- orig_data[-train_flag_orig,]

train_flag_syn <- caret::createDataPartition(syn_data$target, 
                                             p = .8, 
                                             list = FALSE)
svm_train_syn <- syn_data[train_flag_syn,]
svm_test_syn <- syn_data[-train_flag_syn,]


# Set up cross-validation
svm_cv_orig <- caret::trainControl(method = 'repeatedcv',  # resampling method is repeated cross-validation
                                   number = 10,  # 10 resampling iterations
                                   repeats = 3) #,  # check and confirm
                                   # classProbs = 'TRUE',  # check and confirm
                                   # summaryFunction = twoClassSummary)

svm_cv_syn <- caret::trainControl(method = 'repeatedcv',  # resampling method is repeated cross-validation
                                  number = 10,  # 10 resampling iterations
                                  repeats = 3)  # check and confirm


# Build SVM models
mod_svmlinear_orig <- caret::train(target ~ .,
                                   data = svm_train_orig,
                                   method = 'svmLinear',  # check and confirm, alternate = 'pls'
                                   trControl = svm_cv_orig,
                                   preProcess = c('center', 'scale'),  # check and confirm
                                   tunelength = 10) #,  # check and confirm
                                   # metric = 'ROC') # requires classProbs = TRUE in trainControl()
# tweak performance metric from accuracy / kappa?

mod_svmlinear_syn <- caret::train(target ~ .,
                                  data = svm_train_orig,
                                  method = 'svmLinear',  # check and confirm
                                  trControl = svm_cv_syn,
                                  preProcess = c('center', 'scale'),  # check and confirm
                                  tunelength = 10)  # check and confirm
# tweak performance metric from accuracy / kappa?


# Predict test set
testpred_svmlinear_orig <- predict(mod_svmlinear_orig, 
                                   newdata = svm_test_orig)

testpred_svmlinear_syn <- predict(mod_svmlinear_syn, 
                                  newdata = svm_test_syn)

conf_matrix_orig <- confusionMatrix(testpred_svmlinear_orig, 
                                    svm_test_orig$target)

conf_matrix_syn <- confusionMatrix(testpred_svmlinear_syn, 
                                   svm_test_syn$target)


# e1071 implementation of svm - probably not needed as caret more straightforward
# mod_svm_synth <- svm(formula = target ~ .,
                     # data = syn_df,
                     # type = 'C-classification',
                     # kernel = 'linear')

#======================================================================================#

## Model 5 - naive bayes model (see notes)

#train data
syn_data_nb <- syn_data
#test data
orig_data_nb <- orig_data

colnames(syn_data_nb)[colnames(syn_data_nb)=="ï..age"] <- "age"
colnames(orig_data_nb)[colnames(orig_data_nb)=="ï..age"] <- "age"

#chol as category
nb.cat <- function(x, lower = 100, upper, by = 80,sep = "-", above.char = "+") {labs <- c(paste(seq(lower, upper - by, by = by),
seq(lower + by - 1, upper - 1, by = by),sep = sep),paste(upper, above.char, sep = ""))
cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf), right = FALSE, labels = labs)}
syn_data_nb$cholGroup <-  nb.cat(syn_data_nb$chol, upper = 350)         
orig_data_nb$cholGroup <-  nb.cat(orig_data_nb$chol, upper = 350)   
syn_data_nb$chol <-  NULL     
orig_data_nb$chol <-  NULL 

#removed age and sex
syn_data_nb$age <-  NULL  
orig_data_nb$age <-  NULL  
syn_data_nb$sex <-  NULL  
orig_data_nb$sex <-  NULL 


model_nb <- e1071::naiveBayes(target ~ ., data = syn_data_nb)

orig_data_nb$pred <- predict(model_nb, orig_data_nb)
confusionMatrix_nb <- confusionMatrix(orig_data_nb$pred , orig_data_nb$target )

#   Naive Bayes Model Accuracy (train -  synthpop dataset;  test - original data set) : 0.8746  

#======================================================================================#


## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

