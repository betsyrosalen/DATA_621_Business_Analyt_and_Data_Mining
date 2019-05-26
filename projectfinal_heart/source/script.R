# REFERENCE STUDIES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

studies <- rbind(c('Hall',  '2000', 'Na?ve Bayes', '.832'),
                 c('Hall',  '2000', 'K Nearest Neighbour', '.821'),
                 c('Hall',  '2000', 'Decision Tree', '.753'),
                 c('Yan, Zheng et al.', '2003', 'Multilayer Perceptron', '.636'),
                 c('Herron', '2004', 'Support Vector Machine', '.836'),
                 c('Herron', '2004', 'J4.8 Decision Tree', '.776'),
                 c('Herron', '2004', 'Support Vector Machine', '.834'),
                 c('Andreeva', '2006', 'Na?ve Bayes', '.786'),
                 c('Andreeva', '2006', 'Decision Tree', '.757'),
                 c('Andreeva', '2006', 'Neural Network', '.828'),
                 c('Andreeva', '2006', 'Sequential Minimal Optimization', '.841'),
                 c('Andreeva', '2006', 'Kernel Density', '.844'),
                 c('Polat , Sahan et al.', '2007', 'Fuzzy-AIRS-K-Nearest Neighbour', '.870'),
                 c('Palaniappan and Awang', '2007', 'Na?ve Bayes', '.950'),
                 c('Palaniappan and Awang', '2007', 'Decision Tree', '.949'),
                 c('Palaniappan and Awang', '2007', 'Neural Network', '.935'),
                 c('De Beule, Maesa et al.', '2007', 'Artificial Neural Network', '.820'),
                 c('Tantimongcolwat, Naenna et al.', '2008', 'Direct Kernel Self-organizing Map', '.804'),
                 c('Tantimongcolwat, Naenna et al.', '2008', 'Multilayer Perceptron', '.745'),
                 c('Hara and Ichimura', '2008', 'Automatically Defined Groups', '.678'),
                 c('Hara and Ichimura', '2008', 'Immune Multi-agent Neural Network', '.823'),
                 c('Sitar-Taut, Zdrenghea et al.', '2009', 'Na?ve Bayes', '.620'),
                 c('Sitar-Taut, Zdrenghea et al.', '2009', 'Decision Trees', '.604'),
                 c('Tu, Shin et al.', '2009', 'Bagging Algorithm', '.814'),
                 c('Das, Turkoglu et al.', '2009', 'Neural Network Ensembles', '.890'),
                 c('Rajkumar and Reena', '2010', 'Na?ve Bayes', '.523'),
                 c('Rajkumar and Reena', '2010', 'K Nearest Neighbour', '.457'),
                 c('Rajkumar and Reena', '2010', 'Decision List', '.520'),
                 c('Srinivas, Rani et al.', '2010', 'Na?ve Bayes', '.841'),
                 c('Srinivas, Rani et al.', '2010', 'One Dependency Augmented Na?ve Bayes', '.805'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Back-propagation Neural Network', '.784'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Bayesian Neural Network', '.784'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Probabilistic Neural Network', '.706'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Polynomial Support Vector Machine', '.706'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Radial Basis Support Vector Machine', '.608'),
                 c('Kangwanariyakul, Nantasenamat et al.', '2010', 'Bayesian Neural Network', '.784'),
                 c('Kumari and Godara', '2011', 'RIPPER', '.811'),
                 c('Kumari and Godara', '2011', 'Decision Tree', '.791'),
                 c('Kumari and Godara', '2011', 'Artificial Neural Network', ',801'),
                 c('Kumari and Godara', '2011', 'Support Vector Machine', '.841'),
                 c('Soni, Ansari et al.', '2011', 'Weighted Associative Classifier', '.578'),
                 c('Soni, Ansari et al.', '2011', 'Classification-Association', '.583'),
                 c('Soni, Ansari et al.', '2011', 'Classification-Multiple ClassAssociation', '.536'),
                 c('Soni, Ansari et al.', '2011', 'Classification-Predictive Association', '.523'),
                 c('Abdullah and Rajalaxmi', '2012', 'Decision Tree', '.507'),
                 c('Abdullah and Rajalaxmi', '2012', 'Random Forest', '.633'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Neural Network', '.805'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'J4.8 Decision Tree', '.779'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Support Vector Machine', '.842'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Feature Selection with Neural Network', '.845'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Feature Selection with Decision Tree', '.842'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Feature Selection with Support Vector Machine', '.875'),
                 c('Rajeswari, Vaithiyanathan et al.', '2013', 'Neural Network', '.805'),
                 c('Lakshmi, Krishna et al.',  '2013', 'Support Vector Machine' ,	'.781'), 
                 c('Lakshmi, Krishna et al.',  '2013', 'Decision Tree', '.847'),
                 c('Lakshmi, Krishna et al.',  '2013', 'K Nearest Neighbor' ,	'.840'),
                 c('Lakshmi, Krishna et al.',  '2013', 'K Mean' ,	'.803'),
                 c('Pandey, Pandey et al.', '2013', 'COBWEB', '.020'),
                 c('Pandey, Pandey et al.', '2013', 'EM', '.815'),
                 c('Pandey, Pandey et al.', '2013', 'Farthest First', '.736'),
                 c('Pandey, Pandey et al.', '2013', 'Make Density Based Clusters', '.815'),
                 c('Pandey, Pandey et al.', '2013', 'Simple K-Means', '.809')
)

colnames(studies) <- c('AUTHOR', 'YEAR', 'TECHNIQUE', 'ACCURACY')

colnames(studies) <- c('AUTHOR', 'YEAR', 'TECHNIQUE', 'ACCURACY')

study_summ <- rbind(c('Logistic Regression', '.855'),
                    c('Random Forest', '.724'),
                    c('Support Vector Machine', '.809'),
                    c('Naive Bayes', '.819')
)

colnames(study_summ) <- c('TECHNIQUE', 'MEDIAN ACCURACY')


# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load data

data <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/projectfinal_heart/data/heart.csv', 
                   stringsAsFactors = F, header = T)

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
              c('target','Response Variable - No Heart Disease = 0, Heart Disease = 1','categorical predictor') )

colnames(vars) <- c('VARIABLE','DEFINITION','TYPE')

# Bootstrap surrogate data using synthpop

# https://cran.r-project.org/web/packages/synthpop/vignettes/synthpop.pdf
# https://cran.r-project.org/web/packages/synthpop/synthpop.pdf
# https://www.r-bloggers.com/generating-synthetic-data-sets-with-synthpop-in-r/
# https://www.geos.ed.ac.uk/homes/graab/synthpop.pdf

syn_obj <- synthpop::syn(data = data, m = 20)  # creates 20 synthetic datasets based on original dataset and its variables' distributions
syn_dflist <- syn_obj$syn  # extract list of synthesized data frames from synds object
syn_df <- dplyr::bind_rows(syn_dflist, .id = 'column_label')  # unlist synds object and configures 6,060 synthetic observations as df


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

### Summary Statistics
orig_num_stats <- describe(data_num)[,c(2,8,3,5,9,4)]
syn_num_stats <- describe(syn_num)[,c(2,8,3,5,9,4)]
orig_cat_stats <- summary(data_cat)
syn_cat_stats <- summary(syn_cat)
# orig_cat_stats <- summary(data_cat[, c( 'cp','ca',  'restecg', 'slope', 'thal')])
# syn_cat_stats <- summary(syn_cat[, c('cp','ca',   'restecg', 'slope', 'thal')])
# orig_cat_stats_b <- summary(data_cat[, c('exang', 'fbs', 'sex', 'target')])
# syn_cat_stats_b <- summary(syn_cat[, c('exang', 'fbs', 'sex', 'target')])


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


# Linear relationship between each numeric predictor and the target

linear_graph_data <- orig_data[, c('age','trestbps', 'chol', 'thalach', 'oldpeak', 'target')]
boxplots.target <- linear_graph_data  %>%
  gather(-target,key = "var", value = "val") %>%
  ggplot(aes(x=factor(target), y=val)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  facet_wrap(~ var, scales = "free", ncol=5) +
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


# Compare distribution of original data and synthesized data
compare_synthoriginal <- compare(syn_obj, data) # visually compare of synthetic datasets vs original data
# syn_csv <- write.syn(syn_obj, 'csv')

# style_p <- function(p){ 
#   for (plot in p){plot +
#     scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
#     theme(panel.background = element_blank(), legend.position="top") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# }}
# 
# lapply(compare_synthoriginal$plots, style_p)

compare_synthoriginal$plots[[1]] <- compare_synthoriginal$plots[[1]]+
  scale_fill_manual(values = c("#58BFFF", "#3300FF")) +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
compare_synthoriginal$plots[[2]] <- compare_synthoriginal$plots[[2]]+
  scale_fill_manual(values = c("#58BFFF", "#3300FF")) +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

compare_synthoriginal$plots[[3]] <- compare_synthoriginal$plots[[3]]+
  scale_fill_manual(values = c("#58BFFF", "#3300FF")) +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

compare_synthoriginal$plots[[4]] <- compare_synthoriginal$plots[[4]]+
  scale_fill_manual(values = c("#58BFFF", "#3300FF")) +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# [Betsy / Gabby, please remove the folllowing code block if no one is using]
# Split data into test and train using cross-validation
# folds <- 10  # set to 10 provisionally
# train_bootstrap <- caret::trainControl(method = 'boot', number = folds)  # bootstrap resampling approach
# train_kfold <- caret::trainControl(method = 'cv', number = folds)  # k-fold cross validation
# train_kfoldrpt <- caret::trainControl(method = 'repeatedcv', number = folds, repeats = 3)  # k-fold cross validation, provisionally set to 3 repeats but explore setting
# train_loocv <- caret::trainControl(method = 'LOOCV')
# syn_df$column_label <- NULL


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

# cv: https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# caret vignette: https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# caret overview: http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# SVM setup: https://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines
# SVM setup: https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
# SVM setup: http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/
# SVM maths: http://web.mit.edu/6.034/wwwbob/svm.pdf
# SVM tuning: https://blog.revolutionanalytics.com/2015/10/the-5th-tribe-support-vector-machines-and-caret.html


# Create test and train datasets
# [UPDATE FOR CONSISTENCY WITH ROSE / LYDIA]
train_flag_orig <- caret::createDataPartition(orig_data$target, 
                                              p = .75, 
                                              list = FALSE)
svm_train_orig <- orig_data[train_flag_orig,]
svm_test_orig <- orig_data[-train_flag_orig,]
svm_train_orig$target <- as.factor(make.names(svm_train_orig$target))  # include make.names() so 0-1 coded target variable is syntactically valid for train()
svm_test_orig$target <- as.factor(make.names(svm_test_orig$target))  # include make.names() so 0-1 coded target variable is syntactically valid for train()

train_flag_syn <- caret::createDataPartition(syn_data$target, 
                                             p = .75, 
                                             list = FALSE)
svm_train_syn <- syn_data[train_flag_syn,]
svm_test_syn <- syn_data[-train_flag_syn,]
svm_train_syn$target <- as.factor(make.names(svm_train_syn$target))  # include make.names() so 0-1 coded target variable is syntactically valid for train()
svm_test_syn$target <- as.factor(make.names(svm_test_syn$target))  # include make.names() so 0-1 coded target variable is syntactically valid for train()


# Set up cross-validation methods

# Cross-validation generic setup
ctrl_svm1 <- caret::trainControl(method = 'repeatedcv',  # resampling method is repeated cross-validation
                                 number = 10,  # 10 resampling iterations
                                 repeats = 5) #,  # conduct 5 repetitions of cross-validation

# Cross-validation for use with ROC metric argument
ctrl_svm2 <- caret::trainControl(method = 'repeatedcv',  # resampling method is repeated cross-validation
                                 repeats = 5,  # conduct 5 repetitions of cross-validation
                                 summaryFunction = twoClassSummary,
                                 classProbs = TRUE)


# First SVM RBF model (original data)

# Build first SVM RBF model with original data
mod_svmradial1 <- readRDS("./source/model/mod_svmradial1.rds")
# mod_svmradial1 <- caret::train(target ~ .,
#                                data = svm_train_orig,
#                                method = 'svmRadial',  # RBF model 
#                                trControl = ctrl_svm2,  # cross-validation for ROC 
#                                preProcess = c('center', 'scale'),  # preprocess data to center and scale
#                                metric = 'ROC')  # ROC-based evaluation
# saveRDS(mod_svmradial1, "./source/model/mod_svmradial1.rds")

# Predict values based on first SVM RBF model
testpred_svmradial1 <- predict(mod_svmradial1, 
                               newdata = svm_test_orig)

# Create confusion matrix for first SVM RBF model
confmtrx_svmradial1 <- confusionMatrix(testpred_svmradial1, 
                                       svm_test_orig$target)

# Plot ROC curve for first SVM RBF model
plot_svmradial1 <- ggplot(mod_svmradial1)


# Second SVM RBF model (original data)

# Create grid search of tuning parameters for second SVM RBF model
tunegrid_svmradial2 <- expand.grid(sigma = c(.01, .02, .03, .04, .05),
                             C = c(0.25, 0.4, .5, .6, .75))

# Build second, tuned SVM RBF model with original data
mod_svmradial2 <- readRDS("./source/model/mod_svmradial2.rds")
# mod_svmradial2 <- caret::train(target ~ .,
#                                data = svm_train_orig,
#                                method = 'svmRadial',  # RBF model 
#                                trControl = ctrl_svm2,  # cross-validation for ROC 
#                                preProcess = c('center', 'scale'),  # preprocess data to center and scale
#                                tuneGrid = tunegrid_svmradial2,  # grid search to tune on C and sigma
#                                metric = 'ROC')  # ROC-based evaluation
# saveRDS(mod_svmradial2, "./source/model/mod_svmradial2.rds")

# Predict values based on second, tuned SVM RBF model
testpred_svmradial2 <- predict(mod_svmradial2, 
                               newdata = svm_test_orig)

# Create confusion matrix for second, tuned SVM RBF model
confmtrx_svmradial2 <- confusionMatrix(testpred_svmradial2, 
                                       svm_test_orig$target)

# Plot ROC curve for second, tuned SVM RBF model
plot_svmradial2 <- ggplot(mod_svmradial2)


# Third SVM RBF model (synthetic data)

# Build third SVM RBF model with synthesized data
mod_svmradial3 <- readRDS("./source/model/mod_svmradial3.rds")
# mod_svmradial3 <- caret::train(target ~ .,
#                                data = svm_train_syn,
#                                method = 'svmRadial',  # RBF model 
#                                trControl = ctrl_svm2,  # cross-validation for ROC 
#                                preProcess = c('center', 'scale'),  # preprocess data to center and scale
#                                metric = 'ROC')  # ROC-based evaluation
# saveRDS(mod_svmradial3, "./source/model/mod_svmradial3.rds")

# Predict values based on third SVM RBF model with synthesized data
testpred_svmradial3 <- predict(mod_svmradial3, 
                               newdata = svm_test_syn)

# Create confusion matrix for third SVM RBF model with synthesized data
confmtrx_svmradial3 <- confusionMatrix(testpred_svmradial3, 
                                       svm_test_syn$target)

# Plot ROC curve for third SVM RBF model with synthesized data
plot_svmradial3 <- ggplot(mod_svmradial3)


# SVM Linear model

# Use grid search of tuning parameters for second SVM RBF model
tunegrid_svmlinear1 <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 
                                         0.5, 0.75, 1, 1.25, 1.5, 
                                         1.75, 2, 5))

# Build first SVM linear model with original data
mod_svmlinear1 <- readRDS("./source/model/mod_svmlinear1.rds")
# mod_svmlinear1 <- caret::train(target ~ .,  
#                                data = svm_train_syn,
#                                method = 'svmLinear',  # check and confirm, alternate = 'pls'
#                                trControl = ctrl_svm1,
#                                preProcess = c('center', 'scale'),  # check and confirm
#                                tuneGrid = tunegrid_svmlinear1,
#                                tunelength = 10) # check and confirm
# saveRDS(mod_svmlinear1, "./source/model/mod_svmlinear1.rds")

# Predict values based on linear SVM RBF model
testpred_svmlinear1 <- predict(mod_svmlinear1, 
                               newdata = svm_test_syn)

# Create confusion matrix for first SVM linear model
confmtrx_svmlinear1 <- confusionMatrix(testpred_svmlinear1, 
                                       svm_test_syn$target)

# Plot ROC curve for first SVM linear model with synthesized data
plot_svmlinear1 <- ggplot(mod_svmlinear1)


#======================================================================================#

## Model 5 - Naive Bayes Model (see notes)

#train data
orig_traindata_nb <- svm_train_orig
colnames(orig_traindata_nb)[colnames(orig_traindata_nb)=="ï..age"] <- "age"

syn_traindata_nb <- svm_train_syn
colnames(syn_traindata_nb)[colnames(syn_traindata_nb)=="ï..age"] <- "age"

#test data
orig_testdata_nb <- svm_test_orig
colnames(orig_testdata_nb)[colnames(orig_testdata_nb)=="ï..age"] <- "age"

syn_testdata_nb <- svm_test_syn
colnames(syn_testdata_nb)[colnames(syn_testdata_nb)=="ï..age"] <- "age"


#chol as category
nb.cat <- function(x, lower = 100, upper, by = 80,sep = "-", above.char = "+") {labs <- c(paste(seq(lower, upper - by, by = by),
                                                                                                seq(lower + by - 1, upper - 1, by = by),sep = sep),paste(upper, above.char, sep = ""))
cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf), right = FALSE, labels = labs)}

orig_traindata_nb$cholGroup <-  nb.cat(orig_traindata_nb$chol, upper = 350)
syn_traindata_nb$cholGroup <-  nb.cat(syn_traindata_nb$chol, upper = 350)         

orig_testdata_nb$cholGroup <-  nb.cat(orig_testdata_nb$chol, upper = 350)   
syn_testdata_nb$cholGroup <-  nb.cat(syn_testdata_nb$chol, upper = 350)   

orig_traindata_nb$chol <- NULL
syn_traindata_nb$chol <-  NULL     

orig_testdata_nb$chol <-  NULL 
syn_testdata_nb$chol <-  NULL 


#remove age and sex

orig_traindata_nb$age <-  NULL  
syn_traindata_nb$age <-  NULL  
orig_testdata_nb$age <-  NULL  
syn_testdata_nb$age <-  NULL  

orig_traindata_nb$sex <-  NULL  
syn_traindata_nb$sex <-  NULL  
orig_testdata_nb$sex <-  NULL  
syn_testdata_nb$sex <-  NULL  

model_orig_nb <- e1071::naiveBayes(target ~ ., data = orig_traindata_nb)
orig_testdata_nb$pred <- predict(model_orig_nb, orig_testdata_nb)
confusionMatrix_orig_nb <- caret::confusionMatrix(orig_testdata_nb$pred , orig_testdata_nb$target)

model_syn_nb <- e1071::naiveBayes(target ~ ., data = syn_traindata_nb)
syn_testdata_nb$pred <- predict(model_syn_nb, syn_testdata_nb)
confusionMatrix_syn_nb <- caret::confusionMatrix(syn_testdata_nb$pred , syn_testdata_nb$target)


#======================================================================================#


## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

