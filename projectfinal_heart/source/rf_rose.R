registerDoMC(cores=8)
# Random Forest <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# seed <- 23
# # set predictor variable x, target variable y
# x <- orig_data[,1:13]
# y <- orig_data[,14]
# 

# Baseline model
rf.baseline <- readRDS("./source/model/rf_base.rds")
# rf.baseline <- train(target~., 
#                      data=orig_data, 
#                      method="rf", 
#                      metric="Accuracy", 
#                      tuneGrid=expand.grid(.mtry=sqrt(ncol(x))), 
#                      trControl=trainControl(method="repeatedcv", number=10, repeats=3))

#--------------------------------------------------------------------------------------

# Use syn_data as in-sample, orig_data as out-of-sample with cross validation using tuned hyper parameters
rf.syn.clf <- readRDS("./source/model/rf_syn_clf.rds")
rf.syn.clf.cv <- readRDS("./source/model/rf_syn_clf_cv.rds")

# set.seed(seed)
# split <- sample.split(syn_data$target, SplitRatio = 0.75)
# training_set <- subset(syn_data, split == TRUE)
# test_set <- subset(syn_data, split == FALSE)
# 
# ## Fitting classifier to the Training set
# rf.syn.clf <- randomForest(x = training_set[-14],
#                           y = training_set$target,
#                           ntree = 1000, # Insert tuned hyperparameters
#                           mtry = 1)
# #saveRDS(rf.syn.clf, "./model/rf_syn_clf.rds")
# ## cross validation
# rf.syn.clf.cv <- rf.crossValidation(rf.syn.clf, training_set[-14], p=0.10, n=100, ntree=1000)
#saveRDS(rf.syn.clf.cv, "./model/rf_syn_clf_cv.rds")

## Predicting the Test set results
syn.y.pred <- predict(rf.syn.clf, newdata = orig_data[-14])
syn.cm <- table(orig_data$target, syn.y.pred)

#--------------------------------------------------------------------------------------

# Use orig_data as in-sample and out-of-sample after split with cross validation using tuned hyper parameters
rf.org.clf <- readRDS("./source/model/rf_org_clf.rds")
rf.org.clf.cv <- readRDS("./source/model/rf_org_clf_cv.rds")
# 
# ## split data
# set.seed(seed)
# split <- sample.split(orig_data$target, SplitRatio = 0.75)
# training_set <- subset(orig_data, split == TRUE)
# test_set <- subset(orig_data, split == FALSE)
# 
# ## Fitting classifier to the Training set
# rf.org.clf = randomForest(x = training_set[-14],
#                           y = training_set$target, 
#                           ntree = 1000, # Insert tuned hyperparameters
#                           mtry = 1)
# saveRDS(rf.org.clf, "./model/rf_org_clf.rds")
# rf.org.clf.cv <- rf.crossValidation(rf.org.clf, training_set[-14], p=0.10, n=100, ntree=1000)
# saveRDS(rf.org.clf.cv, "./model/rf_org_clf_cv.rds")
## Predicting the Test set results
org.y.pred <- predict(rf.org.clf, newdata = orig_data[-14])
org.cm <- table(orig_data$target, org.y.pred)

#--------------------------------------------------------------------------------------

# Confusion matrix of prediction
rf.org.cm <- caret::confusionMatrix(table(predicted=as.numeric(levels(org.y.pred)[as.integer(org.y.pred)]), 
                                          actual = orig_data$target), mode = 'everything')

rf.syn.cm <- caret::confusionMatrix(table(predicted=as.numeric(levels(syn.y.pred)[as.integer(syn.y.pred)]), 
                                          actual = orig_data$target), mode = 'everything')
eval_mods <- data.frame(rf.org.cm$byClass, 
                          rf.syn.cm$byClass)
eval_mods <- data.frame(t(eval_mods))
row.names(eval_mods) <- c("RF Original Tuned", 
                          "RF Synthesized Tuned")
eval_mods <- dplyr::select(eval_mods, Sensitivity, Specificity, Precision, Recall, F1)

# ROC graph
ROCRPred.org <- prediction(as.numeric(levels(org.y.pred)[as.integer(org.y.pred)]), 
                           orig_data$target)
ROCRPref.org <- performance(ROCRPred.org, 'tpr', 'fpr')

ROCRPred.syn <- prediction(as.numeric(levels(syn.y.pred)[as.integer(syn.y.pred)]), 
                           orig_data$target)
ROCRPref.syn <- performance(ROCRPred.syn, 'tpr', 'fpr')
#--------------------------------------------------------------------------------------

# Hyper parameter tuning using Orig_data

## Using Caret Random Search
rf.random <- readRDS("./source/model/hp_rf_random.rds")
# rf.random <- train(target~., 
#                    data=orig_data, 
#                    method="rf", 
#                    metric="Accuracy", 
#                    tuneLength=15, 
#                    trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="random"),
#                    preProcess = c("center", "scale"))
# saveRDS(rf.random, "./model/hp_rf_random.rds")
#--------------------------------------------------------------------------------------
## Grid Search
rf.gridsearch <- readRDS("./source/model/hp_rf_grid.rds")
# set.seed(seed)
# rf.gridsearch <- train(target~.,
#                        data=orig_data,
#                        method="rf",
#                        metric='Accuracy',
#                        tuneGrid=expand.grid(.mtry=c(1:15)),
#                        trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="grid"),
#                        preProcess = c("center", "scale"))
# saveRDS(rf.gridsearch, "./model/hp_rf_grid_1.rds")
#--------------------------------------------------------------------------------------
## Tuning using algorithm tools
bestmtry <- readRDS("./source/model/bestmtry.rds")
# bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
# saveRDS(bestmtry, "./model/bestmtry.rds")
#--------------------------------------------------------------------------------------
## Craft your own - manually
manual.rf <- readRDS("./source/model/hp_manual.rds")
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(target~., data=orig_data, 
#                method="rf", 
#                metric='Accuracy', 
#                tuneGrid=expand.grid(.mtry=c(sqrt(ncol(x)))), 
#                trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="grid"),
#                preProcess = c("center", "scale"),
#                ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# ### compare results
# manual.rf <- resamples(modellist)
# saveRDS(manual.rf, "./model/hp_manual.rds")
#--------------------------------------------------------------------------------------
## Extend Caret Custom
custom.result <- readRDS("./source/model/hp_custom.rds")
# custom.rf <- list(type = "Classification", library = "randomForest", loop = NULL)
# 
# custom.rf$parameters <- data.frame(parameter = c("mtry", "ntree"), 
#                                    class = rep("numeric", 2), 
#                                    label = c("mtry", "ntree"))
# 
# custom.rf$grid <- function(x, y, len = NULL, search = "grid") {}
# 
# custom.rf$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# 
# custom.rf$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# custom.rf$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# 
# custom.rf$sort <- function(x) x[order(x[,1]),]
# custom.rf$levels <- function(x) x$classes
# 
# set.seed(seed)
# custom.result <- train(target~., 
#                        data=orig_data, 
#                        method=custom.rf, 
#                        metric='Accuracy', 
#                        tuneGrid=expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500)), 
#                        trControl=trainControl(method="repeatedcv", number=10, repeats=3))
# saveRDS(custom.result, "./model/hp_custom.rds")
#--------------------------------------------------------------------------------------
# Hyper Parameter Tuning using syn_data <<<<<<<<<<<<<<<<<< TOO SLOW!

# ## Using Caret Random Search
# rf.random.syn <- train(target~., 
#                    data=syn_data, 
#                    method="rf", 
#                    metric="Accuracy", 
#                    tuneLength=15, 
#                    trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="random"),
#                    preProcess = c("center", "scale"))
# 
# ## Grid Search
# set.seed(seed)
# rf.gridsearch.syn <- train(target~., 
#                        data=syn_data, 
#                        method="rf", 
#                        metric=metric, 
#                        tuneGrid=expand.grid(.mtry=c(1:15)), 
#                        trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="grid"),
#                        preProcess = c("center", "scale"))
# 
# ## Tuning using algorithm tools
# bestmtry.syn <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
# 
# ## Craft your own - manually
# modellist.syn <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(target~., data=syn_data, 
#                method="rf", 
#                metric=metric, 
#                tuneGrid=expand.grid(.mtry=c(sqrt(ncol(x)))), 
#                trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="grid"),
#                preProcess = c("center", "scale"),
#                ntree=ntree)
#   key <- toString(ntree)
#   modellist.syn[[key]] <- fit
# }
# ### compare results
# manual.rf.syn <- resamples(modellist.syn)
# 
# ## Extend Caret Custom
# custom.rf.syn <- list(type = "Classification", library = "randomForest", loop = NULL)
# 
# custom.rf.syn$parameters <- data.frame(parameter = c("mtry", "ntree"), 
#                                    class = rep("numeric", 2), 
#                                    label = c("mtry", "ntree"))
# 
# custom.rf.syn$grid <- function(x, y, len = NULL, search = "grid") {}
# 
# custom.rf.syn$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# 
# custom.rf.syn$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# custom.rf.syn$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# 
# custom.rf.syn$sort <- function(x) x[order(x[,1]),]
# custom.rf.syn$levels <- function(x) x$classes
# 
# set.seed(seed)
# custom.result.syn <- train(target~., 
#                        data=syn_data, 
#                        method=custom.rf.syn, 
#                        metric=metric, 
#                        tuneGrid=expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500)), 
#                        trControl=trainControl(method="repeatedcv", number=10, repeats=3))

rf.org.cm2 <- caret::confusionMatrix(org.y.pred, reference=orig_data$target)
rf.syn.cm2 <- caret::confusionMatrix(syn.y.pred, reference=orig_data$target)

rf_compared <- data.frame(t(rf.org.cm2$overall))
rf_compared <- rbind(rf_compared, data.frame(t(rf.syn.cm2$overall)))

rownames(rf_compared) <- c("orig", "syn")
rf_compared <- rf_compared[order(-rf_compared$Accuracy),]

