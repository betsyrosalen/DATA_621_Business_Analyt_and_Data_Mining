set.seed(123)
# Create test and train datasets
lr_train_flag_orig <- caret::createDataPartition(orig_data$target, 
                                              p = .75, 
                                              list = FALSE)
lr_svm_train_orig <- orig_data[train_flag_orig,]
lr_svm_test_orig <- orig_data[-train_flag_orig,]

lr_train_flag_syn <- caret::createDataPartition(syn_data$target, 
                                             p = .75, 
                                             list = FALSE)
lr_svm_train_syn <- syn_data[lr_train_flag_syn,]
lr_svm_test_syn <- syn_data[-lr_train_flag_syn,]

#############
# ORIG_DATA #
#############

# Summary of variables for orig_data
# 14:48:01> summary(lm(as.numeric(target)~., orig_data))
# 
# Call:
#   lm(formula = as.numeric(target) ~ ., data = orig_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.01468 -0.18519  0.03069  0.22575  1.02624 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.4632832  0.3688720   3.967 9.26e-05 ***
#   age          0.0027129  0.0026488   1.024 0.306638    
# sex1        -0.1640717  0.0482271  -3.402 0.000766 ***
#   cp1          0.1688621  0.0639741   2.640 0.008767 ** 
#   cp2          0.2252316  0.0537885   4.187 3.78e-05 ***
#   cp3          0.2657100  0.0811006   3.276 0.001184 ** 
#   trestbps    -0.0023401  0.0012187  -1.920 0.055844 .  
# chol        -0.0003189  0.0004021  -0.793 0.428416    
# fbs1         0.0333463  0.0574909   0.580 0.562362    
# restecg1     0.0469891  0.0408340   1.151 0.250823    
# restecg2    -0.0870584  0.1757245  -0.495 0.620689    
# thalach      0.0018708  0.0011189   1.672 0.095634 .  
# exang1      -0.0935220  0.0499945  -1.871 0.062437 .  
# oldpeak     -0.0415842  0.0230281  -1.806 0.072023 .  
# slope1      -0.0653951  0.0859299  -0.761 0.447281    
# slope2       0.0736023  0.0950718   0.774 0.439480    
# ca1         -0.2703534  0.0530757  -5.094 6.46e-07 ***
#   ca2         -0.3426057  0.0682472  -5.020 9.20e-07 ***
#   ca3         -0.2960027  0.0861537  -3.436 0.000680 ***
#   ca4          0.0443264  0.1555910   0.285 0.775939    
# thal1        0.2283726  0.2543715   0.898 0.370068    
# thal2        0.2879971  0.2418275   1.191 0.234694    
# thal3        0.0784919  0.2436939   0.322 0.747623    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.335 on 280 degrees of freedom
# Multiple R-squared:  0.5818,	Adjusted R-squared:  0.549 
# F-statistic: 17.71 on 22 and 280 DF,  p-value: < 2.2e-16

# 14:52:43> summ(lm(as.numeric(target) ~ sex + ca + cp, lr_svm_train_orig))
# MODEL FIT:
# F(11,231) = 16.20, p = 0.00
# R2 = 0.43
# Adj. R2 = 0.41 
# AIC = 235.6967  BIC = 270.6273
lm_select1_orig <- lm(as.numeric(target) ~ sex + ca + cp, lr_svm_train_orig)
lm_select1_pred_orig <- as.factor(round(predict(lm_select1_orig, lr_svm_test_orig))-1)
lm_select1_pred_orig_confusion <- confusionMatrix(lm_select1_pred_orig, reference=lr_svm_test_orig$target)
lm_select1_pred_origXsyn <- as.factor(round(predict(lm_select1_orig, lr_svm_test_syn))-1)
lm_select1_pred_origXsyn_confusion <- confusionMatrix(lm_select1_pred_origXsyn, reference=lr_svm_test_syn$target)

# 15:34:44> summ(lm(as.numeric(target) ~ sex * ca + cp, lr_svm_train_orig))
# MODEL FIT:
# F(8,234) = 21.78, p = 0.00
# R2 = 0.44
# Adj. R2 = 0.41 
# AIC = 237.9943  BIC = 283.4041
lm_select2_orig <- lm(as.numeric(target) ~ sex * ca + cp, lr_svm_train_orig)
lm_select2_pred_orig <- as.factor(round(predict(lm_select2_orig, lr_svm_test_orig))-1)
lm_select2_pred_orig_confusion <- confusionMatrix(lm_select2_pred_orig, reference=lr_svm_test_orig$target)
lm_select2_pred_origXsyn <- as.factor(round(predict(lm_select2_orig, lr_svm_test_syn))-1)
lm_select2_pred_origXsyn_confusion <- confusionMatrix(lm_select2_pred_origXsyn, reference=lr_svm_test_syn$target)

# 15:36:36> summ(lm(as.numeric(target)~., lr_svm_train_orig[sapply(lr_svm_train_orig, is.numeric)|names(lr_svm_train_orig)=="target"]))
# MODEL FIT:
# F(5,237) = 18.98, p = 0.00
# R2 = 0.29
# Adj. R2 = 0.27 
# AIC = 283.0819  BIC = 307.5334
lm_numeric_orig <- lm(as.numeric(target)~., lr_svm_train_orig[sapply(lr_svm_train_orig, is.numeric)|names(lr_svm_train_orig)=="target"])
#lm_numeric_pred_origXsyn <- as.factor(round(predict(lm_numeric_orig, lr_svm_test_syn))-1)
lm_numeric_pred_orig <- as.factor(sapply(round(predict(lm_numeric_orig, lr_svm_test_orig))-1, function (x) ifelse(x < 0, 0, ifelse(x>1, 1, x)))) # if there is an error about more levels, this is where you fix it.
lm_numeric_pred_orig_confusion <- confusionMatrix(lm_numeric_pred_orig, reference=lr_svm_test_orig$target)
lm_numeric_pred_origXsyn <- as.factor(sapply(round(predict(lm_numeric_orig, lr_svm_test_syn))-1, function (x) ifelse(x < 0, 0, ifelse(x>1, 1, x)))) 
lm_numeric_pred_origXsyn_confusion <- confusionMatrix(lm_numeric_pred_origXsyn, reference=lr_svm_test_syn$target)

# 15:38:03> summ(lm(as.numeric(target)~., lr_svm_train_orig[!sapply(lr_svm_train_orig, is.numeric)|names(lr_svm_train_orig)=="target"]))
# MODEL FIT:
# F(17,225) = 17.73, p = 0.00
# R2 = 0.57
# Adj. R2 = 0.54 
# AIC = 182.3636  BIC = 248.7317
lm_factors_orig <- lm(as.numeric(target)~., lr_svm_train_orig[!sapply(lr_svm_train_orig, is.numeric)|names(lr_svm_train_orig)=="target"])
lm_factors_pred_orig <- as.factor(round(predict(lm_factors_orig, lr_svm_test_orig))-1)
lm_factors_pred_orig_confusion <- confusionMatrix(lm_factors_pred_orig, reference=lr_svm_test_orig$target)
lm_factors_pred_orig_confusion2 <- confusionMatrix(lm_factors_pred_orig, reference=lr_svm_test_orig$target, mode = "prec_recall")
lm_factors_pred_origXsyn <- as.factor(round(predict(lm_factors_orig, lr_svm_test_syn))-1)
lm_factors_pred_origXsyn_confusion <- confusionMatrix(lm_factors_pred_origXsyn, reference=lr_svm_test_syn$target)


############
# SYN_DATA #
############

# Summary of variables for syn_data
# 15:40:07> summary(lm(as.numeric(target)~., syn_data))
# Call:
#   lm(formula = as.numeric(target) ~ ., data = syn_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.10137 -0.26218  0.01789  0.25941  1.16579 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.017e+00  9.072e-02  22.234  < 2e-16 ***
#   age         -6.828e-03  6.410e-04 -10.652  < 2e-16 ***
#   sex1        -1.412e-01  1.184e-02 -11.924  < 2e-16 ***
#   cp1          3.300e-01  1.563e-02  21.120  < 2e-16 ***
#   cp2          3.121e-01  1.313e-02  23.774  < 2e-16 ***
#   cp3          2.801e-01  2.033e-02  13.776  < 2e-16 ***
#   trestbps    -6.030e-04  3.017e-04  -1.999 0.045686 *  
#   chol        -3.261e-04  9.885e-05  -3.299 0.000976 ***
#   fbs1         1.844e-02  1.373e-02   1.343 0.179344    
# restecg1     1.844e-02  1.020e-02   1.808 0.070616 .  
# restecg2     3.291e-02  4.748e-02   0.693 0.488239    
# thalach      7.416e-04  2.721e-04   2.726 0.006435 ** 
#   exang1      -1.792e-02  1.228e-02  -1.459 0.144678    
# oldpeak     -6.553e-03  5.560e-03  -1.179 0.238598    
# slope1       4.967e-02  1.924e-02   2.582 0.009851 ** 
#   slope2       6.780e-02  2.203e-02   3.077 0.002099 ** 
#   ca1         -2.220e-01  1.266e-02 -17.534  < 2e-16 ***
#   ca2         -2.460e-01  1.603e-02 -15.341  < 2e-16 ***
#   ca3         -2.615e-01  2.147e-02 -12.176  < 2e-16 ***
#   ca4         -2.183e-01  3.605e-02  -6.057 1.47e-09 ***
#   thal1       -3.547e-02  5.881e-02  -0.603 0.546399    
# thal2       -1.230e-02  5.590e-02  -0.220 0.825896    
# thal3       -2.212e-01  5.622e-02  -3.935 8.42e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3745 on 6037 degrees of freedom
# Multiple R-squared:  0.4397,	Adjusted R-squared:  0.4377 
# F-statistic: 215.4 on 22 and 6037 DF,  p-value: < 2.2e-16

# 15:40:23> summ(lm(as.numeric(target) ~ age + sex + chol + ca + cp, lr_svm_train_syn))
# MODEL FIT:
# F(10,4837) = 317.00, p = 0.00
# R2 = 0.40
# Adj. R2 = 0.39  
# AIC = 4605.077  BIC = 4682.913
lm_select_syn <- lm(as.numeric(target) ~ age + sex + chol + ca + cp, lr_svm_train_syn)
lm_select_pred_syn <- as.factor(round(predict(lm_select_syn, lr_svm_test_syn))-1)
lm_select_pred_syn_confusion <- confusionMatrix(lm_select_pred_syn, reference=lr_svm_test_syn$target)
lm_select_pred_synXorig <- as.factor(round(predict(lm_select_syn, lr_svm_test_orig))-1)
lm_select_pred_synXorig_confusion <- confusionMatrix(lm_select_pred_synXorig, reference=lr_svm_test_orig$target)

# summ(lm(as.numeric(target)~., lr_svm_train_syn[sapply(lr_svm_train_syn, is.numeric)|names(lr_svm_train_syn)=="target"]))
# MODEL FIT:
# F(5,4842) = 209.21, p = 0.00
# R2 = 0.18
# Adj. R2 = 0.18
# AIC = 6090.317  BIC = 6135.722
lm_numeric_syn <- lm(as.numeric(target)~., lr_svm_train_syn[sapply(lr_svm_train_syn, is.numeric)|names(lr_svm_train_syn)=="target"])
lm_numeric_pred_syn <- as.factor(round(predict(lm_numeric_syn, lr_svm_test_syn))-1)
lm_numeric_pred_syn_confusion <- confusionMatrix(lm_numeric_pred_syn, reference=lr_svm_test_syn$target)
lm_numeric_pred_synXorig <- as.factor(round(predict(lm_numeric_syn, lr_svm_test_orig))-1)
lm_numeric_pred_synXorig_confusion <- confusionMatrix(lm_numeric_pred_synXorig, reference=lr_svm_test_orig$target)

# 15:47:08> summ(lm(as.numeric(target)~., lr_svm_train_syn[!sapply(lr_svm_train_syn, is.numeric)|names(lr_svm_train_syn)=="target"]))
# MODEL FIT:
# F(17,4830) = 206.19, p = 0.00
# R2 = 0.42
# Adj. R2 = 0.42
# AIC = 4417.274  BIC = 4540.515
lm_factors_syn <- lm(as.numeric(target)~., lr_svm_train_syn[!sapply(lr_svm_train_syn, is.numeric)|names(lr_svm_train_syn)=="target"])
lm_factors_pred_syn <- as.factor(round(predict(lm_factors_syn, lr_svm_test_syn))-1)
lm_factors_pred_syn_confusion <- confusionMatrix(lm_factors_pred_syn, reference=lr_svm_test_syn$target)
lm_factors_pred_syn_confusion2 <- confusionMatrix(lm_factors_pred_syn, reference=lr_svm_test_syn$target, mode = "prec_recall")
lm_factors_pred_synXorig <- as.factor(round(predict(lm_factors_syn, lr_svm_test_orig))-1)
lm_factors_pred_synXorig_confusion <- confusionMatrix(lm_factors_pred_synXorig, reference=lr_svm_test_orig$target)

lr_compared <- data.frame(t(lm_select1_pred_orig_confusion$overall))
lr_compared <- rbind(lr_compared, data.frame(t(lm_select1_pred_origXsyn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_select2_pred_orig_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_select2_pred_origXsyn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_numeric_pred_orig_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_numeric_pred_origXsyn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_factors_pred_orig_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_factors_pred_origXsyn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_select_pred_syn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_select_pred_synXorig_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_numeric_pred_syn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_numeric_pred_synXorig_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_factors_pred_syn_confusion$overall)))
lr_compared <- rbind(lr_compared, data.frame(t(lm_factors_pred_synXorig_confusion$overall)))
rownames(lr_compared) <- c("select1_o", "select1_oXs", "select2_o", "select2_oXs",
                           "numeric_o", "numeric_oXs", "factors_o", "factors_oXs",
                           "select_s", "select_sXo", "numeric_s", "numeric_sXo",
                           "factors_s", "factors_sXo")
lr_compared <- lr_compared[order(-lr_compared$Accuracy),]

# > lr_compared
#             Accuracy     Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue
# factors_o   0.8666667 0.7342193     0.7540778     0.9406356    0.5500000   1.652878e-07  2.888444e-01
# factors_sXo 0.8666667 0.7342193     0.7540778     0.9406356    0.5500000   1.652878e-07  2.888444e-01
# factors_s   0.8110561 0.6195429     0.7878601     0.8327318    0.5255776   8.832341e-96  1.514299e-03
# select1_o   0.8000000 0.5959596     0.6766996     0.8921589    0.5500000   4.669544e-05  1.000000e+00
# select2_o   0.8000000 0.5959596     0.6766996     0.8921589    0.5500000   4.669544e-05  1.000000e+00
# factors_oXs 0.7970297 0.5925115     0.7732649     0.8193437    0.5255776   4.140217e-86  4.071894e-01
# select_s    0.7970297 0.5934073     0.7732649     0.8193437    0.5255776   4.140217e-86  4.830944e-01
# select1_oXs 0.7846535 0.5668204     0.7604220     0.8074952    0.5255776   4.158957e-78  2.585758e-02
# select2_oXs 0.7846535 0.5668204     0.7604220     0.8074952    0.5255776   4.158957e-78  2.585758e-02
# select_sXo  0.7833333 0.5695364     0.6580438     0.8792840    0.5500000   1.471654e-04  2.672575e-01
# numeric_o   0.7166667 0.4137931     0.5855639     0.8254947    0.5500000   6.083096e-03  1.456101e-01
# numeric_s   0.6633663 0.3226732     0.6359777     0.6899598    0.5255776   1.727546e-22  5.350931e-02
# numeric_oXs 0.6559406 0.3030564     0.6284295     0.6826927    0.5255776   2.755634e-20  7.537632e-09
# numeric_sXo 0.6166667 0.2229730     0.4821150     0.7392929    0.5500000   1.821414e-01  1.000000e+00

##############
# BEST MODEL #
##############

lr_plots <- autoplot(lm_factors_orig, which = 1:6, colour = "#58BFFF",
                                  smooth.colour = 'red', smooth.linetype = 'solid',
                                  ad.colour = 'black',
                                  label.size = 3, label.n = 5, label.colour = "#3300FF",
                                  ncol = 2) +
  theme(panel.background=element_blank())
