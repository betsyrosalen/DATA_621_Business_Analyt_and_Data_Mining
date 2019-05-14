
# DATA SET TO USE::
## |--------------------------------------------------------------
## | 1. Data as is :                    `train_imputed`
## | 2. Data by shifted by min value:  `train_plusmin`
## | 3. Data by Jeremy's method:       `train_plusiqr15`
## | 4. Data by ABS and Log:            `train_abslog`
## |--------------------------------------------------------------


# Model 1:

neg.bin.imputed <- glm.nb(TARGET ~ ., data=train_imputed)
neg.bin.min <- glm.nb(TARGET ~ ., data=train_plusmin)
neg.min.iqr <- glm.nb(TARGET ~ ., data=train_plusiqr15)
neg.min.abslog <- glm.nb(TARGET ~ ., data=train_abslog)


# Model 3: Zero Dispersion Counts

zero.infl.imputed <- zeroinfl(TARGET ~ . |STARS, data=train_imputed, dist="negbin")
#zero.infl.min <- zeroinfl(TARGET ~ . |STARS, data=train_plusmin, dist="negbin")
#zero.infl.iqr <- zeroinfl(TARGET ~ . |STARS, data=train_plusiqr15, dist="negbin")
#zero.infl.abslog <- zeroinfl(TARGET ~ . |STARS, data=train_abslog, dist="negbin")


# Evaluate

# MAE, RMSE


# # Prediction:
# final.pred <- predict(zero.infl.1, test) 
# final.df <- cbind(TARGET_FLAG=final.pred)
# hist(final.pred)
# 
# # Export:
# write.csv(final.df, 'wine_pred.csv', row.names = FALSE)
# 
