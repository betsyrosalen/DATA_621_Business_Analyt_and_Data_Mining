##############################
# MULTIPLE LINEAR REGRESSION #
##############################

# --------------------------- MODELS ---------------------------

# One of two Multiple Linear Regression models.
# Multiple R2 = 0.537. Adjusted R2 = 0.537. P-value = <0.0000000000000002.
# Mean of Residuals is 0. Sum of Residuals is 0.00000000000079.
# BIC of 43315
mlr_1 = lm(TARGET ~ AcidIndex + LabelAppeal + STARS, data=train_imputed)
# using train_plusmin has same results.
# using train_abslog has similar, lesser (0.536) results.
# using train_plusiqr15 has same results.

# Two of two Multiple Linear Regression models.
# Multiple R2 = 0.567. Adjusted R2 = 0.566. P-value = <0.0000000000000002.
# Mean of Residuals is -0.000000000000000049. Sum of Residuals is -0.00000000000062.
# BIC of 42650
mlr_2 = lm(TARGET ~ AcidIndex*LabelAppeal*STARS, data=train_imputed)
# using train_plusmin has same results.
# using train_abslog has similar, lesser (0.567 and 0.565) results.
# using train_plusiqr15 has same results.

# ---------------------------- PLOTS ----------------------------

# mlr_1 plot. Q-Q looks good.
mlr_1_plot = autoplot(mlr_1, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
  theme(panel.background=element_blank())

# mlr_2 plot. Q-Q doesn't look as good, but Residuals vs Leverage looks better.
mlr_2_plot = autoplot(mlr_1, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
  theme(panel.background=element_blank())

# ------------------------ PREDICTIONS -------------------------

mlr_1_predictions = predict(mlr_1, data=train_imputed)
mlr_1_predictions = as.factor(round(mlr_1_predictions))
levels(mlr_1_predictions) = sort(as.numeric(unique(c(-2, train_imputed$TARGET, levels(mlr_1_predictions)))))
mlr_1_confusion = confusionMatrix(mlr_1_predictions, reference=factor(train_imputed$TARGET, levels=levels(mlr_1_predictions)))

mlr_2_predictions = predict(mlr_2, data=train_imputed)
mlr_2_predictions = as.factor(round(mlr_2_predictions))
levels(mlr_2_predictions) = levels(mlr_1_predictions)
mlr_2_confusion = confusionMatrix(mlr_2_predictions, reference=factor(train_imputed$TARGET, levels=levels(mlr_2_predictions)))
