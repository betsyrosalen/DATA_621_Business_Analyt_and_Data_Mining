# One of two Multiple Linear Regression models.
# Multiple R2 = 0.537. Adjusted R2 = 0.537. P-value = <0.0000000000000002.
# Mean of Residuals is 0. Sum of Residuals is 0.00000000000079.
mlr_1 = lm(TARGET ~ AcidIndex + LabelAppeal + STARS, data=train_imputed)
