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
mlr_1_AIC = AIC(mlr_1)
mlr_1_BIC = BIC(mlr_1)

# Two of two Multiple Linear Regression models.
# Multiple R2 = 0.567. Adjusted R2 = 0.566. P-value = <0.0000000000000002.
# Mean of Residuals is -0.000000000000000049. Sum of Residuals is -0.00000000000062.
# BIC of 42650
mlr_2 = lm(TARGET ~ AcidIndex*LabelAppeal*STARS, data=train_imputed)
# using train_plusmin has same results.
# using train_abslog has similar, lesser (0.567 and 0.565) results.
# using train_plusiqr15 has same results.
mlr_2_AIC = AIC(mlr_2)
mlr_2_BIC = BIC(mlr_2)


# ------------------------ PREDICTIONS -------------------------

mlr_1_predictions = predict(mlr_1, data=train_imputed)
mlr_1_predictions = as.factor(round(mlr_1_predictions))
levels(mlr_1_predictions) = sort(as.numeric(unique(c(-2, train_imputed$TARGET, levels(mlr_1_predictions)))))
mlr_1_confusion = confusionMatrix(mlr_1_predictions, reference=factor(train_imputed$TARGET, levels=levels(mlr_1_predictions)))

mlr_2_predictions = predict(mlr_2, data=train_imputed)
mlr_2_predictions = as.factor(round(mlr_2_predictions))
levels(mlr_2_predictions) = levels(mlr_1_predictions)
mlr_2_confusion = confusionMatrix(mlr_2_predictions, reference=factor(train_imputed$TARGET, levels=levels(mlr_2_predictions)))


# ---------------------------- PLOTS ----------------------------

# mlr_1 plot. Q-Q looks good.
mlr_1_plot = autoplot(mlr_1, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
  theme(panel.background=element_blank())

# mlr_2 plot. Q-Q doesn't look as good, but Residuals vs Leverage looks better.
mlr_2_plot = autoplot(mlr_2, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
  theme(panel.background=element_blank())


# -------------------------- WRITE UP ---------------------------


# Multiple Linear Regression Models

# Finally, there was the need to explore the efficiency of Multiple Linear Regression in determining the `TARGET` variable for the dataset. Both models created utilized the same factors for determining the `TARGET` in slightly different ways.

# Multiple Linear Regression Model 1

# For the first model, it was previously discovered there was a definite interaction between the `TARGET` and the variables `AcidIndex`, `LabelAppeal`, and `STARS`. The latter of these two variables are factors and as a result have a lot of potential interactions. To start with, just the variables themselves were examined as potential influencers for determining the `TARGET`.

# ```{r f_, fig.cap="First Multiple Linear Regression Model's Summary"}
# summ(mlr_1)
# ```

# This model clearly shows the influence each variable has on the `TARGET`, although the efficiency comes into question. The $\text{R}^{2}$ is less-than-stellar and the same can be said of the Adjusted $\text{R}^{2}$. Additionally, this model has an AIC of `r mlr_1_AIC` and a BIC of `r mlr_1_BIC`. It is not the best model that has been seen so far, but it is the first multiple linear regression model to work with.

# Outside of its relatively atrocious performance in general, as a model, it is actually decent.

# ```{r f_, fig.cap="First Multiple Linear Regression Model's Plots"}
# mlr_1_plot
# ```

# The residuals are nearly even across the line for the Residuals vs. Fitted plot, and the Normal Q-Q plot shows the residuals are following neatly across the line, indicating they are normally distributed. The Scale-Location plot shows an interesting distribution, but on the whole, the line is nearly horizontal but the points are not necessarily random. The Cook's Distance plot shows there are a number of observed outliers that become more difficult to observe in the Residuals vs Leverage plot, and really stand out in the Cook's Distance vs Leverage plot. This, together, indicates a good model, but the numbers and Scale Location plot show this good model is just not good enough.

# Confusion Matrix

# ```{r f_, fig.cap="First Multiple Linear Regression Model's Confusion Matrix"}
# grid.table(mlr_1_confusion$table)
# ```
# 
# Another efficient way to consider the accuracy of a model is to look at its confusion matrix. Above are a variety of possible values for `TARGET` based off of the predictions and the actual `TARGET` values itself. This first model overall does not excel at properly predicting the values, as it did not guess a single value to be 6, 7, or 8, although there were observations of the `TARGET` variable with those values. Overall, the accuracy is `r mlr_1_confusion$overall[["Accuracy"]]`, which shows just how little this model should be trusted.
