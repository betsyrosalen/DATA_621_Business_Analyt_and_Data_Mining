# One of two Multiple Linear Regression models. Accuracy of 0.455
# If the data replaces the NAs for STARS with "Not Given", accuracy goes up to 0.537
mlr_1 = lm(TARGET ~ AcidIndex + LabelAppeal + as.factor(STARS), data=train[!is.na(train$STARS),])
