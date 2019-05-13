# train_imputed model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# pois.mod1 <- glm(TARGET~., fam = poisson, d = train_imputed)
# summary(pois.mod1)

# train_plusmin model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pois.mod2.null = glm(TARGET ~ 1, family = "poisson", data = train_plusmin)

pois.mod2 <- glm(TARGET~., fam = poisson, d = train_plusmin)
summary(pois.mod2)

# pois.mod2.a <- glm(TARGET~., fam = poisson, d = train_imputed)
# summary(pois.mod2.a)
# pois.mod2.b <- glm(TARGET~., fam = quasipoisson, d = train_imputed)
# summary(pois.mod2.b)
#
# se <- function(model) sqrt(diag(vcov(model)))
#
# round(data.frame('poisson'=coef(pois.mod2.a), 'quasip'=coef(pois.mod2.b), 'se.poiss'=se(pois.mod2.a), 'se.quasi'=se(pois.mod2.b), 'ratio'=se(pois.mod2.b)/se(pois.mod2.a)), 4)


# train_plusiqr15 model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# pois.mod3 <- glm(TARGET~., fam = poisson, d = train_plusiqr15)
# summary(pois.mod3)
#
# train_abslog model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# pois.mod4 <- glm(TARGET~., fam = poisson, d = train_abslog)
# summary(pois.mod4)

# REFINEMENT >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# forward.pois.mod2 <- step(pois.mod2, direction = "forward", trace=FALSE)
# backward.pois.mod2 <- step(pois.mod2, direction = "backward", trace=FALSE)

step.pois.mod2 <- step(pois.mod2.null, scope = list(upper=pois.mod2),
     direction = "both", data = data, trace=FALSE)

drop1 <- drop1(step.pois.mod2, test="F")

# got rid of Ph from step  model
mod.6 <- glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity +
                   TotalSulfurDioxide + Alcohol + FreeSulfurDioxide + Sulphates +
                   Chlorides, fam = poisson, d = train_plusmin)
summary(mod.6)

ocount <- table(train_plusmin$TARGET)[1:9]
pcount <- colSums(predprob(mod.6)[,1:9])
plot(pcount,ocount,type="n",xlab="Predicted",ylab="Observed") +
text(pcount,ocount, 0:8)

hist(train[train[, 'TARGET']==0, 'STARS'])
hist(train[train[, 'STARS']==0, 'TARGET'])

# Hurdle Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

hurd.mod2 <- hurdle(TARGET~., data = train_plusmin)
summary(hurd.mod2)

hurd.mod3 <- hurdle(TARGET~Alcohol + LabelAppeal + AcidIndex + STARS |
                        VolatileAcidity + FreeSulfurDioxide +
                        TotalSulfurDioxide + pH + Sulphates + Alcohol +
                        LabelAppeal + AcidIndex + STARS,
                    data = train_plusmin)
summary(hurd.mod3)

# hurd.mod4 <- hurdle(TARGET~., data = train_plusmin, dist = "negbin")
# Causes this error...
# Error in optim(fn = countDist, gr = countGrad, par = c(start$count, if (dist ==  :
#                             non-finite value supplied by optim

# from :https://data.library.virginia.edu/getting-started-with-hurdle-models/
# Need to install from R-Forge instead of CRAN
# install.packages("countreg", repos="http://R-Forge.R-project.org")
# library(countreg)
rootogram(pois.mod2)
rootogram(step.pois.mod2)
rootogram(mod.6)
rootogram(hurd.mod2)
rootogram(hurd.mod3)

comparison <- AIC(pois.mod2, step.pois.mod2, mod.6, hurd.mod2, hurd.mod3)

# Zero Inflated Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

### CAN'T GET THIS TO WORK!!!

# train_plusmin$STARS_bins <- as.numeric(train_plusmin$STARS)
# train_plusmin$STARS_bins <- ifelse(train_plusmin$STARS_bins < 3, 0, 1)

# less_than_three <- function(x) ifelse(x < 3, x, 0)
# three_and_over <- function(x) ifelse(x >= 3, x, 0)
# (less_than_three(as.numeric(STARS)) +
#         three_and_over(as.numeric(STARS))

# zi.mod2 <- zeroinfl(TARGET~.| FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+
#                         Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Density+pH+
#                         Sulphates+Alcohol+LabelAppeal+AcidIndex+STARS_bins,
#                     data = train_plusmin)

# zi.mod2 <- zeroinfl(TARGET~.| VolatileAcidity + FreeSulfurDioxide +
#                         TotalSulfurDioxide + pH + Sulphates + Alcohol +
#                         LabelAppeal + AcidIndex + STARS,
#                     data = train_plusmin)

# summary(zi.mod2)

# SUMMARY, PLOT, PREDICT >>>>>s>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mod6_summary <- summ(mod.6, vifs = TRUE)

mod6_plot <- autoplot(mod.6, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
    theme(panel.background=element_blank())

### Model 6 Predictions
pred.6.raw <- predict(mod.6, newdata = train_plusmin)



# Call:
#     glm(formula = TARGET ~ ., family = poisson, data = train_imputed)
#
# Deviance Residuals:
#     Min       1Q   Median       3Q      Max
# -3.2276  -0.6532  -0.0032   0.4532   3.7741
#
# Coefficients:
#                         Estimate   Std. Error  z value Pr(>|z|)
#     (Intercept)         6.977e-01  1.989e-01   3.507 0.000453 ***
#     FixedAcidity        4.967e-05  8.202e-04   0.061 0.951709
#     VolatileAcidity    -3.052e-02  6.529e-03  -4.675 2.94e-06 ***
#     CitricAcid          5.037e-03  5.898e-03   0.854 0.393043
#     ResidualSugar       4.672e-05  1.511e-04   0.309 0.757217
#     Chlorides          -4.241e-02  1.599e-02  -2.652 0.008009 **
#     FreeSulfurDioxide   8.898e-05  3.418e-05   2.603 0.009239 **
#     TotalSulfurDioxide  7.781e-05  2.209e-05   3.522 0.000428 ***
#     Density            -2.655e-01  1.918e-01  -1.384 0.166231
#     pH                 -1.247e-02  7.530e-03  -1.656 0.097762 .
#     Sulphates          -1.199e-02  5.488e-03  -2.185 0.028870 *
#     Alcohol             3.641e-03  1.374e-03   2.650 0.008047 **
#     LabelAppeal-1       2.362e-01  3.799e-02   6.218 5.04e-10 ***
#     LabelAppeal0        4.266e-01  3.705e-02  11.513  < 2e-16 ***
#     LabelAppeal1        5.590e-01  3.770e-02  14.829  < 2e-16 ***
#     LabelAppeal2        6.970e-01  4.244e-02  16.425  < 2e-16 ***
#     AcidIndex          -7.977e-02  4.573e-03 -17.442  < 2e-16 ***
#     STARS1              7.661e-01  1.954e-02  39.201  < 2e-16 ***
#     STARS2              1.085e+00  1.824e-02  59.477  < 2e-16 ***
#     STARS3              1.205e+00  1.920e-02  62.740  < 2e-16 ***
#     STARS4              1.324e+00  2.431e-02  54.469  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
# Null deviance: 22861  on 12794  degrees of freedom
# Residual deviance: 13634  on 12774  degrees of freedom
# AIC: 45618
#
# Number of Fisher Scoring iterations: 6