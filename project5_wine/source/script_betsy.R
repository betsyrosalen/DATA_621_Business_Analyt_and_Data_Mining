set.seed(123)

# train_imputed model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pois.mod.2 <- glm(TARGET~., fam = poisson, d = train_imputed)
summary(pois.mod.2)

# train_plusiqr15 model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pois.mod.3 <- glm(TARGET~., fam = poisson, d = train_plusiqr15)
summary(pois.mod.3)

# train_abslog model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pois.mod.4 <- glm(TARGET~., fam = poisson, d = train_abslog)
summary(pois.mod.4)

# train_plusmin model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pois.mod.1 <- glm(TARGET~., fam = poisson, d = train_plusmin)
summary(pois.mod.1)

data.prep.comparison <- as.data.frame(cbind(pois.mod.2$coefficients,pois.mod.1$coefficients,
      pois.mod.3$coefficients,pois.mod.4$coefficients))
prepnames <- c('imputed only', 'plus min', 'plus iqr1.5', 'abs log')
colnames(data.prep.comparison) <- prepnames
data.prep.comparison$range <- apply(data.prep.comparison, 1, max) - apply(data.prep.comparison, 1, min)

data.prep.AICs <- AIC(pois.mod.2,pois.mod.1,pois.mod.3,pois.mod.4)
rownames(data.prep.AICs) <- prepnames
data.prep.AICs

##### THERE WAS NO REAL DIFFERENCE IN THE MODEL FOR THE 4 DIFFERENT DATA #######
##### PREPARATIONS SO I WENT WITH THE ONE THAT MADE MOST SENSE TO ME ###########

pois.mod.null = glm(TARGET ~ 1, family = "poisson", data = train_plusmin)


#-------------------------------------------------------------------------------
# REFINEMENT >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# forward.pois.mod.1 <- step(pois.mod.1, direction = "forward", trace=FALSE)
# backward.pois.mod.1 <- step(pois.mod.1, direction = "backward", trace=FALSE)

step.pois.mod.1 <- step(pois.mod.null, scope = list(upper=pois.mod.1),
     direction = "both", data = data, trace=FALSE)

drop1.pois.mod.1 <- drop1(step.pois.mod.1, test="F")
drop1.pois.mod.1

# got rid of Ph from step  model
pois.mod.2 <- glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity +
                   TotalSulfurDioxide + Alcohol + FreeSulfurDioxide + Sulphates +
                   Chlorides, fam = poisson, d = train_plusmin)
summary(pois.mod.2)

anova(pois.mod.2, test="Chisq")

pois2.influenceplot <- car::influencePlot(pois.mod.2)

minusinfluential <- train_plusmin[-c(3953, 4940, 8887, 10108, 12513),]

pois.mod.3 <- glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity +
                      TotalSulfurDioxide + Alcohol + FreeSulfurDioxide + Sulphates +
                      Chlorides, fam = poisson, d = minusinfluential)

summary(pois.mod.3)

anova(pois.mod.3, test="Chisq")

#-------------------------------------------------------------------------------
# Poisson and Quasipoisson comparison >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

quasi.mod.null <- glm(TARGET~1, fam = quasipoisson, d = minusinfluential)
summary(quasi.mod.null)

quasi.mod.1 <- glm(TARGET~., fam = quasipoisson, d = minusinfluential)
summary(quasi.mod.1)

drop1.quasi.mod.1 <- drop1(quasi.mod.1, test="F")
drop1.quasi.mod.1

# Removed FixedAcidity + ResidualSugar + CitricAcid + Density + pH
quasi.mod.2 <- glm(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide +
                           TotalSulfurDioxide + Sulphates + Alcohol + LabelAppeal +
                           AcidIndex + STARS,
                       fam = quasipoisson, d = minusinfluential)
summary(quasi.mod.2)

anova(quasi.mod.2, test="Chisq")

drop1.quasi.mod.2 <- drop1(quasi.mod.2, test="F")
drop1.quasi.mod.2

car::influencePlot(quasi.mod.2)

#-------------------------------------------------------------------------------
# Plots to understand the data better >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Plots to show that the distribution for the STARS is very different when TARGET equals zero
counts <- table(train[train[, 'TARGET']==0, 'STARS'])
barplot(counts, main="Count of STARS when TARGET equals Zero",
        xlab="STARS", col="#58BFFF")
counts <- table(train[train[, 'TARGET']>0, 'STARS'])
barplot(counts, main="Count of STARS when TARGET does NOT equal Zero",
     xlab="STARS", col="#58BFFF")

# Starter code if I want to fix up the plots with ggplot
# ggplot(train[train[, 'TARGET']==0, c('TARGET','STARS')], aes(x = STARS)) + geom_bar()

# Plots to show that the distribution for the TARGET is very different when STARS equals zero
# Also shows zero inflation
counts <- table(train[train[, 'STARS']==0, 'TARGET'])
barplot(counts, main="Count of TARGET when STARS equals Zero",
        xlab="TARGET", col="#58BFFF")
counts <- table(train[train[, 'STARS']>0, 'TARGET'])
barplot(counts, main="Count of TARGET when STARS does NOT equal Zero",
        xlab="TARGET", col="#58BFFF")

# Plots to show that the distribution for the LabelAppeal is NOT very different when TARGET equals zero
counts <- table(train[train[, 'TARGET']==0, 'LabelAppeal'])
barplot(counts, main="Count of LabelAppeal when TARGET equals Zero",
        xlab="LabelAppeal", col="#58BFFF")
counts <- table(train[train[, 'TARGET']>0, 'LabelAppeal'])
barplot(counts, main="Count of LabelAppeal when TARGET does NOT equal Zero",
        xlab="LabelAppeal", col="#58BFFF")

# Plots to show that the distribution for the TARGET is NOT very different when LabelAppeal is less than or greater than zero
# Also shows zero inflation
counts <- table(train[train[, 'LabelAppeal']<0, 'TARGET'])
barplot(counts, main="Count of TARGET when LabelAppeal is less than Zero",
        xlab="TARGET", col="#58BFFF")
counts <- table(train[train[, 'LabelAppeal']>=0, 'TARGET'])
barplot(counts, main="Count of TARGET when LabelAppeal is greater than or equal to Zero",
        xlab="TARGET", col="#58BFFF")

# Plot expected vs observed counts of TARGET to show zero inflation
ocount <- table(train_plusmin$TARGET)[1:9]
pcount <- colSums(predprob(pois.mod.2)[,1:9])
plot(pcount,ocount,type="n", main="TARGET Counts", xlab="Predicted",
     ylab="Observed", yaxt = "none") +
    text(pcount,ocount, 0:8)
axis(2, seq(0,4000,1000))

#-------------------------------------------------------------------------------
# Hurdle Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# hurd.mod1 <- hurdle(TARGET ~ ., data = train_plusmin)
# summary(hurd.mod1)

hurd.mod2 <- hurdle(TARGET ~ AcidIndex + Alcohol + LabelAppeal + STARS |
                        VolatileAcidity + FreeSulfurDioxide +
                        TotalSulfurDioxide + pH + Sulphates + Alcohol +
                        LabelAppeal + AcidIndex + STARS,
                    data = minusinfluential)
hurd.summ <- summary(hurd.mod2)

# hurd.mod4 <- hurdle(TARGET~., data = train_plusmin, dist = "negbin")
# Causes this error...
# Error in optim(fn = countDist, gr = countGrad, par = c(start$count, if (dist ==  :
#                             non-finite value supplied by optim

#-------------------------------------------------------------------------------
# Zero Inflated Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

### CAN'T GET THIS TO WORK!!! Well, maybe sometimes I can!  Set seed maybe?

train_plusmin$STARS_bins <- as.numeric(train_plusmin$STARS)
train_plusmin$STARS_bins <- ifelse(train_plusmin$STARS_bins < 3, 0, 1)

# less_than_three <- function(x) ifelse(x < 3, x, 0)
# three_and_over <- function(x) ifelse(x >= 3, x, 0)
# (less_than_three(as.numeric(STARS)) +
#         three_and_over(as.numeric(STARS))

# zi.mod1 <- zeroinfl(TARGET~.| FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+
#                         Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Density+pH+
#                         Sulphates+Alcohol+LabelAppeal+AcidIndex+STARS_bins,
#                     data = train_plusmin)

zi.mod2 <- zeroinfl(TARGET~AcidIndex + Alcohol + LabelAppeal + STARS | VolatileAcidity + FreeSulfurDioxide +
                        TotalSulfurDioxide + pH + Sulphates + Alcohol +
                        LabelAppeal + AcidIndex + STARS,
                    data = minusinfluential)

zi.summ <- summary(zi.mod2)

#-------------------------------------------------------------------------------
# VCD GOODFIT FUNCTION >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fit <- goodfit(train_plusmin$TARGET, type = "poisson", method = "MinChisq")
summary(fit)
#rootogram(fit)
Ord_plot(train_plusmin$TARGET)
distplot(train_plusmin$TARGET, type="poisson")
distplot(train_plusmin$TARGET, type="nbinomial")
distplot(train_plusmin$TARGET, type="binomial")

#-------------------------------------------------------------------------------
# MODEL COMPARISON >>>>>>>>>>s>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# from :https://data.library.virginia.edu/getting-started-with-hurdle-models/
# Need to install from R-Forge instead of CRAN
# install.packages("countreg", repos="http://R-Forge.R-project.org")
# library(countreg)

countreg::rootogram(pois.mod.1)
countreg::rootogram(pois.mod.3)
# countreg::rootogram(quasi.mod.2) # Error in rootogram.glm(quasi.mod.2) :
                                    # family currently not supported
countreg::rootogram(hurd.mod2)
countreg::rootogram(zi.mod2)

comparison <- AIC(pois.mod.1, pois.mod.2, pois.mod.3, quasi.mod.1, quasi.mod.2, hurd.mod2, zi.mod2)
comparison

dispersiontest(pois.mod.2, trafo=1, alternative = "two.sided")

#-------------------------------------------------------------------------------
# SUMMARY, PLOT, PREDICT >>>>>s>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#### Doesn't work for hurdle or zero inflated models ###########################
pois2.summary <- summ(pois.mod.2, vifs = TRUE)
pois2.summary
pois3.summary <- summ(pois.mod.3, vifs = TRUE)
pois3.summary
quasi.summary <- summ(quasi.mod.2, vifs = TRUE)
quasi.summary
# hurd.summary <- summ(hurd.mod2, vifs = TRUE)
# hurd.summary
# zi.summary <- summ(zi.mod2, vifs = TRUE)
# zi.summary

se <- function(model) sqrt(diag(vcov(model)))
pois.quasi.compare <- round(data.frame('poisson'=coef(pois.mod.3), 'quasip'=coef(quasi.mod.2),
                                       'se.poiss'=se(pois.mod.3), 'se.quasi'=se(quasi.mod.2),
                                       'ratio'=se(quasi.mod.2)/se(pois.mod.3)), 4)
pois.quasi.compare

# Doesn't work for hurdle or zero inflated models
pois_plot <- autoplot(pois.mod.3, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
    theme(panel.background=element_blank())

quasi_plot <- autoplot(quasi.mod.2, which = 1:6, colour = "#58BFFF",
                      smooth.colour = 'red', smooth.linetype = 'solid',
                      ad.colour = 'black',
                      label.size = 3, label.n = 5, label.colour = "#3300FF",
                      ncol = 2) +
    theme(panel.background=element_blank())
quasi_plot

### Poisson Model Predictions
pois.pred.raw <- predict(pois.mod.2, newdata = train_plusmin)


#-------------------------------------------------------------------------------
##### BETSY'S NOTES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
#
# TARGET + FixedAcidity + VolatileAcidity + CitricAcid +
#     ResidualSugar + Chlorides + FreeSulfurDioxide +
#     TotalSulfurDioxide + Density + pH + Sulphates +
#     Alcohol + LabelAppeal + AcidIndex + STARS

# Final test!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# train_plusminlog <- train_imputed_raw
#
# # list of columns that will be transformed
# cols <- c("FixedAcidity","VolatileAcidity",
#           "CitricAcid","ResidualSugar",
#           "Chlorides","FreeSulfurDioxide",
#           "TotalSulfurDioxide","Sulphates","Alcohol")
#
# # Transformation of train_plusminlog by adding the minimum value plus one
# for (col in cols) {
#     train_plusminlog[, col] <- sqrt(train_plusminlog[, col] + abs(min(train_plusminlog[, col])) + 1)
# }
#
# #train_plusminlog$STARS <- as.factor(train_plusminlog$STARS)
# #train_plusminlog$LabelAppeal <- as.factor(train_plusminlog$LabelAppeal)
#
# # Data Distribution
# hist_plusminlog <- train_plusminlog[,-c(13,15)] %>%
#     gather() %>%
#     ggplot(aes(value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_histogram(fill = "#58BFFF") +
#     xlab("") +
#     ylab("") +
#     theme(panel.background = element_blank())
# hist_plusminlog
#
# hurd.mod3 <- hurdle(TARGET~.| FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+
#                         Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Density+pH+
#                         Sulphates+Alcohol+LabelAppeal+AcidIndex+STARS,
#                     data = train_plusminlog)
# summary(hurd.mod3)
#
# hurd.mod4 <- hurdle(TARGET~AcidIndex + Alcohol + LabelAppeal + STARS | VolatileAcidity +
#                         FreeSulfurDioxide + TotalSulfurDioxide + pH + Sulphates + Alcohol +
#                         LabelAppeal + AcidIndex + STARS,
#                     data = train_plusminlog)
# summary(hurd.mod4)
#
# zi.mod3 <- zeroinfl(TARGET~.| FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+
#                         Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Density+pH+
#                         Sulphates+Alcohol+LabelAppeal+AcidIndex+STARS,
#                     data = train_plusminlog)
# summary(zi.mod3)
#
# zi.mod4 <- zeroinfl(TARGET~AcidIndex + Alcohol + LabelAppeal + STARS | VolatileAcidity +
#                         FreeSulfurDioxide + TotalSulfurDioxide + pH + Sulphates + Alcohol +
#                         LabelAppeal + AcidIndex + STARS,
#                     data = train_plusminlog)
# summary(zi.mod4)

