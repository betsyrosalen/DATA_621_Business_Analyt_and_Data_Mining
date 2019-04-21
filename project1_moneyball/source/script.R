# Proj 1
# load data
train <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj1/master/moneyball-training-data.csv',
                  stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj1/master/moneyball-evaluation-data.csv',
                 stringsAsFactors = F, header = T)
# check data
str(train)
str(test)

# remove index
train$INDEX <- NULL
test$INDEX <- NULL

# clean the variable names so it is easier to use 
cleanVar <- function(data) {
  name.list <- names(data)
  name.list <- gsub("TEAM_", "", name.list)
  names(data) <- name.list
  data
}

# apply the function
train <- cleanVar(train)
test <- cleanVar(test)

# check data once again
str(train)
str(test)


# Tables and Figures

# Summary Statistics
tbl1 <- describe(train)[,c(2,8,3,5,9,4)]

# Histogram
Hist <- train %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())

# Boxplot
melt.train <- melt(train)

outlier.boxplot <- ggplot(melt.train, aes(variable, value)) + 
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="#58BFFF", outlier.size = 1) +
  scale_y_log10() + 
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point", 
               size=2, show.legend=TRUE) +
  coord_flip(ylim = c(0, 2200), expand = TRUE) +   
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 2200, by = 200)) + 
  labs(colour="Statistics", x="", y="log transformed freq.") + 
  scale_colour_manual(values=c("red", "blue")) +
  theme(panel.background=element_blank(), legend.position="top")

# Linearity
linearity <- train %>%
  gather(-TARGET_WINS, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_WINS)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET_WINS") +
  xlab("") +
  theme(panel.background = element_blank())

# Missing values
#table(is.na(train)) #3478 missing values
#sapply(train, function(x) sum(is.na(x)))
na.barplot <- plot_missing(train) 

# Fix missing values - remove BATTING_HBP (90%+ missing)
train.mod <- subset(train, select = -c(BATTING_HBP))

# NA Imputation
train.mod <- as.data.table(train.mod)
dummies <- dummyVars(~ ., data = train.mod[, -1])
train.dummy <- predict(dummies, train.mod)
pre.process <- preProcess(train.dummy, method='bagImpute')
imputation <- as.data.frame(predict(pre.process, train.dummy))

imputed_train <- cbind(train.mod$TARGET_WINS, imputation)
names(imputed_train)[1] <- "TARGET_WINS"

# Data before imputing values
train.mod.desc <- describe(train.mod)[,c(2,8,3,5,9,4)]

# Data after imputing values
imputed_train.desc <- describe(imputed_train)[,c(2,8,3,5,9,4)]

# Differences between original and imputed data
origin.impute.diff <- train.mod.desc - imputed_train.desc

# Remove outliers

max_sd = 5 # change this number to change the threshold for how many standard deviations from the mean are acceptable

outliers <- sapply(imputed_train[,-1], function(x) ifelse(x < mean(x)+(sd(x)*max_sd), TRUE, NA))
#outliers <- sapply(imputed_train[,-1], function(x) ifelse(findInterval(x, c(mean(x)-(sd(x)*max_sd),mean(x)+(sd(x)*max_sd)), rightmost.closed = T) == 1, TRUE, NA))
imputed_train <- imputed_train[complete.cases(outliers),]

sapply(train.mod, function(x) sum(is.na(x)))

train.mod[, `:=`(BATTING_SO = imputation$BATTING_SO,
                 BASERUN_SB = imputation$BASERUN_SB,
                 BASERUN_CS = imputation$BASERUN_CS,
                 PITCHING_SO = imputation$PITCHING_SO,
                 FIELDING_DP = imputation$FIELDING_DP)]

# Correlations
corr.train <- round(cor(imputed_train),3)
corr.plot <- ggcorrplot::ggcorrplot(corr.train, 
                                    type = 'lower',
                                    lab=T,
                                    lab_size=2)

# Feature Engineering
imputed_train$BP_H <- imputed_train$BATTING_H - imputed_train$PITCHING_H
imputed_train$BP_HR <- imputed_train$BATTING_HR - imputed_train$PITCHING_HR
imputed_train$BP_BB <- imputed_train$BATTING_BB - imputed_train$PITCHING_BB
imputed_train$BP_SO <- imputed_train$BATTING_SO - imputed_train$PITCHING_SO

# Model 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
model_exp <- lm(TARGET_WINS ~  BATTING_H + BATTING_HR + BATTING_SO + FIELDING_E + 
                  PITCHING_SO + BASERUN_SB + PITCHING_HR + BATTING_BB + BATTING_2B + 
                  BATTING_3B + FIELDING_DP + PITCHING_BB + PITCHING_H ,
                data = imputed_train)

model_exp2 <- lm(TARGET_WINS ~   BATTING_HR + BATTING_SO + FIELDING_E + 
                   PITCHING_SO + BASERUN_SB + PITCHING_HR + BATTING_BB  + 
                   BATTING_3B + FIELDING_DP + PITCHING_BB + PITCHING_H ,
                 data = imputed_train)

figure6 <- stripchart(data.frame(scale(imputed_train)), method ="jitter", las=2,
                      vertical=TRUE)

model_exp3 <- lm(TARGET_WINS ~   BATTING_HR + BATTING_SO + FIELDING_E + 
                   PITCHING_SO + BASERUN_SB + PITCHING_HR + BATTING_BB  + 
                   BATTING_3B + FIELDING_DP + PITCHING_BB + log(PITCHING_H) ,
                 data = imputed_train)

residplot_exp <- ggplot(data = model_exp3, 
                        aes(x = .fitted, 
                            y = .resid)) +
                  geom_point(aes(y = .resid, 
                                 color = .resid)) +
                  scale_color_gradient2(low = "midnightblue", 
                                        mid = 'white', 
                                        high = 'red2') +
                  stat_smooth(method = 'loess', 
                              se = TRUE, 
                              fill = 'gray95', 
                              color = 'darkgray') +
                  geom_hline(yintercept = 0, 
                             col = "black", 
                             linetype = "dashed", 
                             alpha = .8, 
                             size = .5) +
                  guides(color = FALSE) +
                  labs(x = 'Fitted Values', 
                       y = 'Residuals') +
                  theme_minimal() +
                  scale_y_continuous(labels = scales::comma) +
                  scale_x_continuous(labels = scales::comma) +
                  theme(plot.title = element_text(hjust = .5))

qqplot_exp <- ggplot(data = imputed_train, aes(sample = TARGET_WINS)) +
                    stat_qq(size = 1.5) +
                    stat_qq_line(color = 'darkgray') +
                    labs(x = "Theoretical Quantiles", 
                         y = "Standardized Residuals") +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5))

pred_exp <- predict(model_exp3, test) 

# Model 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

plot1 <- ggplot(imputed_train, aes(x = BATTING_HR, y = PITCHING_HR)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

plot2 <- ggplot(imputed_train, aes(x = BATTING_HR, y = BATTING_SO)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

plot3 <- ggplot(imputed_train, aes(x = BATTING_BB, y = PITCHING_BB)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

plot4 <- ggplot(imputed_train, aes(x = BATTING_SO, y = PITCHING_SO)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

plot5 <- ggplot(imputed_train, aes(x = BATTING_SO, y = PITCHING_HR)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

plot6 <- ggplot(imputed_train, aes(x = BASERUN_SB, y = BASERUN_CS)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  theme(panel.background = element_blank())

lm_data <- imputed_train[,-c(9,11:13,16:19)]
lm_data <- as.data.frame(lm_data)

fig8 <- lm_data %>%
  gather(-TARGET_WINS, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_WINS)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=5) +
  xlab("") +
  ylab("TARGET_WINS") +
  theme(panel.background = element_blank())

Histograms <- lm_data %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  #ggtitle("Histograms") +
  theme(panel.background = element_blank())

# Log Transform Data
to_log <- c("BASERUN_SB", "BATTING_3B", "FIELDING_E", "PITCHING_H")
log_lm_data <- lm_data
log_lm_data[,to_log] <- log(log_lm_data[,to_log])

fig10 <- log_lm_data[,c(to_log, "TARGET_WINS")] %>%
  gather(-TARGET_WINS, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_WINS)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=2) +
  xlab("") +
  ylab("TARGET_WINS") +
  theme(panel.background = element_blank())

fig11 <- log_lm_data[,to_log] %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol=4) +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  ggtitle("Histograms") +
  theme(panel.background = element_blank())

# Model 2 - first model
# Basic linear model with all variables
lm1 <- lm(TARGET_WINS ~ BATTING_H + BATTING_2B + log(BATTING_3B) + BATTING_HR + BATTING_BB + BATTING_SO + log(BASERUN_SB) + log(PITCHING_H) + log(FIELDING_E) + FIELDING_DP, lm_data)
lm_summary <- summary(lm1)

# All Subsets Regression from leaps package
leaps <- regsubsets(x=log_lm_data[,-1], y=log_lm_data[,1], nbest=3)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
leaps_plot <- plot(leaps, scale="r2")

# Scale all the predictor variables
z_train <- data.frame(cbind(lm_data[,1],sapply(lm_data[,-1], scale)))
# Linear model using all scaled predictors
colnames(z_train)[1] <- "TARGET_WINS"
scaled_lm <- lm(TARGET_WINS ~ BATTING_H + BATTING_2B + BATTING_3B + BATTING_HR + BATTING_BB + BATTING_SO + BASERUN_SB + PITCHING_H + FIELDING_E + FIELDING_DP, z_train)
scaled_lm_summary <- summary(scaled_lm)

#nullmod
nullmod <- lm(TARGET_WINS ~ 1, lm_data)
anova(nullmod, lm1)
### Test one predictor
lm2 <- lm(TARGET_WINS ~ ., lm_data[, -2])
anova(lm2, lm1)
### Test one predictor
lm3 <- lm(TARGET_WINS ~ ., lm_data[, -3])
anova(lm3, lm1)

#Testing a subspace

all_data <- imputed_train[,-c(16:19)]
lm4 <- lm(TARGET_WINS ~ I(BATTING_HR+PITCHING_HR)+I(BATTING_BB+PITCHING_BB)+
            I(BATTING_SO+PITCHING_SO)+BATTING_H+BATTING_2B+log(BATTING_3B)+
            log(BASERUN_SB)+BASERUN_CS+log(PITCHING_H)+log(FIELDING_E)+FIELDING_DP, all_data)
lm4_summary <- summary(lm4)

mod_1 <- lm(TARGET_WINS ~ ., imputed_train)
step <- stepAIC(mod_1, direction="both")
#step$anova # display results

#changed to BATTING_BB*PITCHING_BB, BATTING_SO*PITCHING_SO, BATTING_H*log(PITCHING_H
mod_3 <- lm(TARGET_WINS ~ BATTING_3B + BATTING_HR + BATTING_BB*PITCHING_BB + 
              BATTING_SO*PITCHING_SO + BASERUN_SB + BASERUN_CS + BATTING_H*log(PITCHING_H) + 
              log(FIELDING_E) + FIELDING_DP, imputed_train)

mod_3_summary <- summary(mod_3)

# Model 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

Histograms.mod3 <- train.mod[,c(10, 12:14)] %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", nrow = 1) +
  geom_histogram(fill = "#58BFFF") +  
  theme(panel.background = element_blank())


logtransform_lm <- lm(TARGET_WINS ~ 
                        BATTING_H
                      + BATTING_2B
                      + BATTING_3B
                      + BATTING_HR
                      + BATTING_BB
                      + BATTING_SO
                      + BASERUN_SB
                      #+ BASERUN_CS
                      + log(PITCHING_H)
                      #+ log(PITCHING_HR + .0001) # p-value around .16 as log .27 w/o so remove
                      + PITCHING_BB
                      #+ log(PITCHING_SO + .0001) # p-value around .5 whether or not log transform
                      + FIELDING_E
                      + FIELDING_DP
                      , data = imputed_train)

logtransform_lm_summary <- summary(logtransform_lm)

#logtransform_lm_summary

# Model 3: append predictions and residuals  
imputed_train$logtransform_pred <- predict(logtransform_lm)
imputed_train$logtransform_resid <- residuals(logtransform_lm)

# Model 3: residual plot
logtransform_residplot <- ggplot(data = logtransform_lm, # for each model update chart object name and dataframe 
                                 aes(x = .fitted, 
                                     y = .resid)) +
  geom_point(aes(y = .resid, 
                 color = .resid)) +
  scale_color_gradient2(low = "midnightblue", 
                        mid = 'white', 
                        high = 'red2') +
  stat_smooth(method = 'loess', 
              se = TRUE, 
              fill = 'gray95', 
              color = 'darkgray') +
  geom_hline(yintercept = 0, 
             col = "black", 
             linetype = "dashed", 
             alpha = .8, 
             size = .5) +
  guides(color = FALSE) +
  labs(x = 'Fitted Values', 
       y = 'Residuals') +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = .5))

# Model 3: QQ-plot residuals 
logtransform_qqplot <- ggplot(logtransform_lm, aes(sample = .stdresid)) +  # for each model update chart object name and model object 
  stat_qq(size = 1.5) +
  stat_qq_line(color = 'darkgray') +
  labs(x = "Theoretical Quantiles", 
       y = "Standardized Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

#Multi-colinearity
collintest_df <- imputed_train[,3:15]
# subset dataframe for independent variables
collintest_stats <- pcor(collintest_df, method = 'pearson')

# Check Conditions for Least Squares Regression
residplot_exp <- ggplot(data = mod_3, 
                        aes(x = .fitted, 
                            y = .resid)) +
  geom_point(aes(y = .resid, 
                 color = .resid)) +
  scale_color_gradient2(low = "midnightblue", 
                        mid = 'white', 
                        high = 'red2') +
  stat_smooth(method = 'loess', 
              se = TRUE, 
              fill = 'gray95', 
              color = 'darkgray') +
  geom_hline(yintercept = 0, 
             col = "black", 
             linetype = "dashed", 
             alpha = .8, 
             size = .5) +
  guides(color = FALSE) +
  labs(x = 'Fitted Values', 
       y = 'Residuals') +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = .5))

qqplot_exp <- ggplot(data = lm_data, aes(sample = TARGET_WINS)) +
  stat_qq(size = 1.5) +
  stat_qq_line(color = 'darkgray') +
  labs(x = "Theoretical Quantiles", 
       y = "Standardized Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

# Predictions
predictions <- round(predict(mod_3, test))

p1.pred <- ggplot(data.frame(predictions), aes(predictions)) +
  geom_histogram(fill = "#58BFFF", bins = 20) +
  xlab("Test Data Predictions") +
  ylab("") +
  theme(panel.background = element_blank())

p2.pred <- ggplot(lm_data, aes(TARGET_WINS)) +
  geom_histogram(fill = "#58BFFF", bins = 20) +
  xlab("Training Data") +
  ylab("") +
  theme(panel.background = element_blank())
