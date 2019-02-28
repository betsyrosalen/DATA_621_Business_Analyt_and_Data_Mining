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


# Tables and Figures to add in Appendix   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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