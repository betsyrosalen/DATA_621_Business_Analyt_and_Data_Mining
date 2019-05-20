
# DATA EXPLORATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load data
data <- read.csv ('https://raw.githubusercontent.com/betsyrosalen/DATA_621_Business_Analyt_and_Data_Mining/master/projectfinal_heart/data/heart.csv', 
                   stringsAsFactors = F, header = T)

variable_descriptions <- rbind(c('var_name','def','type'),
                               c('var_name','def','type'),
                               c('var_name','def','type'))

colnames(variable_descriptions) <- c('VARIABLE','DEFINITION','TYPE')

# bootstrap surrogate data using synthpop
# https://cran.r-project.org/web/packages/synthpop/vignettes/synthpop.pdf
# https://cran.r-project.org/web/packages/synthpop/synthpop.pdf
# https://www.r-bloggers.com/generating-synthetic-data-sets-with-synthpop-in-r/
# https://www.geos.ed.ac.uk/homes/graab/synthpop.pdf


syn_obj <- synthpop::syn(data = data, m = 200)  # creates 10 synthetic datasets based on original dataset and its variables distributions
syn_dflist <- syn_obj$syn  # extract list of synthesized data frames from synds object
syn_df <- dplyr::bind_rows(syn_dflist, .id = 'column_label')

# address outliers, treating as NAs and imputing

# nrow(syn_df)
# compare(synth.obj, data)  # visually compare of synthetic datasets vs original data
# syn_csv <- write.syn(syn_obj, 'csv')



# split data into test and train using cross-validation

folds <- 10  # set to 10 provisionally
train_bootstrap <- caret::trainControl(method = 'boot', number = folds)  # bootstrap resampling approach
train_kfold <- caret::trainControl(method = 'cv', number = folds)  # k-fold cross validation
train_kfoldrpt <- caret::trainControl(method = 'repeatedcv', number = folds, repeats = 3)  # k-fold cross validation, provisionally set to 3 repeats but explore setting
train_loocv <- caret::trainControl(method = 'LOOCV')
syn_df$column_label <- NULL





# Summary Statistics

orig_data <- data
syn_data <-  syn_df


orig_data$sex <- as.factor(orig_data$sex)
orig_data$cp <- as.factor(orig_data$cp)
orig_data$ca <- as.factor(orig_data$ca)
orig_data$fbs <- as.factor(orig_data$fbs)
orig_data$restecg <- as.factor(orig_data$restecg)
orig_data$slope <- as.factor(orig_data$slope)
orig_data$target <- as.factor(orig_data$target)
orig_data$thal <- as.factor(orig_data$thal)
orig_data$exang <- as.factor(orig_data$exang)

syn_data$sex <- as.factor(syn_data$sex)
syn_data$cp <- as.factor(syn_data$cp)
syn_data$ca <- as.factor(syn_data$ca)
syn_data$fbs <- as.factor(syn_data$fbs)
syn_data$restecg <- as.factor(syn_data$restecg)
syn_data$slope <- as.factor(syn_data$slope)
syn_data$target <- as.factor(syn_data$target)
syn_data$thal <- as.factor(syn_data$thal)
syn_data$exang <- as.factor(syn_data$exang)


data_num <- orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak')]
data_cat <- orig_data[, c('sex', 'cp','ca', 'exang','fbs', 'restecg', 'slope','target', 'thal' )]
syn_num <- syn_data[, c( 'age','trestbps', 'chol','thalach', 'oldpeak')]
syn_cat <- syn_data[, c('sex', 'cp','ca', 'exang', 'fbs', 'restecg', 'slope','target', 'thal')]

orig_data_stats <- describe(data_num)[,c(2,8,3,5,9,4)]
syn_data_stats <- describe(syn_num)[,c(2,8,3,5,9,4)]
data_cat_stats <- summary(data_cat[, c( 'cp','ca',  'restecg', 'slope', 'thal')])
syn_cat_stats <- summary(syn_cat[, c('cp','ca',   'restecg', 'slope', 'thal')])
data_cat_stats_b <- summary(data_cat[, c('exang', 'fbs', 'sex', 'target')])
syn_cat_stats_b <- summary(syn_cat[, c('exang', 'fbs', 'sex', 'target')])



# Outliers

#Data Distribution

data_num_hist <- orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak', 'target')]
hist.num <- data_num_hist %>%
  gather(-target, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(target))) +
  geom_histogram(position="dodge", bins=10, alpha=0.5) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top")

bar.cat <- data_cat %>%
  gather(-target, key = "var", value = "val") %>%
  ggplot(aes(x = val, fill=factor(target))) +
  geom_bar(position="dodge", alpha=0.5) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual("target",values = c("#58BFFF", "#3300FF")) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank(), legend.position="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Scaled BoxPlots
scaled.train.num <- as.data.table(scale(orig_data[, c( 'age','trestbps', 'chol', 'thalach', 'oldpeak')]))
melt.train <- melt(scaled.train.num)

scaled.boxplots <- ggplot(melt.train, aes(variable, value)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  coord_flip() +
  #scale_y_continuous(labels = scales::comma,
  #                   breaks = seq(0, 110, by = 10)) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("#9900FF", "#3300FF")) +
  theme(panel.background=element_blank(), legend.position="top")


#Linear relationship between each numeric predictor and the target


linear_graph_data <- orig_data[, c('age','trestbps', 'chol', 'thalach', 'oldpeak', 'target')]
boxplots.target <- linear_graph_data  %>%
  gather(-target,key = "var", value = "val") %>%
  ggplot(aes(x=factor(target), y=val)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="red", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  facet_wrap(~ var, scales = "free", ncol=4) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("#9900FF", "#3300FF")) +
  theme(panel.background=element_blank())

## Correlation

plot.data <- linear_graph_data
plot.data$target<- factor(plot.data$target)
corr.plot2 <- plot.data %>% 
  ggscatmat(color="target", alpha=0.1) +
  scale_color_manual(values=c("#58BFFF", "#3300FF")) +
  theme(panel.background=element_blank(), legend.position="top",
        axis.text.x = element_text(angle=-40, vjust=1, hjust=0))



#Missing Data

# DATA PREPARATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# BUILD MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Model 1 - logistic regression model (see notes)

#======================================================================================#

## Model 2 - decision tree model (see notes)

#======================================================================================#

## Model 3 - random forest model (see notes)

#======================================================================================#

## Model 4 - support vector machines model (see notes)

#======================================================================================#

## Model 5 - naive bayes model (see notes)

#======================================================================================#


## Model Evaluations



# SELECT MODELS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

