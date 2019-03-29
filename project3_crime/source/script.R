
# load data
train <- read.csv ('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-training-data_modified.csv', stringsAsFactors = F, header = T)
test <- read.csv('https://raw.githubusercontent.com/silverrainb/data621proj3/master/crime-evaluation-data_modified.csv', stringsAsFactors = F, header = T)

# Summary Statistics
sum_stat<- describe(train)[,c(2,8,3,5,9,4)]


# Shape of Predictor Distributions
Hist <- train[,-13] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())


# Outliers
boxplot_train <- train[,-13]
boxplot_train$tax <- boxplot_train$tax/10
melt.train <- melt(boxplot_train)

outlier.boxplot <- ggplot(melt.train, aes(variable, value)) +
  geom_boxplot(width=.5, fill="#58BFFF", outlier.colour="#58BFFF", outlier.size = 1) +
  stat_summary(aes(colour="mean"), fun.y=mean, geom="point",
               size=2, show.legend=TRUE) +
  stat_summary(aes(colour="median"), fun.y=median, geom="point",
               size=2, show.legend=TRUE) +
  coord_flip(ylim = c(0, 110), expand = TRUE) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 110, by = 10)) +
  labs(colour="Statistics", x="", y="") +
  scale_colour_manual(values=c("red", "blue")) +
  theme(panel.background=element_blank(), legend.position="top")


#  Missing Values
na.barplot <- plot_missing(train)


# Linearity
linearity <- train %>%
  gather(-target, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = target)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("target") +
  xlab("") +
  theme(panel.background = element_blank())

# Correlation
# correl <- ggpairs(train)
correl2 <- train %>% 
  select(-target) %>% 
  cor() %>% 
  corrplot(method = "circle")

# Linearity at log10 scale
linearity_log <- train %>%
  gather(-target, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = target)) +
  geom_point(alpha=0.1) +
  scale_x_log10() + 
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("target") +
  xlab("") +
  theme(panel.background = element_blank())
