
vars_lt <- rbind(c('TARGET','Number of Cases Purchased','count response'),
              c('AcidIndex','Method of testing total acidity by using a weighted avg','continuous numerical predictor'),
              c('Alcohol','Alcohol Content','continuous numerical predictor'),
              c('Chlorides','Chloride content of wine','continuous numerical predictor'),
              c('CitricAcid','Citric Acid Content','continuous numerical predictor'),
              c('Density','Density of Wine','continuous numerical predictor'),
              c('FixedAcidity','Fixed Acidity of Wine','continuous numerical predictor'),
              c('FreeSulfurDioxide','Sulfur Dioxide content of wine','continuous numerical predictor'),
              c('LabelAppeal','Marketing Score indicating the appeal of label design','continuous numerical predictor'),
              c('ResidualSugar','Residual Sugar of wine','continuous numerical predictor'),
              c('STARS','Wine rating by a team of experts. 4 = Excellent, 1 = Poor','continuous numerical predictor'),
              c('Sulphates','Sulfate conten of wine','continuous numerical predictor'),
              c('TotalSulfurDioxide','Total Sulfur Dioxide of Wine','continuous numerical predictor'),
              c('VolatileAcidity','Volatile Acid content of wine','continuous numerical predictor'),
              c('pH','pH of wine','continuous numerical predictor') )

colnames(vars_lt) <- c('VARIABLE','DEFINITION','TYPE')


# Summary Statistics
summary_stat_lt <- describe(train)

# Data Distribution
hist_lt <- train %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "#58BFFF") +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank())

# Boxplot

scaled.train.num <- as.data.table(scale(train[, c('STARS', 'AcidIndex','LabelAppeal','Alcohol',
                                                      'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                                                      'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity', 'TARGET')]))


melt.train <- melt(scaled.train.num)

scaled_boxplots_lt <- ggplot(melt.train, aes(variable, value)) +
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
scaled_boxplots_lt

#Scatter plot between numeric predictors and the TARGET
linearity_lt <- train %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())

#Scatter plot between log transformed predictors and the log transformed TARGET filtered for rows where TARGET is greater than 0
logged_vals_lt <- train[,c('STARS', 'AcidIndex','LabelAppeal','Alcohol',
                               'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                               'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity', 'TARGET')]
logged_vals_lt <- logged_vals_lt %>%
  filter(TARGET>0) %>%
  log()

linearity_log_lt <- logged_vals_lt %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())

#Box Cox
bc_lt <- train[train[, 'TARGET'] > 0, ]



## Square Root Transformed Predictors and Log transformed Target Linearity Plot
X_lt <- train[train[, 'TARGET']>0,
                 c('STARS', 'AcidIndex','LabelAppeal','Alcohol',
                   'Sulphates', 'pH', 'TotalSulfurDioxide','FreeSulfurDioxide', 'Chlorides',
                   'ResidualSugar', 'CitricAcid', 'VolatileAcidity','FixedAcidity')]
sqroot_vals_lt <- data.table(cbind(log(train[train[, 'TARGET']>0,'TARGET']),
                                sapply(X_lt, sqrt)))
colnames(sqroot_vals_lt)[1] <- 'TARGET'

linearity_root_lt <- sqroot_vals_lt %>%
  gather(-TARGET, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET)) +
  geom_point(alpha=0.1) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol=3) +
  ylab("TARGET") +
  xlab("") +
  theme(panel.background = element_blank())
