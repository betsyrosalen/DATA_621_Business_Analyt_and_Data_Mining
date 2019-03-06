
mb_train <- read.csv("./data/moneyball-training-data.csv")[,-1] # use me
mb_complete <- mb_train[complete.cases(mb_train),]

Means <- sapply(mb_complete, mean)
Stan_Dev <- sapply(mb_complete, sd)

Data_Summary <- summary(mb_train)

Boxplots <- ggplot(stack(mb_complete), aes(x=ind, y=values)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

Point_plots <- ggplot(data=mb_complete, aes(x=TARGET_WINS)) +
    geom_point(aes(y=TEAM_BATTING_H, color="Base Hits by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_2B, color="Doubles by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_3B, color="Triples by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_HR, color="Homeruns by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_BB, color="Walks by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_SO, color="Strikeouts by Batters"), alpha=0.1) +
    geom_point(aes(y=TEAM_BASERUN_SB, color="Stolen Bases"), alpha=0.1) +
    geom_point(aes(y=TEAM_BASERUN_CS, color="Caught Stealing"), alpha=0.1) +
    geom_point(aes(y=TEAM_BATTING_HBP, color="Batters Hit by Pitch"), alpha=0.1) +
    geom_point(aes(y=TEAM_PITCHING_H, color="Hits Allowed"), alpha=0.1) +
    geom_point(aes(y=TEAM_PITCHING_HR, color="Homeruns Allowed"), alpha=0.1) +
    geom_point(aes(y=TEAM_PITCHING_SO, color="Strikeouts by Pitchers"), alpha=0.1) +
    geom_point(aes(y=TEAM_FIELDING_E, color="Errors"), alpha=0.05) +
    geom_point(aes(y=TEAM_FIELDING_DP, color="Double Plays"), alpha=0.1) +
    labs(color="Variables", ylab="Variables")

Correlation <- ggcorrplot(as.data.frame(round(cor(mb_complete), 3)),
           type="upper", lab=TRUE, lab_size=2)

Missing_values <- sapply(mb_train, function(x) sum(is.na(x)))

Histograms <- mb_train %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

Pairs <- ggpairs(mb_train)

Corr_matrix <- round(cor(mb_complete),2)


mb_lm <- lm(TARGET_WINS ~ ., mb_train)

LM_Summary <- summary(mb_lm)

LM_plot <- plot(mb_lm)

# All Subsets Regression from leaps package
Leaps <- regsubsets(x=mb_complete[,2:16], y=mb_complete[,1], nbest=3)

# plots a table of possible models showing variables in each model.
# models are ordered by the selection statistic.
Leaps_plot <- plot(Leaps, scale="r2")

# Scale all the predictor variables
mb_scaled <- as.data.frame(scale(mb_complete[,2:16], center=Means[2:16], scale=Stan_Dev[2:16]))
mb_scaled$TARGET_WINS <- mb_complete[,1]

# Linear model using scaled predictors
mb_scaled_lm <- lm(TARGET_WINS ~ ., mb_scaled)

Scaled_LM_Summary <- summary(mb_scaled_lm)

Scaled_LM_plot <- plot(mb_scaled_lm)

# All Subsets Regression from leaps package on SCALED data
Scaled_Leaps <- regsubsets(x=mb_scaled[,1:15], y=mb_scaled[,16], nbest=3)

# plots a table of possible models showing variables in each model.
# models are ordered by the selection statistic.
Scaled_Leaps_plot <- plot(Scaled_Leaps, scale="r2")
