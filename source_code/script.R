# Training Exploration

moneyball_train <- read.csv("./data/moneyball-training-data.csv")[,-1] # use me
moneyball_complete <- moneyball_train[complete.cases(moneyball_train),]

Summary <- summary(moneyball_train)

Standard_Deviation <- sapply(moneyball_complete, sd)

Boxplots <- ggplot(stack(moneyball_complete), aes(x=ind, y=values)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

Point_plots <- ggplot(data=moneyball_complete, aes(x=TARGET_WINS)) +
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

Correlation <- ggcorrplot(as.data.frame(round(cor(moneyball_complete), 3)), 
           type="upper", lab=TRUE, lab_size=.8)

Missing_values <- sapply(moneyball_train, function(x) sum(is.na(x)))
