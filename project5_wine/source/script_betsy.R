# create a copy pf train_imputed to transform by adding the minimum value plus one so there are no zero values left.
train_plusmin <- train_imputed

# list of columns that will be transformed ^^^
cols <- c("FixedAcidity","VolatileAcidity",
         "CitricAcid","ResidualSugar",
         "Chlorides","FreeSulfurDioxide",
         "TotalSulfurDioxide","Sulphates","Alcohol")

# Transformation by adding the minimum value plus one.
for (col in cols) {
    train_plusmin[, col] <- train_plusmin[, col] + abs(min(train_plusmin[, col])) + 1
}
