# Model 1:

nb.model.rose.1 <- glm.nb(TARGET ~ ., 
                          data=train_imputed)
summary(nb.model.rose.1)
par(mfrow=c(2,2))
plot(nb.model.rose.1)

MASS::stepAIC(nb.model.rose.1, trace=0)


# Model 2:

nb.model.rose.2 <- glm.nb(TARGET ~ VolatileAcidity + Chlorides + TotalSulfurDioxide + 
                            Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                          data=train_imputed)
summary(nb.model.rose.2)
par(mfrow=c(2,2))
plot(nb.model.rose.2)


# Model 3: Zero Dispersion Counts

z.model.rose.1 <- zeroinfl(TARGET ~ . |STARS, 
                           data=train_imputed, 
                           dist="negbin")
summary(z.model.rose.1)

# Evaluate
# MAE, RMSE
nb1 <- rootogram(nb.model.rose.1, style = "hanging", plot = FALSE)
nb2<- rootogram(nb.model.rose.2, style = "hanging", plot = FALSE)
zinb <- rootogram(z.model.rose.1, style = "hanging", plot = FALSE)

#ylims <- ylim(20, 50)  # common scale for comparison
plot_grid(autoplot(nb1),# + ylims, 
          autoplot(nb2),# + ylims, 
          autoplot(zinb),# + ylims, 
          ncol = 3, labels = "auto")


nb1.fit <- rootogram(table(train$TARGET), fitted=table(c(trunc(fitted(nb.model.rose.1)), 8)), type='hanging', plot = FALSE)
nb2.fit <- rootogram(table(train$TARGET), fitted=table(c(trunc(fitted(nb.model.rose.2)), 8)), type='hanging', plot = FALSE)
zinb.fit <- rootogram(table(train$TARGET), fitted=table(c(trunc(fitted(z.model.rose.1)), 8)), type='hanging', plot = FALSE)

#ylims <- ylim(-2, 7)  # common scale for comparison
plot_grid(autoplot(nb1.fit),# + ylims, 
          autoplot(nb2.fit),# + ylims, 
          autoplot(zinb.fit),# + ylims, 
          ncol = 3, 
          labels = "auto")



# Prediction:
final.pred <- predict(z.model.rose.1, test) 
final.df <- cbind(TARGET_FLAG=final.pred)
hist(final.pred)

# Export:
write.csv(final.df, 'wine_pred.csv', row.names = FALSE)

