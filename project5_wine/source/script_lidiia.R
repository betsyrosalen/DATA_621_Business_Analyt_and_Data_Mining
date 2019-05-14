
#useBIC(hurd.mod2, return.K = FALSE, nobs = NULL)
#useBIC(zi.mod2, return.K = FALSE, nobs = NULL)
#useBIC(nb.model.jeremy.2, return.K = FALSE, nobs = NULL, c.hat = 1)
#useBIC(nb.model.jeremy.3, return.K = FALSE, nobs = NULL, c.hat = 1)
#useBIC(mlr_1, return.K = FALSE, nobs = NULL)       


#df.residual(hurd.mod2)     
#df.residual(zi.mod2)  
#df.residual(nb.model.jeremy.2)  
#df.residual(nb.model.jeremy.3)  
#df.residual(mlr_1)  


#logLik(hurd.mod2)     
#logLik(zi.mod2)  
#logLik(nb.model.jeremy.2)  
#logLik(nb.model.jeremy.3)  
#logLik(mlr_2)  


model_select_lt <- rbind(c('Poisson-Logit Hurdle Regression',useBIC(hurd.mod2, return.K = FALSE, nobs = NULL),logLik(hurd.mod2),'27'),
                         c('Zero-inflated Poisson',useBIC(zi.mod2, return.K = FALSE, nobs = NULL),logLik(zi.mod2),'27'),
                         c('Negative Binomial 1',useBIC(nb.model.jeremy.2, return.K = FALSE, nobs = NULL, c.hat = 1),logLik(nb.model.jeremy.2),'23'),
                         c('Negative Binomial 2',useBIC(nb.model.jeremy.3, return.K = FALSE, nobs = NULL, c.hat = 1),logLik(nb.model.jeremy.3),'22'),
                         c('Linear Model',useBIC(mlr_1, return.K = FALSE, nobs = NULL),logLik(mlr_1),'11'),
                         c('Linear Model 2',useBIC(mlr_2, return.K = FALSE, nobs = NULL),logLik(mlr_2),'49')
                         
                         
)

colnames(model_select_lt) <- c('model','BIC','Log-likelihood', 'Degrees of freedom')


root.pois <- countreg::rootogram(hurd.mod2, style = "hanging", plot = FALSE)
ylims <- ylim(-1, 100)  
countreg_model <- plot_grid(autoplot(root.pois) + ylims, ncol = 1, labels = "auto")

test$TARGET_hurd <- predict(hurd.mod2, newdata = test, type = "response")

predictions_lt <- round(test$TARGET_hurd)

p1.pred <- ggplot(data.frame(predictions_lt), aes(predictions_lt)) +
  geom_histogram(fill = "#58BFFF", bins = 20) +
  xlab("Test Data Predictions") +
  ylab("") +
  theme(panel.background = element_blank())

p2.pred <- ggplot(train, aes(TARGET)) +
  geom_histogram(fill = "#58BFFF", bins = 20) +
  xlab("Training Data") +
  ylab("") +
  theme(panel.background = element_blank())


summary.pred.count <- describe(test[, c('TARGET_hurd')])[,c(2,8,3,5,9,4)]


#https://stats.idre.ucla.edu/stata/seminars/regression-models-with-count-data/
#https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf

