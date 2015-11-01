# Determine number of detected anomalies prior to error injection
data.test <- data.validation
threshold <- 10000
injection.size <- 5
repetitions <- 1000

# First predictions are calculated
model.lrm.predictions <- predict( model.lrm, data.test)
model.sem.predictions <- predict( model.sem, data.test)
model.var.predictions <- predict( model.var$varresult$IS )
model.rvar.predictions <- predict( model.var.restricted$varresult$IS )

# Re-align predictions, i.e. return only the last valid predictions
valid.prediction.count <- length(model.lrm.predictions)
#model.lrm.predictions <- model.lrm.predictions
model.sem.predictions <- model.sem.predictions$eq3.pred
model.var.predictions <- model.var.predictions[(length(model.var.predictions)+1-valid.prediction.count):length(model.var.predictions)]
model.rvar.predictions <- model.rvar.predictions[(length(model.rvar.predictions)+1-valid.prediction.count):length(model.rvar.predictions)]
names(model.sem.predictions) <- names(model.lrm.predictions)
names(model.var.predictions) <- names(model.lrm.predictions)
names(model.rvar.predictions) <- names(model.lrm.predictions)
data.test.results <- data.frame(data.test$IS, model.lrm.predictions, model.sem.predictions, model.var.predictions, model.rvar.predictions)
names(data.test.results) <- c('Actual', 'LRM', 'SEM', 'VAR', 'RVAR')

# Count number of detected anomalies prior to error injection (false positives)
model.lrm.pre.error.counter <- 0
model.sem.pre.error.counter <- 0
model.var.pre.error.counter <- 0
model.rvar.pre.error.counter <- 0

