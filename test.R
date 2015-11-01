# Determine number of detected anomalies prior to error injection
data.test <- data.validation
threshold <- 10000
injection.size <- 10
repetitions <- 1000

# First predictions are calculated
model.lrm.predictions <- predict( model.lrm, data.test, interval="confidence")
model.sem.predictions <- predict( model.sem, data.test, interval="confidence" )
model.var.predictions <- predict( model.var$varresult$IS, interval="confidence" )
model.rvar.predictions <- predict( model.var.restricted$varresult$IS, interval="confidence" )

# Re-align predictions, i.e. return only the last valid predictions
valid.prediction.count <- length(model.lrm.predictions[,1])
model.lrm.predictions <- as.data.frame(model.lrm.predictions)
model.sem.predictions <- as.data.frame(model.sem.predictions)
model.var.predictions <- as.data.frame(model.var.predictions)
model.rvar.predictions <- as.data.frame(model.rvar.predictions)


model.sem.predictions <- model.sem.predictions[,c('eq3.pred', 'eq3.lwr', 'eq3.upr')]
model.var.predictions <- model.var.predictions[(nrow(model.var.predictions)+1-valid.prediction.count):nrow(model.var.predictions),]
model.rvar.predictions <- model.rvar.predictions[(nrow(model.rvar.predictions)+1-valid.prediction.count):nrow(model.rvar.predictions),]
names(model.lrm.predictions) <- c('lrm.pred', 'lrm.lwr', 'lrm.upr')
names(model.sem.predictions) <- c('sem.pred', 'sem.lwr', 'sem.upr')
names(model.var.predictions) <- c('var.pred', 'var.lwr', 'var.upr')
names(model.rvar.predictions) <- c('rvar.pred', 'rvar.lwr', 'rvar.upr')
data.test.results <- data.frame(data.test$IS, model.lrm.predictions, model.sem.predictions, model.var.predictions, model.rvar.predictions)
names(data.test.results) <- c('Actual', names(model.lrm.predictions), names(model.sem.predictions), names(model.var.predictions), names(model.rvar.predictions))

# Count number of detected anomalies prior to error injection (false positives)
model.lrm.pre.error.count <- nrow(subset(data.test.results, Actual <= lrm.lwr | Actual >= lrm.upr))
model.sem.pre.error.count <- nrow(subset(data.test.results, Actual <= sem.lwr | Actual >= sem.upr))
model.var.pre.error.count <- nrow(subset(data.test.results, Actual <= var.lwr | Actual >= var.upr))
model.rvar.pre.error.count <- nrow(subset(data.test.results, Actual <= rvar.lwr | Actual >= rvar.upr))


model.lrm.T1.error.count <- NULL
model.sem.T1.error.count <- NULL
model.var.T1.error.count <- NULL
model.rvar.T1.error.count <- NULL
model.lrm.T2.error.count <- 0
model.sem.T2.error.count <- 0
model.var.T2.error.count <- 0
model.rvar.T2.error.count <- 0

# Inject anomalies
for( i in 1:repetitions){
  # Select (5) random samples
  sample.selection <- sample( seq( 1, nrow( data.test ) ), injection.size )
  
  # Set selected observations to 0
  data.test.injected <- data.test.results
  data.test.injected$Actual[sample.selection] <- 0
  
  # Count Type I errors, i.e. data (not included in sample) does not comply with test interval
  model.lrm.T1.error.count <- c(model.lrm.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= lrm.lwr | Actual >= lrm.upr)))
  model.sem.T1.error.count <- c(model.sem.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= sem.lwr | Actual >= sem.upr)))
  model.var.T1.error.count <- c(model.var.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= var.lwr | Actual >= var.upr)))
  model.rvar.T1.error.count <- c(model.rvar.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= rvar.lwr | Actual >= rvar.upr)))
  
  model.lrm.T2.error.count <- c(model.lrm.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= lrm.lwr & Actual <= lrm.upr)))
  model.sem.T2.error.count <- c(model.sem.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= sem.lwr & Actual <= sem.upr)))
  model.var.T2.error.count <- c(model.var.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= var.lwr & Actual <= var.upr)))
  model.rvar.T2.error.count <- c(model.rvar.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= rvar.lwr & Actual <= rvar.upr)))
}

# Print results to screen
source('report.R')