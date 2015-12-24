# Load the systemfit library to create the SEM model
# Load the vars library to be able to use VAR() and the predict() function
# for VAR models. The zoo library is used for its time series functions
require("systemfit")
require("zoo")
require("vars")
require("tseries")
require("forecast")

# Determine number of detected anomalies prior to error injection
data.test <- data.validation
injection.size <- 10
repetitions <- 1000

# First predictions are calculated
model.lrm.predictions <- predict( model.lrm, data.test, interval="confidence")
model.sem.predictions <- predict( model.sem, data.test, interval="confidence" )
model.var.predictions <- predict( model.var$varresult$IS, interval="confidence" )
model.rvar.predictions <- predict( model.var.restricted$varresult$IS, interval="confidence" )
model.arima.predictions <- data.frame(fitted(model.arima.is), fitted(model.arima.is) - 1.96*sqrt(model.arima.is$sigma2), fitted(model.arima.is) + 1.96*sqrt(model.arima.is$sigma2))

# Re-align predictions, i.e. return only the last valid predictions
valid.prediction.count <- length(model.lrm.predictions[,1])
model.lrm.predictions <- as.data.frame(model.lrm.predictions)
model.sem.predictions <- as.data.frame(model.sem.predictions)
model.var.predictions <- as.data.frame(model.var.predictions)
model.rvar.predictions <- as.data.frame(model.rvar.predictions)
model.arima.predictions <- as.data.frame(model.arima.predictions)


model.sem.predictions <- model.sem.predictions[,c('eq3.pred', 'eq3.lwr', 'eq3.upr')]
model.var.predictions <- model.var.predictions[(nrow(model.var.predictions)+1-valid.prediction.count):nrow(model.var.predictions),]
model.rvar.predictions <- model.rvar.predictions[(nrow(model.rvar.predictions)+1-valid.prediction.count):nrow(model.rvar.predictions),]
model.arima.predictions <- model.arima.predictions[(nrow(model.arima.predictions)+1-valid.prediction.count):nrow(model.arima.predictions),]
names(model.lrm.predictions) <- c('lrm.pred', 'lrm.lwr', 'lrm.upr')
names(model.sem.predictions) <- c('sem.pred', 'sem.lwr', 'sem.upr')
names(model.var.predictions) <- c('var.pred', 'var.lwr', 'var.upr')
names(model.rvar.predictions) <- c('rvar.pred', 'rvar.lwr', 'rvar.upr')
names(model.arima.predictions) <- c('arima.pred', 'arima.lwr', 'arima.upr')
data.test.results <- data.frame(data.test$IS, model.lrm.predictions, model.sem.predictions, model.var.predictions, model.rvar.predictions, model.arima.predictions)
names(data.test.results) <- c('Actual', names(model.lrm.predictions), names(model.sem.predictions), names(model.var.predictions), names(model.rvar.predictions), names(model.arima.predictions))

# Count number of detected anomalies prior to error injection (false positives)
model.lrm.pre.error.count <- nrow(subset(data.test.results, Actual <= lrm.lwr | Actual >= lrm.upr))
model.sem.pre.error.count <- nrow(subset(data.test.results, Actual <= sem.lwr | Actual >= sem.upr))
model.var.pre.error.count <- nrow(subset(data.test.results, Actual <= var.lwr | Actual >= var.upr))
model.rvar.pre.error.count <- nrow(subset(data.test.results, Actual <= rvar.lwr | Actual >= rvar.upr))
model.arima.pre.error.count <- nrow(subset(data.test.results, Actual <= arima.lwr | Actual >= arima.upr))
model.combi.pre.error.count <- nrow(subset(data.test.results, (Actual <= arima.lwr | Actual >= arima.upr) & (Actual <= rvar.lwr | Actual >= rvar.upr)))
model.combi2.pre.error.count <- nrow(subset(data.test.results, (Actual <= var.lwr | Actual >= var.upr) & (Actual <= rvar.lwr | Actual >= rvar.upr)))

model.lrm.T1.error.count <- NULL
model.sem.T1.error.count <- NULL
model.var.T1.error.count <- NULL
model.rvar.T1.error.count <- NULL
model.arima.T1.error.count <- NULL
model.combi.T1.error.count <- NULL
model.combi2.T1.error.count <- NULL
model.lrm.T2.error.count <- NULL
model.sem.T2.error.count <- NULL
model.var.T2.error.count <- NULL
model.rvar.T2.error.count <- NULL
model.arima.T2.error.count <- NULL
model.combi.T2.error.count <- NULL
model.combi2.T2.error.count <- NULL

# Inject anomalies
for( i in 1:repetitions){
  # Select (10) random samples
  sample.selection <- sample( seq( 1, nrow( data.test ) ), injection.size )
  
  # Set selected observations to 0
  data.test.injected <- data.test.results
  data.test.injected$Actual[sample.selection] <- 0
  
  # Count Type I errors, i.e. data (not included in sample) does not comply with test interval
  model.lrm.T1.error.count <- c(model.lrm.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= lrm.lwr | Actual >= lrm.upr)))
  model.sem.T1.error.count <- c(model.sem.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= sem.lwr | Actual >= sem.upr)))
  model.var.T1.error.count <- c(model.var.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= var.lwr | Actual >= var.upr)))
  model.rvar.T1.error.count <- c(model.rvar.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= rvar.lwr | Actual >= rvar.upr)))
  model.arima.T1.error.count <- c(model.arima.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], Actual <= arima.lwr | Actual >= arima.upr)))
  model.combi.T1.error.count <- c(model.combi.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], (Actual <= arima.lwr | Actual >= arima.upr) & (Actual <= rvar.lwr | Actual >= rvar.upr))))
  model.combi2.T1.error.count <- c(model.combi2.T1.error.count, nrow(subset(data.test.injected[-sample.selection,], (Actual <= var.lwr | Actual >= var.upr) & (Actual <= rvar.lwr | Actual >= rvar.upr))))
  
  model.lrm.T2.error.count <- c(model.lrm.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= lrm.lwr & Actual <= lrm.upr)))
  model.sem.T2.error.count <- c(model.sem.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= sem.lwr & Actual <= sem.upr)))
  model.var.T2.error.count <- c(model.var.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= var.lwr & Actual <= var.upr)))
  model.rvar.T2.error.count <- c(model.rvar.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= rvar.lwr & Actual <= rvar.upr)))
  model.arima.T2.error.count <- c(model.arima.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= arima.lwr & Actual <= arima.upr)))
  model.combi.T2.error.count <- c(model.combi.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= rvar.lwr & Actual <= rvar.upr)))
  model.combi2.T2.error.count <- c(model.combi2.T2.error.count, nrow(subset(data.test.injected[sample.selection,], Actual >= rvar.lwr & Actual <= rvar.upr)))
}

# Print results to screen
source('report.R')