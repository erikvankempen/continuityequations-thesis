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

# Count number of detected anomalies prior to error injection
model.lrm.pre.error.counter <- 0
model.sem.pre.error.counter <- 0
model.var.pre.error.counter <- 0
model.rvar.pre.error.counter <- 0

for ( i in 1:nrow(data.test) ) {
  if( abs( data.test$IS[i] -
             model.lrm.predictions[i] ) > threshold ) {
    model.lrm.pre.error.counter <- model.lrm.pre.error.counter + 1
  }
  
	if( abs( data.test$IS[i] -
		model.sem.predictions$eq3.pred[i] ) > threshold ) {
			model.sem.pre.error.counter <- model.sem.pre.error.counter + 1
	}
	
	# VAR test
	if( abs( data.test$IS[i] -
		model.var.predictions[i-26] ) > threshold ) {
			model.var.pre.error.counter <- model.var.pre.error.counter + 1
	}
	
	# RVAR test
	if( abs( data.test$IS[i] -
		model.rvar.predictions[i-26] ) > threshold ) {
			model.rvar.pre.error.counter <- model.rvar.pre.error.counter + 1
	}
}

print( "Number of detected anomalies prior to injection:" )
cat( "LRM: ", model.lrm.pre.error.counter )
cat( "SEM: ", model.sem.pre.error.counter )
cat( "VAR: ", model.var.pre.error.counter )
cat( "RVAR: ", model.rvar.pre.error.counter )

# Determine number of detected anomalies after error injection
# Set counters
model.sem.error.counter <- 0
model.var.error.counter <- 0
model.rvar.error.counter <- 0

for ( i in 1:repetitions ) {
	# Select (5) random samples
	sample.selection <- sample( seq( 1, nrow( data.test ) ), injection.size )
	
	# Set selected observations to 0
	data.test$IS[sample.selection] <- 0
	
	# Count number of Type II errors (false negatives)
	for ( j in 1:injection.size ) {
		# SEM test
		if( abs( data.test$IS[sample.selection[j]] -
			model.sem.predictions$eq3.pred[sample.selection[j]] ) > threshold ) {
				model.sem.error.counter <- model.sem.error.counter + 1
		}
		
		# VAR test
		if( abs( data.test$IS[sample.selection[j]] -
			model.var.predictions[sample.selection[j]] ) > threshold ) {
				model.var.error.counter <- model.var.error.counter + 1
		}
		
		# RVAR test
		if( abs( data.test$IS[sample.selection[j]] -
			model.rvar.predictions[sample.selection[j]] ) > threshold ) {
				model.rvar.error.counter <- model.rvar.error.counter + 1
		}
	}
}

model.sem.error.avg <- model.sem.error.counter / repetitions
model.var.error.avg <- model.var.error.counter / repetitions
model.rvar.error.avg <- model.rvar.error.counter / repetitions

print( "Average number of Type II errors after injection:" )
print( "SEM: ", model.sem.error.avg )
print( "VAR: ", model.var.error.avg )
print( "RVAR: ", model.rvar.error.avg )
