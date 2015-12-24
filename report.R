# Create results data frame
results.lrm.vector <- c(repetitions, length(data.test[,1]), injection.size, model.lrm.pre.error.count,
                        mean(model.lrm.T1.error.count), min(model.lrm.T1.error.count), max(model.lrm.T1.error.count), sd(model.lrm.T1.error.count),
                        mean(model.lrm.T2.error.count), min(model.lrm.T2.error.count), max(model.lrm.T2.error.count),
                        sd(model.lrm.T2.error.count))
results.sem.vector <- c(repetitions, length(data.test[,1]), injection.size, model.sem.pre.error.count, 
                        mean(model.sem.T1.error.count), min(model.sem.T1.error.count), max(model.sem.T1.error.count), sd(model.sem.T1.error.count),
                        mean(model.sem.T2.error.count), min(model.sem.T2.error.count), max(model.sem.T2.error.count),
                        sd(model.sem.T2.error.count))
results.var.vector <- c(repetitions, length(data.test[,1]), injection.size, model.var.pre.error.count, 
                        mean(model.var.T1.error.count), min(model.var.T1.error.count), max(model.var.T1.error.count), sd(model.var.T1.error.count),
                        mean(model.var.T2.error.count), min(model.var.T2.error.count), max(model.var.T2.error.count),
                        sd(model.var.T2.error.count))
results.rvar.vector <- c(repetitions, length(data.test[,1]), injection.size, model.rvar.pre.error.count, 
                         mean(model.rvar.T1.error.count), min(model.rvar.T1.error.count), max(model.rvar.T1.error.count), sd(model.rvar.T1.error.count),
                         mean(model.rvar.T2.error.count), min(model.rvar.T2.error.count), max(model.rvar.T2.error.count),
                         sd(model.rvar.T2.error.count))
results.combi.vector <- c(repetitions, length(data.test[,1]), injection.size, model.combi.pre.error.count, 
                          mean(model.combi.T1.error.count), min(model.combi.T1.error.count), max(model.combi.T1.error.count), sd(model.combi.T1.error.count),
                          mean(model.combi.T2.error.count), min(model.combi.T2.error.count), max(model.combi.T2.error.count),
                          sd(model.combi.T2.error.count))
results.combi2.vector <- c(repetitions, length(data.test[,1]), injection.size, model.combi2.pre.error.count, 
                          mean(model.combi2.T1.error.count), min(model.combi2.T1.error.count), max(model.combi2.T1.error.count), sd(model.combi2.T1.error.count),
                          mean(model.combi2.T2.error.count), min(model.combi2.T2.error.count), max(model.combi2.T2.error.count),
                          sd(model.combi2.T2.error.count))
results.arima.vector <- c(repetitions, length(data.test[,1]), injection.size, model.arima.pre.error.count, 
                          mean(model.arima.T1.error.count),  min(model.arima.T1.error.count), max(model.arima.T1.error.count), sd(model.arima.T1.error.count),
                          mean(model.arima.T2.error.count), min(model.arima.T2.error.count), max(model.arima.T2.error.count),
                          sd(model.arima.T2.error.count))
results.final.df <- data.frame(results.lrm.vector, results.sem.vector, results.var.vector, results.rvar.vector,
                               results.arima.vector, results.combi.vector, results.combi2.vector)
names(results.final.df) <- c("LRM", "SEM", "VAR", "RVAR", "ARIMA", "Combi ARIMA", "Combi VAR")
row.names(results.final.df) <- c("Repetitions", "N", "Injected anomalies", "Pre-test anomalies", "T1 mean", "T1 min", "T1 max", "T1 sd",
                                 "T2 mean", "T2 min", "t2 max", "T2 sd")

# Print results to screen
cat('*** Continuity Equations Test! ***\n\n')
cat('Data file used: ', data.file, '\n\n')
cat('Modeling was performed using the lm, systemfit and vars packages and resulted in the following\nmodels:\n')
cat('\n')
cat('* LRM *\n')
cat('Model: IS ~ SO + GS\n')
cat('R-squared (adjusted): ', summary(model.lrm)$adj.r.squared, '\n')
cat('\n')
cat('* SEM *\n')
cat('Model: IS ~ GS\n')
cat('\n')
cat('* VAR *\n')
cat('Model: y ~ ', toString(formula(model.var$varresult$IS)[3]), '\n')
cat('R-squared (adjusted): ', summary(model.var$varresult$IS)$adj.r.squared, '\n')
cat('\n')
cat('* RVAR *\n')
cat('Model: y ~ ', toString(formula(model.var.restricted$varresult$IS)[3]), '\n')
cat('R-squared (adjusted): ', summary(model.var.restricted$varresult$IS)$adj.r.squared, '\n')
cat('\n')

cat('Test was run ', repetitions, 'times, with ', injection.size, ' anomalies injected each repetition.\n\n')

cat('Average of Type I (false positive) errors:\n')
cat('LRM:        ', mean(model.lrm.T1.error.count), ' ( min: ', min(model.lrm.T1.error.count),' max: ', max(model.lrm.T1.error.count), ' stdev: ', sd(model.lrm.T1.error.count), ' pre-test: ', model.lrm.pre.error.count,')\n')
cat('SEM:        ', mean(model.sem.T1.error.count), ' ( min: ', min(model.sem.T1.error.count),' max: ', max(model.sem.T1.error.count), ' stdev: ', sd(model.sem.T1.error.count), ' pre-test: ', model.sem.pre.error.count,')\n')
cat('VAR:        ', mean(model.var.T1.error.count), ' ( min: ', min(model.var.T1.error.count),' max: ', max(model.var.T1.error.count), ' stdev: ', sd(model.var.T1.error.count), ' pre-test: ', model.var.pre.error.count,')\n')
cat('RVAR:       ', mean(model.rvar.T1.error.count), ' ( min: ', min(model.rvar.T1.error.count),' max: ', max(model.rvar.T1.error.count), ' stdev: ', sd(model.rvar.T1.error.count), ' pre-test: ', model.rvar.pre.error.count,')\n')
cat('RVAR->VAR:  ', mean(model.combi.T1.error.count), ' ( min: ', min(model.combi.T1.error.count),' max: ', max(model.combi.T1.error.count), ' stdev: ', sd(model.combi.T1.error.count), ' pre-test: ', model.combi.pre.error.count,')\n')
cat('ARIMA(IS):  ', mean(model.arima.T1.error.count), ' ( min: ', min(model.arima.T1.error.count),' max: ', max(model.arima.T1.error.count), ' stdev: ', sd(model.arima.T1.error.count), ' pre-test: ', model.arima.pre.error.count,')\n')
cat('\n')

cat('Average of Type II (false negative) errors:\n')
cat('LRM:        ', mean(model.lrm.T2.error.count), ' ( min: ', min(model.lrm.T2.error.count),' max: ', max(model.lrm.T2.error.count), ' stdev: ', sd(model.lrm.T2.error.count), ' injected: ', injection.size,')\n')
cat('SEM:        ', mean(model.sem.T2.error.count), ' ( min: ', min(model.sem.T2.error.count),' max: ', max(model.sem.T2.error.count), ' stdev: ', sd(model.sem.T2.error.count), ' injected: ', injection.size,')\n')
cat('VAR:        ', mean(model.var.T2.error.count), ' ( min: ', min(model.var.T2.error.count),' max: ', max(model.var.T2.error.count), ' stdev: ', sd(model.var.T2.error.count), ' injected: ', injection.size,')\n')
cat('RVAR:       ', mean(model.rvar.T2.error.count), ' ( min: ', min(model.rvar.T2.error.count),' max: ', max(model.rvar.T2.error.count), ' stdev: ', sd(model.rvar.T2.error.count), ' injected: ', injection.size,')\n')
cat('RVAR->VAR:  ', mean(model.combi.T2.error.count), ' ( min: ', min(model.combi.T2.error.count),' max: ', max(model.combi.T2.error.count), ' stdev: ', sd(model.combi.T2.error.count), ' injected: ', injection.size,')\n')
cat('ARIMA(IS):  ', mean(model.arima.T2.error.count), ' ( min: ', min(model.arima.T2.error.count),' max: ', max(model.arima.T2.error.count), ' stdev: ', sd(model.arima.T2.error.count), ' injected: ', injection.size,')\n')
