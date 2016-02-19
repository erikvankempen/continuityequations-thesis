require("systemfit")
require("zoo")
require("vars")
require("tseries")
require("forecast")
require("tictoc")

source('models.R')
tic("Testing")
for( i in seq( from=0, to=1, by=0.10 ) ) {
  injection.amplitude <- i
  source('test.R')
  source('report.R')
  write.csv(results.final.df, file=paste("Results/results-", as.character(injection.amplitude*100), ".csv", sep=""))
}
toc()



