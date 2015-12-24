require("systemfit")
require("zoo")
require("vars")
require("tseries")
require("forecast")
require("tictoc")

source('models.R')
tic("Testing")
source('test.R')
toc()

source('report.R')

write.csv(results.final.df, file="results.csv")
