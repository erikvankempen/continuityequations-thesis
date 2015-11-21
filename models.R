# Load the systemfit library to create the SEM model
# Load the vars library to be able to use VAR() and the predict() function
# for VAR models. The zoo library is used for its time series functions
require("systemfit")
require("zoo")
require("vars")
require("tseries")

# Some properties of the model have to be predefined.
# model.lag.max: the maximum lag between all steps.
# t.threshold: threshhold value for the t-statistics in the iterative
# exlusion of uncorrelated variables.
model.lag.max <- 30
model.lag.avg <- 7
t.threshold   <- 2

# Data is read from a CSV file. The data consists of daily aggregates of
# quantities per process step. In this example there are three process steps:
# SO: sales order; GS: goods shipped; IS: invoice sent
data.file <- "Data/Sales-NL01-Quantities.csv"
#data.file <- "Data/Sales-DE01-Quantities.csv"
data.raw <- read.csv( file=data.file , sep=";", 
                      header=TRUE, colClasses=c('Date', 'numeric', 'numeric', 'numeric') )

# Missing dates in the provided CSV files are filled by merging an empty
# data frame containing all dates with the provided data. Missing dates
# between the first and last day of the provided data is filled with zeros.
data.empty <- data.frame(
  Date=seq.Date(from=as.Date( head(sort( data.raw[,1] ), 1 ) ),
                to=as.Date( tail( sort(data.raw[,1]), 1) ), by="1 day") )
data.merged <- merge( data.empty, data.raw, by = c("Date"), all.x=TRUE,
                      all.y=FALSE )
data.merged[ is.na(data.merged) ] <- 0

data.merged <- data.merged[, 2:4]

#data.merged <- sales.data

# Select observations for the training and validation subsets
data.training <- data.merged[ 1:200, ]
#data.training <- data.merged
data.validation <- data.merged[  201:nrow(data.merged), ]

# The SEM is modeled by using the systemfit function
model.sem.formulas = c(as.formula("SO ~ +IS + GS"), as.formula("GS ~ +SO"), as.formula("IS ~ +GS"))
model.sem <- systemfit( model.sem.formulas, method = "OLS", data=data.training )

# A linear model is created using the lm function. This models the invoiced
# amounts based on ordered and shipped amounts.
model.lrm <- lm(IS ~ . , data=data.training)


# A multipe time series object is created by using the ts function on the merged
# data frame. This mts object can be used by the vars package for modeling.
data.tseries <- ts( data = data.merged )

# The VAR is modeled by using the VAR function from the vars package based on
# the mts object. A maximum lag can be provided and since trend and constant terms
# should not be included in the model, type is set to none. In this case the model
# contains all of the variables restricted by lag.max (30) in this example.
model.var <- VAR( data.tseries, p=2, lag.max=model.lag.max, type="none" )

# The VAR model is restricted further to exclude all weakly correlated variables
# from the model.
model.var.restricted <- restrict( model.var, thresh=t.threshold, method = "ser" )

# An optional GARCH model is created based on the garch function from the tseries
# package
#model.garch.so <- garch( data.training$SO, order = c(1, 1), series = NULL )
#model.garch.gs <- garch( data.training$GS, order = c(1, 1), series = NULL )
#model.garch.is <- garch( data.training$IS, order = c(1, 1), series = NULL )

# Serveral built-in functions can be used to present the resulting models.
summary( model.lrm )
print( model.sem )
summary( model.var )
summary( model.var.restricted )
