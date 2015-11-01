# Generate quantities

x <- seq(0,25,0.025)
y <- floor(sin(x) * 100 + 10000)
noise <- floor(rnorm(1001, 0, 100))

sales.data <- data.frame(matrix(0, nrow=1001, ncol=3))
names(sales.data) <- c("SO","GS", "IS")

sales.data$SO <- y + noise

for (i in 1:998){
  offset <- round(rnorm(1, 1, 0.5))
  quantity.first <- round(0.05 * sales.data$SO[i], 0)
  sales.data$GS[i + offset] <- sales.data$GS[i + offset] + quantity.first
  
  offset <- round(rnorm(1, 2, 0.5))
  quantity.second <- round(0.90 * sales.data$SO[i], 0)
  sales.data$GS[i + offset] <- sales.data$GS[i + offset] + quantity.second
  
  offset <- round(rnorm(1, 3, 0.5))
  quantity.third <- sales.data$SO[i] - quantity.first - quantity.second
  sales.data$GS[i + offset] <- sales.data$GS[i + offset] + quantity.third
}

for (i in 1:998){
  offset <- round(rnorm(1, 1, 0.5))
  quantity.first <- round(0.80 * sales.data$GS[i], 0)
  sales.data$IS[i + offset] <- sales.data$IS[i + offset] + quantity.first

  quantity.second <- sales.data$GS[i] - quantity.first
  sales.data$IS[i + 1] <- sales.data$IS[i + 1] + quantity.second
}

sum(sales.data$SO)
sum(sales.data$GS)
sum(sales.data$IS)
sum(sales.data$SO)/sum(sales.data$GS)
sum(sales.data$GS)/sum(sales.data$IS)

head(sales.data, 100)

plot(sales.data$SO[1:100])
plot(sales.data$GS[1:100])
plot(sales.data$IS[1:100])
