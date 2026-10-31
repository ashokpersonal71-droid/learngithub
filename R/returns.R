
# Investment Analysis and Portfolio Management
# Exercise 1: Calculating Stock Returns

# Stock prices across five trading days
prices <- c(100, 105, 103, 108, 112, 115)

# Calculate simple daily returns
returns <- diff(prices) / head(prices, -1)

# Display percentage returns
round(returns * 100, 2)

# Average daily return
mean_return <- mean(returns)
cat("Average daily return:",
    round(mean_return * 100, 2), "%\n")

# Standard deviation of returns
risk <- sd(returns)
cat("Daily volatility:",
    round(risk * 100, 2), "%\n")
