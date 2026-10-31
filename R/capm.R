
# Capital Asset Pricing Model

risk_free_rate <- 0.06
market_return <- 0.12
beta <- 1.20

expected_return <- risk_free_rate +
  beta * (market_return - risk_free_rate)

cat("Expected return:",
    round(expected_return * 100, 2), "%\n")
