
# Automated Tests for Portfolio Calculations
# IAPM GitHub Lesson 6

# Function to calculate portfolio metrics
portfolio_metrics <- function(
    weights, returns, volatility, correlation) {

  stopifnot(
    length(weights) == 2,
    length(returns) == 2,
    length(volatility) == 2,
    all(is.finite(c(
      weights, returns, volatility, correlation
    ))),
    all(weights >= 0),
    abs(sum(weights) - 1) < 1e-10,
    all(volatility >= 0),
    length(correlation) == 1,
    abs(correlation) <= 1
  )

  expected_return <- sum(weights * returns)

  variance <-
    weights[1]^2 * volatility[1]^2 +
    weights[2]^2 * volatility[2]^2 +
    2 * weights[1] * weights[2] *
    volatility[1] * volatility[2] *
    correlation

  list(
    expected_return = expected_return,
    variance = variance,
    risk = sqrt(max(variance, 0))
  )
}

# Test inputs
w <- c(0.60, 0.40)
mu <- c(0.12, 0.08)
sd <- c(0.20, 0.10)
rho <- 0.30

result <- portfolio_metrics(w, mu, sd, rho)

# Test 1: Expected return
stopifnot(
  isTRUE(all.equal(
    result$expected_return,
    0.104,
    tolerance = 1e-10
  ))
)
cat("PASS: Expected return\n")

# Test 2: Portfolio variance
stopifnot(
  isTRUE(all.equal(
    result$variance,
    0.01888,
    tolerance = 1e-10
  ))
)
cat("PASS: Portfolio variance\n")

# Test 3: Portfolio volatility
stopifnot(
  isTRUE(all.equal(
    result$risk,
    sqrt(0.01888),
    tolerance = 1e-10
  ))
)
cat("PASS: Portfolio volatility\n")

# Test 4: Perfect positive correlation
perfect_corr <- portfolio_metrics(
  w, mu, sd, 1
)

expected_risk <- sum(w * sd)

stopifnot(
  isTRUE(all.equal(
    perfect_corr$risk,
    expected_risk,
    tolerance = 1e-10
  ))
)
cat("PASS: Perfect correlation test\n")

# Test 5: Invalid weights rejected
invalid_weights <- try(
  portfolio_metrics(
    c(0.70, 0.40), mu, sd, rho
  ),
  silent = TRUE
)

stopifnot(inherits(invalid_weights, "try-error"))
cat("PASS: Invalid weights rejected\n")

cat("\nAll portfolio tests passed!\n")
