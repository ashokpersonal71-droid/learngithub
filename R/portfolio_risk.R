
# Portfolio Risk and Return Analysis
# GitHub Lesson 3: Feature Branch Exercise

# Asset weights
w <- c(0.60, 0.40)

# Expected annual returns
mu <- c(0.12, 0.08)

# Annual standard deviations
sigma <- c(0.20, 0.10)

# Correlation between assets
rho <- 0.30

# Validate portfolio weights
stopifnot(
  length(w) == 2,
  all(is.finite(w)),
  all(w >= 0),
  abs(sum(w) - 1) < 1e-10
)

# Expected portfolio return
portfolio_return <- sum(w * mu)

# Covariance matrix
cov_matrix <- matrix(
  c(
    sigma[1]^2,
    rho * sigma[1] * sigma[2],
    rho * sigma[1] * sigma[2],
    sigma[2]^2
  ),
  nrow = 2,
  byrow = TRUE
)

# Portfolio variance
portfolio_variance <- as.numeric(
  t(w) %*% cov_matrix %*% w
)

# Portfolio standard deviation
portfolio_risk <- sqrt(portfolio_variance)

# Display results
cat(
  "Expected portfolio return:",
  round(portfolio_return * 100, 2),
  "%\n"
)

cat(
  "Portfolio variance:",
  round(portfolio_variance, 5),
  "\n"
)

cat(
  "Portfolio volatility:",
  round(portfolio_risk * 100, 2),
  "%\n"
)
