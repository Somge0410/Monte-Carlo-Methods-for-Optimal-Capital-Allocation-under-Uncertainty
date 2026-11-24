
set.seed(43)

p <- 0.48
rounds <- 200
N <- 10000

# Generate 10,000 possible histories
wins <- matrix(
  rbinom(rounds * N, 1, p),
  nrow = rounds
)

analyze <- function(f) {
  returns <- ifelse(wins == 1, 1 + 1.8*f, 1 - f)

  # Wealth evolution, starting at 1
  wealth <- rbind(1, apply(returns, 2, cumprod))

  # Highest wealth reached so far
  peaks <- apply(wealth, 2, cummax)

  # Maximum drawdown in each history
  drawdowns <- apply(1 - wealth / peaks, 2, max)

  c(
    median_final_wealth = median(wealth[rounds + 1, ]),
    prob_drawdown_50 = mean(drawdowns >= 0.85)
  )
}
best<- optimize(function(f) analyze(f)[1], c(0, 1), maximum = TRUE)
print(best$maximum)
print(analyze(best$maximum))
print(sapply(c(0.15,0.2,0.25), analyze))
