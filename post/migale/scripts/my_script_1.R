create <- function(mu = 0) { rnorm(100, mean = mu) }
analyze  <- function(x) { mean(x) }
results <- numeric(100)
for (i in 1:100) {
  results[i] <- analyze(create())
}
saveRDS(results, "results.rds")