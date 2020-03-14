## Arguments
args <- commandArgs(trailingOnly = TRUE)
id <- as.integer(args[1])
## Computations
create <- function(mu = 0) { rnorm(100, mean = mu) }
analyze  <- function(x) { mean(x) }
result <- analyze(create())
## Results
saveRDS(object = result, file = paste0("result_", id, ".rds"))