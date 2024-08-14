################################################################################
#                         Data Generating Processes
#
# In this script, we define the data generating processes (DGP) according to the
# setups derived from previous work, i.e., the DAGs introduced in Fig. 2 of
# Blesch et al. (2023).
################################################################################

# DAG 1 from Blesch et al. (2023) ----------------------------------------------
# all edges are directed from top to bottom (e.g. x1 -> x4)
#
#       X1    X2 
#       |     |
#       |     |
#       X4   X3
#        \   /
#          Y

dgp_blesch <- function(n, effect_size = 0.5, num_levels = 3, all_cont = FALSE) {
  
  # X1 (categorical)
  if (all_cont) {
    x1 <- list(value = rnorm(n, mean = 0, sd = 1))
    x1$internal <- x1$value
  } else {
    x1 <- quant_cut(rnorm(n, mean = 0, sd = 1), num_levels = num_levels)
  }
  

  # X2 (Gaussian)
  x2 <- rnorm(n, mean = 0, sd = 1)
  
  # X3 ~ β * X2 + ε (categorical)
  if (all_cont) {
    x3 <- list(value = x2 * effect_size + rnorm(n, mean = 0, sd = 1))
    x3$internal <- x3$value
  } else {
    x3 <- quant_cut(x2 * effect_size + rnorm(n, mean = 0, sd = 1), num_levels = num_levels)
  }
  
  # X4 ~ β * X1 + ε (Gaussian)
  x4 <- x1$internal * effect_size + rnorm(n, mean = 0, sd = 1)
  
  # Outcome Y ~ β * X3 - β * X4 + ε
  y <- x3$internal * effect_size - x4 * effect_size + rnorm(n, mean = 0, sd = 1)
  
  data.frame(y = y, x1 = x1$value, x2 = x2, x3 = x3$value, x4 = x4)
}

# Create batchtools wrapper for dgp_blesch
problem_blesch <- function(job, data, ...) {
  dgp_blesch(...)
}

# Helper functions -------------------------------------------------------------

# quant_cut: Quantile-based discretization
quant_cut <- function(to_quant_cut, num_levels) {
  quantiles <- quantile(to_quant_cut, probs = seq(0, 1, length.out = num_levels + 1))
  res <- cut(to_quant_cut, 
             breaks = quantiles, 
             labels = sample(LETTERS[1:num_levels]), 
             include.lowest = TRUE)
  res_internal <- internal_dummy_repr(res, num_levels)
  list(value = res, internal = res_internal)
}

# internal_dummy_repr: Internal representation of dummy variables
internal_dummy_repr <- function(to_dummy_code, num_levels) {
  dummy_weights <- seq(-1, 1, length.out = num_levels)
  dummy <- fastDummies::dummy_cols(to_dummy_code, remove_selected_columns = TRUE)
  as.vector(as.matrix(dummy) %*% as.vector(dummy_weights))
}

