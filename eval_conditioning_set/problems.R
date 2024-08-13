################################################################################
#                         Data Generating Processes
#
# In this script, we define the data generating processes (DGP) according to the
# setups derived from previous work, i.e., the DAGs introduced Fig. 3
# of König et al. (2021).
################################################################################

# DAG 2 from König et al. (2021) -----------------------------------------------
# all edges are directed from top to bottom and left to right (e.g. x1 -> x4)
#
#       X1 -- X2 
#       |     |
#       |     |
#       X4   X3
#        \   /  \
#          Y     X5

dgp_koenig <- function(n) {
  
  # Generate variables
  x1 <- rnorm(n, mean = 0, sd = 1)
  x2 <- x1 + rnorm(n, mean = 0, sd = 1)
  x3 <- x2 + rnorm(n, mean = 0, sd = 1)
  x4 <- x1 + rnorm(n, mean = 0, sd = 0.3)
  x5 <- x3 + rnorm(n, mean = 0, sd = 0.7)
  
  # Generate response
  y <- x3 + x4 + rnorm(n, mean = 0, sd = 0.5)
  
  # Return data.frame
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
}

# Create batchtools wrapper for dgp_koenig
problem_koenig <- function(job, data, ...) {
  dgp_koenig(...)
}