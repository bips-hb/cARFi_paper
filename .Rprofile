## Limit number of CPUs
# For e.g. XGBoost
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
Sys.setenv(MC.CORES = 1)
# Unsure, MKL is an Intel-specific thing
Sys.setenv(MKL_NUM_THREADS = 1)
# Package-specific settings
try(data.table::setDTthreads(1))
try(RhpcBLASctl::blas_set_num_threads(1))
try(RhpcBLASctl::omp_set_num_threads(1))

# Set global ggplot2 theme
library(ggplot2)
library(envalysis)

theme_set(
  theme_publish(base_family = "serif", base_size = 13) +
  theme(panel.grid.major.y = element_line("lightgray"))
)
