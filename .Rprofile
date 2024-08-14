# Install required packages ----------------------------------------------------

required_packages <- c(
  # CRAN packages
  "ggplot2", "batchtools", "data.table", "here", "envalysis", "ggpubr",
  "ggsci", "fastDummies", "Metrics", "mlr3", "mlr3learners", "mlr3verse",
  "ranger", "doParallel", "microbenchmark", "pak", "dplyr",
  # Not CRAN packages
  "seqknockoff", "cs", "cpi"
)

if (!all(required_packages %in% rownames(utils::installed.packages()))) {
  # Load/install packages
  res <- readline(prompt = "Not all packages are installed. Install all required packages? [N/Y]")
  
  if (tolower(res) == "y") {
    pak::pkg_install(c(
      "ggplot2", "batchtools", "data.table", "here", "envalysis", "ggpubr",
      "ggsci", "fastDummies", "Metrics", "mlr3", "mlr3learners", "mlr3verse",
      "ranger", "doParallel", "microbenchmark", "pak", "dplyr",
      "bips-hb/arf", "kormama1/seqknockoff",
      "christophM/paper_conditional_subgroups",
      "bips-hb/cpi@x_tilde_list"))
  } else {
    warning("Not all required packages are installed!")
  }
  
  
} else {
  message("All required packages are installed!")
}


# Limit number of CPUs ---------------------------------------------------------
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

# Set global ggplot2 theme -----------------------------------------------------
library(ggplot2)
library(envalysis)

theme_set(
  theme_publish(base_family = "serif", base_size = 15) +
  theme(panel.grid.major.y = element_line("lightgray"))
)
