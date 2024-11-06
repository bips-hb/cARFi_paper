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
  if (interactive()) {
    res <- readline(prompt = "Not all packages are installed. Install all required packages? [N/Y]")
  } else {
    message("Installing required packages!")
    res <- "Y"
  }
  
  if (tolower(res) == "y") {
    pak::pkg_install(c(
      "ggplot2", "batchtools", "data.table", "here", "envalysis", "ggpubr",
      "ggsci", "fastDummies", "Metrics", "mlr3", "mlr3learners", "mlr3verse",
      "ranger", "doParallel", "microbenchmark", "pak", "dplyr",
      "bips-hb/arf", "kormama1/seqknockoff",
      "christophM/paper_conditional_subgroups",
      "bips-hb/cpi"))
  } else {
    warning("Not all required packages are installed!")
  }
} else {
  # Check 'cpi' version
  if (utils::installed.packages()["cpi", "Version"] < "0.1.5") {
    if (interactive()) {
      res <- readline(prompt = "'cpi' package >= 0.1.5 is required. Install it? [N/Y]")
    } else {
      message("Installing correct version of R package 'cpi >= 0.1.5'.")
      res <- "Y"
    }
    
    if (tolower(res) == "y") {
      pak::pkg_install("bips-hb/cpi")
    } else {
      warning("Not all required packages are installed!")
    }
  }
  
  # Check 'arf' version
  if (utils::installed.packages()["arf", "Version"] < "0.2.2") {
    if (interactive()) {
      res <- readline(prompt = "'arf' package >= 0.2.2 is required for a fast execution. Install it? [N/Y]")
    } else {
      message("Installing correct version of R package 'arf >= 0.2.2'.")
      res <- "Y"
    }
    
    if (tolower(res) == "y") {
      pak::pkg_install("bips-hb/arf")
    } else {
      warning("Not all required packages are installed!")
    }
  }
  
  message("All required packages are installed!")
}
rm(required_packages)

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

# Create folders for all figures -----------------------------------------------
suppressPackageStartupMessages(library(here))

# For all figures
if (!dir.exists(here("figures"))) dir.create(here("figures"))

# Create subfolder for the appendix
if (!dir.exists(here("figures/appendix"))) dir.create(here("figures/appendix"))

# Create subfolder for plots to be combined to figures
# Create appendix folder
if (!dir.exists(here("figures/tmp_plots"))) dir.create(here("figures/tmp_plots"))



