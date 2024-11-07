################################################################################
#                 Simulation: Proof of Concept
#
# Settings:
#     - Number of simulation runs: 10000
#     - Number of samples: 1000
#     - Number of features: 10 (with a Toeplitz covariance matrix with base .5)
#     - Linear and nonlinear data
#     - Algorithms: cARFi
#     - Loss function: mean squared error (MSE)
#     - Learner: random forest, linear model, neural network, and support vector machine
#     - cARFi: Minimum node size of c(2, 5, 10, 20, 50, 100)
#     - cARFi: Number of samples R = 1
################################################################################

# General setup
source(here::here("setup.R"))

library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)

set.seed(42)

# Simulation parameters --------------------------------------------------------
num_replicates <- 10000 
n <- 1000
p <- 10
cov_base <- .5 # Toeplitz with base .5 covariance

# Algorithm parameters ---------------------------------------------------------
learners <- c("regr.lm", "regr.ranger", "regr.nnet", "regr.svm")
tests <- "t" #c("t", "fisher")
measures <- "regr.mse" #c("regr.mse", "regr.mae")
finite_bounds <- "local"
epsilon <- 1e-15
cpi_replicates <- 1
min_node_size <- c(2, 5, 10, 20, 50, 100)

# Registry ---------------------------------------------------------------------
reg_name <- "Sim_regr_toeplitz"
if (!dir.exists(here::here("registries"))) dir.create(here::here("registries"))
reg_dir <- here::here("registries", reg_name)
dir.create("registries", showWarnings = FALSE)
unlink(reg_dir, recursive = TRUE)
makeExperimentRegistry(file.dir = reg_dir,
                       conf.file = here::here("eval_proof_of_concept", "config.R"),
                       packages = c("mlr3verse", "cpi", "mvtnorm", "arf", "foreach"),
                       source = c(here::here("eval_proof_of_concept", "problems.R"),
                                  here::here("cARFi.R")))

# Problems ---------------------------------------------------------------------
addProblem(name = "linear", fun = linear_data, seed = 43)
addProblem(name = "nonlinear", fun = nonlinear_data, seed = 44)

# Algorithms -------------------------------------------------------------------
cpi_fun <- function(data, job, instance,
                    learner_name, finite_bounds = FALSE, epsilon = 0,
                    cpi_replicates = 1, min_node_size = 2,
                    ...) {
  learner <- switch(
    learner_name,
    regr.ranger = lrn(learner_name, num.trees = 500),
    regr.nnet = lrn(learner_name, size = 20, decay = .1, trace = FALSE),
    regr.svm = lrn(learner_name, kernel = "radial"),
    lrn(learner_name)
  )

  as.list(carfi(
    task = instance, learner = learner,
    repls = cpi_replicates,
    arf_args = list(max.depth = 0, parallel = FALSE, replace = FALSE, min_node_size = min_node_size),
    forde_args = list(finite_bounds = finite_bounds, epsilon = epsilon),
    ...
  ))
}
addAlgorithm(name = "cpi", fun = cpi_fun)

# Experiments ------------------------------------------------------------------
prob_design <- list(linear = expand.grid(n = n, p = p, outcome = "regr",
                                         cov_base = cov_base,
                                         stringsAsFactors = FALSE),
                    nonlinear = expand.grid(n = n, p = p, outcome = "regr",
                                            cov_base = cov_base,
                                            stringsAsFactors = FALSE))
algo_design <- list(cpi = expand.grid(
  learner_name = learners,
  test = tests,
  measure = measures,
  log = FALSE,
  finite_bounds = finite_bounds,
  epsilon = epsilon,
  cpi_replicates = cpi_replicates,
  min_node_size = min_node_size,
  stringsAsFactors = FALSE)
)
addExperiments(prob_design, algo_design, repls = num_replicates)
summarizeExperiments()
testJob(1)

# Submit -----------------------------------------------------------------------
if (grepl("^blog[12]$", system("hostname", intern = TRUE))) {
  ids = findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 300)]

  submitJobs(ids = ids,
             resources = list(
               partition = "teton,teton-knl",
               ncpus = 1,
               memory = 2 * 1024,
               walltime = 6 * 24 * 3600,
               chunks.as.arrayjobs = TRUE,
               max.concurrent.jobs = 5000L,
               comment = "pfiarf-regr-toepl"
             ))
} else {
  submitJobs()
}
waitForJobs()

# Get results ------------------------------------------------------------------
res <- reduceResultsList(fun = as.data.table)
names(res) = seq_along(res)

res = data.table::rbindlist(res, idcol = "job.id")
res[, job.id := as.integer(job.id)]

res = ijoin(res, unwrap(getJobPars()))

res[, Variable := factor(Variable,
                         levels = paste0("x", 1:unique(p)),
                         labels = paste0("X", 1:unique(p)))]
res[, Learner := factor(learner_name,
                        levels = c("regr.lm", "regr.svm", "regr.ranger", "regr.nnet"),
                        labels = c("Linear model", "Support vector machine", "Random forest", "Neural network"))]
res[, Problem := factor(problem,
                        levels = c("linear", "nonlinear"),
                        labels = c("Linear data", "Nonlinear data"))]
res[, Statistic := statistic]
saveRDS(res, here::here(paste0("eval_proof_of_concept/", reg_name, ".rds")))


# Create figures ---------------------------------------------------------------
source(here::here("eval_proof_of_concept", "plot_regr_toeplitz.R"))

