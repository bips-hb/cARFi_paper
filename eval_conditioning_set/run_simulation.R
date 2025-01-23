################################################################################
#                 Simulation: Impact of the Conditioning Set
#
# This simulation is based on the DAG described in König et al. (2021) with an
# additional variable X5 which is highly correlated with X4 but without a
# direct effect on the outcome.
#
# Settings:
#     - Number of simulation runs: 50
#     - Number of samples: 3000
#     - Algorithms: PFI, SAGE, cARFi under different conditioning sets, CPI
#       (Gaussian), LOCO, and CS  
#     - Loss function: mean squared error (MSE)
#     - Learner: random forest (ranger) and linear model (regression)
#     - cARFi: Minimum node size of 20
#     - cARFi: Number of samples R = 1
################################################################################

# General setup
source(here::here("setup.R"))

library(batchtools)
library(here)
library(data.table)

# Set seed
set.seed(2024)

# Registry ---------------------------------------------------------------------
reg_name <- "Sim_conditioning_set"
if (!dir.exists(here("registries"))) dir.create(here("registries"))
reg_dir <- file.path(here("registries"), reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(
  file.dir = reg_dir, 
  conf.file = here("eval_conditioning_set/config.R"),
  source = here(c("eval_conditioning_set/problems.R", "methods/algorithms.R")),
  packages = c("Metrics", "cpi", "seqknockoff", "dplyr", "arf", "mlr3", "cs",
               "mlr3learners", "knockoff", "microbenchmark", "parallel",
               "doParallel")
)

# Problems ---------------------------------------------------------------------
source(here("eval_conditioning_set/problems.R"))

addProblem(name = "Conditioning_set", fun = problem_koenig, seed = 1)

# Algorithms -------------------------------------------------------------------
source(here("methods/algorithms.R"))

addAlgorithm(name = "carfi", fun = carfi_wrapper)
addAlgorithm(name = "cpi_gauss", fun = cpi_gauss)
addAlgorithm(name = "loco", fun = loco)
addAlgorithm(name = "pfi", fun = pfi)
addAlgorithm(name = "sage", fun = run_sage)
addAlgorithm(name = "cs", fun = cond_subgroup)

# Global Parameters ------------------------------------------------------------
carfi_feat_conds <- list(list(NULL, "x1", "x2", "x3", "x5", c("x1", "x2"), c("x3", "x4")))
learners <- c("regression", "ranger")

# Experiments ------------------------------------------------------------------

# Define problem and algorithm design
prob_designs <- list(Conditioning_set = expand.grid(n = c(3000)))
algo_designs <- list(
  carfi = expand.grid(learner = learners, feat_cond = carfi_feat_conds),
  cpi_gauss = expand.grid(learner = learners),
  loco = expand.grid(learner = learners),
  pfi = expand.grid(learner = learners),
  cs = expand.grid(learner = learners),
  sage = expand.grid(learner = learners)
)

# Add experiments
addExperiments(prob_designs, algo_designs, repls = 50)
summarizeExperiments()

# Test jobs --------------------------------------------------------------------
testJob(id = 1)

# Submit -----------------------------------------------------------------------
submitJobs(resources = list(name = reg_name, ncpus = 1, memory = 6000, 
                            walltime = 60*60, max.concurrent.jobs = 40))
waitForJobs()

# Get results ------------------------------------------------------------------
loadRegistry(reg_dir, conf.file = here("eval_conditioning_set/config.R"), writeable = FALSE)

res <- reduceResultsDataTable()
jobPars <- getJobPars(ids = res$job.id)
jobPars$algo.pars <- lapply(jobPars$algo.pars, function(x) { x[["feat_cond"]] <- NULL; x })
jobPars <- flatten(jobPars)
jobPars <- jobPars[rep(seq_len(nrow(jobPars)), unlist(lapply(res$result, nrow))), ]
result <- cbind(jobPars, rbindlist(res$result))
levels <- c("PFI", "SAGE", "cARFi", "cARFi (X1)", "cARFi (X2)", "cARFi (X3)", 
            "cARFi (X5)", "cARFi (X1, X2)", "cARFi (X3, X4)", 
            "CPI (Gauss.)", "CS", "LOCO")
result$method <- factor(result$method, levels = levels)
result$Variable <- factor(result$Variable, levels = paste0("X", 1:5))

# Uncomment to use the results shown in the paper
# result <- readRDS("final_results/res_conditioning_set.rds")

# Create plots -----------------------------------------------------------------
library(ggplot2)
library(ggsci)
library(cowplot)

# Prepare data
result <- result[, .(mean = mean(value), q1 = quantile(value, 0.25), q3 = quantile(value, 0.75)),
                 by = c("n", "learner", "method", "Variable")]
result$group <- factor(
  ifelse(result$method %in% c("PFI", "SAGE"), "Marginal",
         ifelse(result$method %in% c("cARFi", "CPI (Gauss.)", "CS", "LOCO"), 
                "Conditional on all", "Conditional on subset")),
  levels = c("Marginal", "Conditional on subset", "Conditional on all"))
result$learner <- factor(result$learner, levels = c("regression", "ranger"),
                         labels = c("Linear model", "Random forest"))

# Remove duplicates
result <- result[!(method == "cARFi (X1, X2)" & Variable %in% c("X1", "X2"))]

# Rename the conditional sets
result$method <- factor(result$method, levels = levels(result$method),
                        labels = c("PFI", "SAGE", "cARFi", "X1", "X2", "X3", 
                                   "X5", "X1, X2", "X3, X4", 
                                   "CPI (Gauss.)", "CS", "LOCO"))

# Generate plots
plots <- lapply(levels(result$group), function(grp) {
  # Get color palette
  col_pal <- switch (grp,
                     "Marginal" = c("gray25", "gray75"),
                     "Conditional on subset" = pal_npg()(10)[1:6],
                     "Conditional on all" = pal_npg()(10)[7:10])
  legend_title <- switch (grp,
                          "Marginal" = "",
                          "Conditional on subset" = "cARFi with\ncond. set",
                          "Conditional on all" = "")
  
  p <- ggplot(result[group == grp]) +
    geom_bar(aes(x = Variable, y = mean, fill = method), stat = "identity", position = position_dodge(0.9)) +
    geom_errorbar(aes(x = Variable, ymin = q1, ymax = q3, group = method), position = position_dodge(0.9), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(rows = vars(learner), cols = vars(group), scales = "free_y") + 
    scale_fill_manual(values = col_pal) +
    guides(fill = guide_legend(title = legend_title, nrow = 2)) +
    theme(legend.position = "top", 
          legend.box.spacing = ggplot2::unit(0, "pt"),
          legend.spacing.y = ggplot2::unit(0, 'pt'),
          plot.margin = ggplot2::margin(0, 0, 0, r = 3, unit = "pt"),
          legend.margin = ggplot2::margin(0, 0, 5, 0, unit = "pt")) +
    labs(fill = NULL, 
         y = if (grp == "Marginal") "Importance" else NULL, 
         x = NULL)
  
  # Remove facet labels if not last plot
  if (grp != "Conditional on all") {
    p <- p + theme(strip.text.y = element_blank())
  }
  
  p
})
p <- plot_grid(plotlist = plots, nrow = 1, rel_widths = c(1.1, 2, 2), align = "h")

# Save plots
ggsave(here("figures/tmp_plots/plot_conditioning_set.pdf"), height = 5.5, width = 12.5)


# Create final figure (i.e, add tikz plot)
library(tinytex)
if (!is_tinytex()) tinytex::install_tinytex() # install tinytex if not already installed
tlmgr_install(pkgs = c("tikz", "bm", "xcolor"))

tinytex::pdflatex(
  file = here("eval_conditioning_set/create_fig.tex"),
  pdf_file = here("figures/fig_conditioning_set.pdf")
)


