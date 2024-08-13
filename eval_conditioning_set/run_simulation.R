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

addAlgorithm(name = "cpi_arf", fun = cpi_arf_wrapper)
addAlgorithm(name = "cpi_gauss", fun = cpi_gauss)
addAlgorithm(name = "loco", fun = loco)
addAlgorithm(name = "pfi", fun = pfi)
addAlgorithm(name = "sage", fun = run_sage)
addAlgorithm(name = "cs", fun = cond_subgroup)

# Global Parameters ------------------------------------------------------------
carfi_feat_conds <- list(list(NULL, "x1", "x2", "x3", "x4", "x5", c("x1", "x2")))
learners <- c("regression", "ranger")

# Experiments ------------------------------------------------------------------

# Define problem and algorithm design
prob_designs <- list(Conditioning_set = expand.grid(n = c(3000)))
algo_designs <- list(
  cpi_arf = expand.grid(learner = learners, feat_cond = carfi_feat_conds),
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
levels <- c("PFI", "SAGE", "CARFFI", "CARFFI (x1)", "CARFFI (x2)", "CARFFI (x3)", 
            "CARFFI (x4)", "CARFFI (x5)", "CARFFI (x1, x2)", "CARFFI (x2, x4)", 
            "CPI (Gauss.)", "csPFI", "LOCO")
result$method <- factor(result$method, levels = levels)

# Create plots -----------------------------------------------------------------
library(ggplot2)
library(ggsci)
library(cowplot)

# Prepare data
result <- result[, .(mean = mean(value), sd = sd(value)),
                 by = c("n", "learner", "method", "Variable")]
result$group <- factor(
  ifelse(result$method %in% c("PFI", "SAGE"), "Marginal",
         ifelse(result$method %in% c("CARFFI", "CPI (Gauss.)", "csPFI", "LOCO"), 
                "Conditional on all", "Conditional on subset")),
  levels = c("Marginal", "Conditional on subset", "Conditional on all"))

# Remove duplicates
result <- result[!((method == "CARFFI (x1, x2)" & Variable %in% c("x1", "x2")) |
                    method == "CARFFI (x2, x4)" & Variable %in% c("x2", "x4"))]

# Generate plots
plots <- lapply(levels(result$group), function(grp) {
  # Get color palette
  col_pal <- switch (grp,
                     "Marginal" = c("gray25", "gray75"),
                     "Conditional on subset" = pal_npg()(10)[1:6],
                     "Conditional on all" = pal_npg()(10)[7:10])
  
  p <- ggplot(result[group == grp]) +
    geom_bar(aes(x = Variable, y = mean, fill = method), stat = "identity", position = position_dodge(0.9)) +
    geom_errorbar(aes(x = Variable, ymin = mean - sd, ymax = mean + sd, group = method), position = position_dodge(0.9), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(rows = vars(learner), cols = vars(group), scales = "free_y") + 
    scale_fill_manual(values = col_pal) +
    theme(legend.position = "top", plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) +
    labs(fill = NULL, 
         y = if (grp == "Marginal") "Importance" else "", 
         x = "Variable")
  
  # Remove facet labels if not last plot
  if (grp != "Conditional on all") {
    p <- p + theme(strip.text.y = element_blank())
  }
  
  p
})
p <- plot_grid(plotlist = plots, nrow = 1, rel_widths = c(1, 2, 2), align = "h")

# Save plots
if (!dir.exists(here("figures"))) dir.create(here("figures"))
ggsave(here("figures/fig_conditioning_set.pdf"), height = 5, width = 12)



