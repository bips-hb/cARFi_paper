################################################################################
#                        Simulation: Mixed data
#
# This simulation is based on the DAG described in Blesch et al. (2021) with 
# only continuous variables and and a setting where X1 and X3 are
# categorical variables with 10 levels each.
#
# Settings:
#     - Number of simulation runs: 500
#     - Number of samples: (inceasing sample sizes)
#     - Algorithms: cARFi, CPI (Gaussian) and CPU (Sequential)  
#     - Loss function: mean squared error (MSE)
#     - Learner: random forest (ranger)
#     - Number of CPUs: 5
#     - cARFi: Minimum node size of 20
#     - cARFi: Number of samples R = 1 and R = 20
################################################################################

# General setup
source(here::here("setup.R"))

library(batchtools)
library(here)
library(data.table)

setDTthreads(5)
set.seed(2024)

# Registry ---------------------------------------------------------------------
reg_name <- "Sim_mixed_data"
if (!dir.exists(here("registries"))) dir.create(here("registries"))
reg_dir <- file.path(here("registries"), reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(
  file.dir = reg_dir,
  conf.file = here("eval_mixed_data/config.R"),
  source = here(c("eval_mixed_data/problems.R", "methods/algorithms.R")),
  packages = c("fastDummies", "mvtnorm", "Metrics", "cpi", "seqknockoff", 
               "dplyr", "arf", "mlr3", "mlr3learners", "knockoff", "cs", 
               "microbenchmark", "parallel", "doParallel"))


# Problems ---------------------------------------------------------------------
source(here("eval_mixed_data/problems.R"))

addProblem(name = "DAG_Blesch_mixed", fun = problem_blesch, seed = 1)
addProblem(name = "DAG_Blesch_cont", fun = problem_blesch, seed = 2)

# Algorithms -------------------------------------------------------------------
source(here("methods/algorithms.R"))

addAlgorithm(name = "carfi", fun = carfi_wrapper)
addAlgorithm(name = "cpi_gauss", fun = cpi_gauss)
addAlgorithm(name = "cpi_seq", fun = cpi_seq)

# Global Parameters ------------------------------------------------------------
num_cpus <- 5

# Experiments -----------------------------------------------------------

# Problem design
DAG_Blesch_cont = expand.grid(
  n = round(10^seq(log10(50), log10(3000), length.out = 13)),
  all_cont = TRUE,
  effect_size =  0.5
)
DAG_Blesch_mixed = expand.grid(
  n = round(10^seq(log10(50), log10(5000), length.out = 13)),
  all_cont = FALSE,
  num_levels = 10,
  effect_size =  0.5
)

# Algorithm design
algo_design_blesch <- list(
  carfi = expand.grid(learner = "ranger", repls = c(1, 20), num_cpus = num_cpus),
  cpi_seq = expand.grid(learner = "ranger", repls = c(1, 20), num_cpus = num_cpus),
  cpi_gauss = expand.grid(learner = "ranger", repls = c(1, 20), num_cpus = num_cpus)
)


# Add experiments
addExperiments(
  list(DAG_Blesch_cont = DAG_Blesch_cont, DAG_Blesch_mixed = DAG_Blesch_mixed), 
  algo_design_blesch, 
  repls = 2
)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
testJob(id = 1)

# Submit -----------------------------------------------------------
submitJobs(resources = list(name = reg_name, ncpus = 1, memory = 6000, 
                            walltime = 60, max.concurrent.jobs = 40))

waitForJobs()

# Get results -------------------------------------------------------------
loadRegistry(reg_dir, conf.file = here("eval_mixed_data/config.R"), writeable = FALSE)

res <- reduceResultsDataTable()
jobPars <- batchtools::flatten(getJobPars(ids = res$job.id))
jobPars <- jobPars[rep(seq_len(nrow(jobPars)), unlist(lapply(res$result, nrow))), ]
result <- cbind(jobPars, rbindlist(res$result))
result[is.na(repls)]$repls <- 1

# To reproduce the exact results of the paper, uncomment the following line
#result <- readRDS(here("final_results/res_mixed_data.rds"))

# Create plots -----------------------------------------------------------------
library(ggplot2)
library(ggsci)
library(envalysis)
library(ggpubr)

# Plot Blesch et al.
res <- result[startsWith(problem, "DAG_Blesch") , 
              .(`Rejection Rate` = mean(rejected)), 
              by = c("n", "method", "Variable", "repls", "problem")]

res$method <- factor(res$method, levels = c("cARFi", "CPI (Seq.)", "CPI (Gauss.)"))


# Figure 1: Mixed data
p1 <- ggplot(res[problem == "DAG_Blesch_mixed", ]) +
  geom_line(aes(x = n, y = `Rejection Rate`, color = Variable, linetype = method)) +
  geom_point(aes(x = n, y = `Rejection Rate`, color = Variable, shape = method), alpha = 0.5) +
  facet_grid(rows = vars(repls), labeller = labeller(repls = function(x) paste0("R = ", x))) +
  scale_color_npg() +
  theme(plot.margin = margin(0,0,0,0),
        axis.title.x = element_text(margin = margin(0,0,0,0)),
        axis.title.y = element_text(margin = margin(0,8,0,0)),
        axis.text.x = element_text(size = 18)) +
  geom_hline(yintercept = 0.05) +
  scale_x_log10(expand = c(0.005, 0.005)) +
  labs(linetype = "Method", color = "Variable", shape = "Method", x = "Sample size",
       y = "Recetion proportion") +
  guides(color = "none", linetype = "none", shape = "none") 

p1_legend <- get_legend(
  p1 +
    theme(legend.position = "right",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 19),
          legend.key.size = ggplot2::unit(1.0, 'cm'), #change legend key size
          legend.key.height = ggplot2::unit(1.0, 'cm'), #change legend key height
          legend.key.width = ggplot2::unit(1.0, 'cm')) +
    guides(linetype = guide_legend(), shape = guide_legend(override.aes = list(size = 5)))
)

ggsave(here("figures/tmp_plots/plot_mixed_data.pdf"), plot = p1 , height = 4, width = 5.25)
ggsave(here("figures/tmp_plots/legend_mixed_data.pdf"), plot = as_ggplot(p1_legend), 
       height = 2, width = 2)


# Appendix (Continuous)
p2 <- ggplot(res[problem == "DAG_Blesch_cont", ]) +
  geom_line(aes(x = n, y = `Rejection Rate`, color = Variable, linetype = method)) +
  geom_point(aes(x = n, y = `Rejection Rate`, color = Variable, shape = method), alpha = 0.5) +
  facet_grid(cols = vars(repls), labeller = labeller(repls = function(x) paste0("R = ", x))) +
  scale_color_npg() +
  theme(legend.position = "top",
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.margin = ggplot2::margin(0, 0, 0, r = 10, unit = "pt")) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  scale_x_log10(expand = c(0.005, 0.005)) +
  labs(linetype = "Method", color = "Variable", shape = "Method", x = "Sample size",
       y = "Rejection proportion") +
  guides(color = "none", shape = guide_legend(override.aes = list(size = 3)))
ggsave(here("figures/tmp_plots/plot_mixed_data_cont.pdf"), plot = p2, 
       height = 5, width = 10)

# Time plots
res_time <- result[ , .(time_mean = mean(time)), 
                   by = c("problem", "n", "learner", "method", "repls")]
res_time$method <- factor(res_time$method, 
                          levels = c("cARFi", "CPI (Seq.)", "CPI (Gauss.)"))
res_time$problem <- factor(res_time$problem, levels = c("DAG_Blesch_mixed", "DAG_Blesch_cont"),
                           labels = c("Mixed data", "Continuous data"))

# Create time plot for first problem
p_time <- ggplot(res_time) +
  geom_line(aes(x = n, y = time_mean, color = method)) +
  geom_point(aes(x = n, y = time_mean, color = method)) +
  facet_grid(rows = vars(problem), cols = vars(repls), scales = "free",
             labeller = labeller(repls = function(x) paste0("R = ", x))) +
  scale_y_log10() +
  scale_color_npg() +
  theme(legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
        panel.grid.major = element_line(colour = "lightgray")) +
  scale_x_log10() +
  labs(y = "Time in sec. (mean)", color = "Method", x = "Sample size")
ggsave(here("figures/appendix/fig_time_appendix.pdf"),
       height = 6, width = 10)

# Create final figures (i.e, add tikz plot and legend together)
library(tinytex)
if (!is_tinytex()) tinytex::install_tinytex() # install tinytex if not already installed
tlmgr_install(pkgs = c("tikz", "bm", "xcolor"))

# Create Figure 2
tinytex::pdflatex(
  file = here("eval_mixed_data/create_fig_mixed.tex"),
  pdf_file = here("figures/fig_mixed_data.pdf")
)

# Create Appendix figure for continuous data
tinytex::pdflatex(
  file = here("eval_mixed_data/create_fig_cont.tex"),
  pdf_file = here("figures/appendix/fig_mixed_data_appendix.pdf")
)


