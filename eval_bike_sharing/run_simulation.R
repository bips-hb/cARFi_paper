################################################################################
#                   Simulation: Bike-Sharing Dataset
# 
# Settings:
#     - Number of simulation runs: 50
#     - Algorithms: PFI and cARFi
#     - Loss function: root mean squared error (RMSE)
#     - Learner: random forest (ranger)
#     - cARFi: Minimum node size of 20
#     - cARFi: Number of samples R = 5
################################################################################

# General setup
source(here::here("setup.R"))

library(batchtools)
library(here)
library(data.table)

setDTthreads(5)
set.seed(2024)

# Registry ----------------------------------------------------------------
reg_name <- "Sim_bike_sharing"
if (!dir.exists(here("registries"))) dir.create(here("registries"))
reg_dir <- file.path(here("registries"), reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(
  file.dir = reg_dir, 
  conf.file = here("eval_bike_sharing/config.R"),
  source = here(c("methods/algorithms.R", "eval_bike_sharing/problems.R")),
  packages = c("fastDummies", "Metrics", "cpi", "ISLR2", "dplyr", "arf", 
               "mlr3", "here", "mlr3learners", "microbenchmark",
               "parallel", "doParallel")
)


# Problems -----------------------------------------------------------
source(here("eval_bike_sharing/problems.R"))

addProblem(name = "Bike", fun = problem_bike, seed = 1)

# Algorithms -----------------------------------------------------------
source(here("methods/algorithms.R"))

addAlgorithm(name = "carfi", fun = carfi_wrapper)
addAlgorithm(name = "pfi", fun = pfi)

# Global Parameters ------------------------------------------------------------
num_cpus <- 5
feat_cond <- list(list(
  NULL, # condition on all other
  # For variable 'hr'
  c("workingday"),
  c("temp"),
  c("temp", "season"),
  
  # For variable 'temp'
  c("season"),
  c("season", "hr")
))

# Experiments -----------------------------------------------------------
algo_design <- list(
  carfi = expand.grid(learner ="ranger", feat_cond = feat_cond,
                      use_rsme = TRUE, repls = 5, num_cpus = num_cpus),
  pfi = expand.grid(learner = "ranger", num_cpus = num_cpus,
                    use_rsme  = TRUE)
)


# Add experiments
addExperiments(list(Bike = data.frame()), algo_design, repls = 50)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
testJob(id = 1)

# Submit -----------------------------------------------------------
submitJobs(resources = list(name = reg_name, ncpus = 1, memory = 6000, 
                            walltime = 60, max.concurrent.jobs = 40))

waitForJobs()

# Get results -------------------------------------------------------------
loadRegistry(reg_dir, conf.file = here("eval_bike_sharing/config.R"), writeable = FALSE)

res <- reduceResultsDataTable()
jobPars <- getJobPars(ids = res$job.id)
jobPars$algo.pars <- lapply(jobPars$algo.pars, function(x) { x[["feat_cond"]] <- NULL; x })
jobPars <- flatten(jobPars)
jobPars <- jobPars[rep(seq_len(nrow(jobPars)), unlist(lapply(res$result, nrow))), ]
result <- cbind(jobPars, rbindlist(res$result))
result$feat_cond <- gsub(".*\\((.*)\\).*|.*", "\\1", result$method)
result$Variable <- factor(result$Variable, 
                          levels = c("Hr", "Temp", "Hum", "Season", "Workingday"),
                          labels =  c("Hour", "Temp.", "Humidity", "Season", "Workday"))

# Uncomment to use the results shown in the paper
# result <- readRDS(here("final_results/res_bike_sharing.rds"))


# Plot the results --------------------------------------------------------
library(ggplot2)
library(ggsci)
library(cowplot)

# Show only on all and on nothing
res1 <- result[feat_cond == ""]
res1$algorithm <- factor(res1$algorithm, 
                         levels = c("pfi", "carfi"), 
                         labels = c("PFI", "cARFi (all)"))
res1 <- res1[, .(mean = mean(value), q1 = quantile(value, 0.05), q3 = quantile(value, 0.95)), 
             by = c("Variable", "algorithm")]

p1 <- ggplot(res1, aes(x = Variable, y = mean, fill = algorithm)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_npg() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position = "top",
        plot.margin = ggplot2::margin(0, 0, 0, r = 5, unit = "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
        axis.title.x = element_text(margin = ggplot2::margin(b = 2, t = 2, unit = "pt"))) +
  labs(x = NULL, y = "Importance (RMSE)", fill = NULL)


# Hour plot
res_hr <- result[Variable == "Hour" & feat_cond %in% c("", "Workingday", "Temp", "Temp, Season")]
res_hr$method <- factor(res_hr$method,
                        levels = c("PFI", "cARFi (Workingday)", "cARFi (Temp)", "cARFi (Temp, Season)", "cARFi"),
                        labels = c("None\n(PFI)", "Workday", "Temp.", "Temp.\nSeason", "All"))
res_hr$Variable <- "Hour of the day"
p2 <- ggplot(res_hr) +
  geom_boxplot(aes(x = method, y = value), fill = c("#E64B35FF", "darkgray", "darkgray", "darkgray", "#4DBBD5FF")) +
  facet_grid(cols = vars(Variable)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, r = 5, unit = "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
        axis.title.x = element_text(margin = ggplot2::margin(b = 2, t = 2, unit = "pt"))) +
  labs(x = NULL, y = NULL)
  
# Temperature plot
res_temp <- result[Variable == "Temp." & feat_cond %in% c("", "Workingday", "Season", "Season, Hr")]
res_temp$method <- factor(res_temp$method, 
                          levels = c("PFI", "cARFi (Workingday)", "cARFi (Season)", "cARFi (Season, Hr)", "cARFi"),
                          labels = c("None\n(PFI)", "Workday", "Season", "Season\nHour", "All"))
res_temp$Variable <- "Temperature"

p3 <- ggplot(res_temp) +
  geom_boxplot(aes(x = method, y = value), fill = c("#E64B35FF", "darkgray", "darkgray", "darkgray", "#4DBBD5FF")) +
  facet_grid(cols = vars(Variable)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, r = 5, unit = "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
        axis.title.x = element_text(margin = ggplot2::margin(b = 2, t = 2, unit = "pt"))) +
  labs(x = NULL, y = NULL)


# Combine and save plots
p <- plot_grid(p1, p2, p3, ncol = 3, labels = c("A", "B", "C"), axis = "b", label_size = 20)
ggsave(here("figures/fig_bike_sharing.pdf"), p, width = 15, height = 3.5)


