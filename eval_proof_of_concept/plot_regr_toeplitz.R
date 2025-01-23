library(data.table)
library(ggplot2)
library(ggsci)
library(envalysis)
library(ggpubr)
library(here)

# Set theme
theme_set(
  theme_publish(base_size = 11, base_family = "serif") +
  theme(
    panel.grid.minor = element_line("gray95"),
    panel.grid.major = element_line("lightgray")
  )
)

reg_name <- "Sim_regr_toeplitz"
# Plots ------------------------------------------------------------------------
res <- readRDS(here("eval_proof_of_concept/Sim_regr_toeplitz.rds"))

# Uncomment this if you want to use the results used in the paper
#res <- readRDS(here("final_results/res_proof_of_concept.rds"))

# Boxplots of CPI values per variable
plots_cpi <- lapply(unique(res$min_node_size), function(m) {
  ggplot(res[min_node_size == m, ], aes(x = Variable, y = CPI)) +
    geom_boxplot(outlier.size = .01) +
    facet_grid(Problem ~ Learner, scales = "free") +
    geom_hline(yintercept = 0, col = "red") +
    xlab("Variable") + ylab("Feature importance") + 
    theme(panel.grid.minor = element_line("gray95"),
          panel.grid.major = element_line("lightgray")
    )
})
names(plots_cpi) <- unique(res$min_node_size)

# Histograms of t-test statistics (only null variables)
plots_tstat <- lapply(unique(res$min_node_size), function(m) {
  ggplot(res[min_node_size == m & test == "t" & Variable %in% c("X1"), ], aes(Statistic)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100) +
    facet_grid(Problem ~ Learner) +
    stat_function(fun = dt, color = 'red', args = list(df = unique(res$n) - 1)) +
    xlab("Test statistic") + ylab("Density") + 
    theme(panel.grid.minor = element_line("gray95"),
          panel.grid.major = element_line("lightgray")
    )
})
names(plots_tstat) <- unique(res$min_node_size)

# Power (mean over replications)
res[, reject := p.value <= 0.05]
res_mean <- res[, .(power = mean(reject, na.rm = TRUE)), by = .(Problem, algorithm, Learner, test, Variable, measure, min_node_size)]
levels(res_mean$Variable) <- rep(seq(0, .9, length.out = 10), each = 1)
res_mean[, Variable := abs(as.numeric(as.character(Variable)))]
res_mean[, power := mean(power), by = list(Problem, algorithm, Learner, test, Variable, measure, min_node_size)]
res_mean[, Test := factor(test, levels = c("fisher", "t"), labels = c("Fisher", "t-test"))]
plots_power <- lapply(unique(res$min_node_size), function(m) {
  ggplot(res_mean[min_node_size == m, ], aes(x = Variable, y = power, col = Learner, shape = Learner)) +
    geom_line() + geom_point() +
    facet_wrap(~ Problem) +
    geom_hline(yintercept = 0.05, col = "black", linetype = "dashed") +
    scale_color_npg() +
    scale_y_continuous(breaks = c(0, .05, .25, .5, .75, 1), limits = c(0, 1)) +
    xlab("Effect size") + ylab("Rejection proportion") + 
    theme(legend.position = "right", legend.direction = "vertical")
})
names(plots_power) <- unique(res$min_node_size)

# Plot all in one plot
library(cowplot)
lapply(as.character(unique(res$min_node_size)), function(m) {
  p <- plot_grid(plots_cpi[[m]], plots_tstat[[m]], plots_power[[m]],
                 labels = "AUTO", ncol = 1)
  ggplot2::ggsave(here(paste0("figures/appendix/fig_", reg_name, "_", m, ".pdf")), 
                  plot = p, width = 10, height = 13)
})

# Plot type I error and power over min_node_size
res_mn <- res_mean[Variable %in% c(0.0, 0.4), ]
res_mn[, Plot := "Type I error"]
res_mn[Variable == 0.4, Plot := "Power at effect size 0.4"]
res_mn[, Plot := factor(Plot, levels = c("Type I error", "Power at effect size 0.4"))]

ggplot(res_mn, aes(x = min_node_size, y = power, col = Learner, shape = Learner, linetype = Test)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(Plot), cols = vars(Problem), scales = "free") +
  geom_hline(yintercept = 0.05, col = "black", linetype = "dashed") +
  scale_color_npg() +
  #scale_y_continuous(breaks = c(0, .05, .25, .5, .75, 1), limits = c(0, 1)) +
  xlab("Leaf size") + ylab("Rejection proportion")

ggplot2::ggsave(here(paste0("figures/appendix/fig_", reg_name, "_", "type1", ".pdf")), 
                width = 10, height = 8)

# Plot for main paper
ggplot(res_mean[min_node_size == 20, ], aes(x = Variable, y = power, col = Learner, shape = Learner)) +
  geom_line() + geom_point() +
  facet_wrap(~ Problem) +
  geom_hline(yintercept = 0.05, col = "black", linetype = "dashed") +
  scale_color_npg() +
  scale_y_continuous(breaks = c(.05, .25, .5, .75, 1), limits = c(0, 1)) +
  xlab("Effect size") + ylab("Rejection proportion") + 
  theme_publish(base_size = 16, base_family = "serif") +
  guides(col = guide_legend(title = "Learner", nrow = 2)) +
  theme(legend.position = "top", panel.grid.major.y = element_line("lightgray"),
        plot.margin = ggplot2::margin(0,0,0,0), 
        axis.title.x = element_text(margin = ggplot2::margin(0,0,0,0)),
        legend.margin = ggplot2::margin(0,0,5,0),
        legend.box.spacing = ggplot2::unit(0, "pt"),
        legend.spacing.y = ggplot2::unit(0, 'pt'),
        legend.key.size = ggplot2::unit(0, "pt")) 
ggsave(here("figures/fig_power.pdf"), width = 6.5, height = 3.2)
