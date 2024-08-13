################################################################################
#                             Algorithms
################################################################################

# CPI ARF ----------------------------------------------------------------------
cpi_arf_wrapper <- function(data, job, instance, learner, num_cpus = 1,
                            feat_cond = NULL, repls = 1, min_node_size = NULL,
                            forde_args = list(finite_bounds = "local", epsilon = 1e-15),
                            arf_args = list(parallel = TRUE, min_node_size = 20L, replace = FALSE),
                            use_rmse = FALSE,
                            ...) {
  source(here("cpi_arf.R"))
  
  # Set min_node_size if provided
  if (!is.null(min_node_size)) arf_args$min_node_size <- min_node_size
  
  # Set number of CPUs
  doParallel::registerDoParallel(num_cpus)
  
  # Create mlr3 instance
  my_mlr3 <- make_mlr3(instance = instance, learner = learner, use_rmse = use_rmse)
  x <- my_mlr3$task$data(cols = my_mlr3$task$feature_names)
  
  # Fit ARF
  arf <- do.call("adversarial_rf", c(arf_args, list(x = x)))
  
  # Create forde
  psi <- do.call("forde", args = c(forde_args, list(arf = arf, x = x, parallel = TRUE)))
  
  res <- lapply(feat_cond, function(cond) {
    # Remove feature if it is the only condition
    if (length(cond) > 0) {
      feat_interest <- setdiff(colnames(instance), c(cond, "y"))
    } else {
      feat_interest <- NULL
    }
    
    # Apply CPI ARF
    time <- apply_fun({
      CPI_r <- cpi_arf(task = my_mlr3$task,
                       arf_obj = arf,
                       arf_forde = psi,
                       repls = repls,
                       learner = my_mlr3$learner,
                       resampling = rsmp("holdout"),
                       log = FALSE,
                       feat_cond = cond,
                       feat_interest = feat_interest,
                       test = "t")
    })
    
    # Return result
    data.frame(
      Variable = CPI_r$Variable,
      value = CPI_r$CPI,
      SE = CPI_r$SE,
      p.value = ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value),
      rank = rank(-CPI_r$CPI),
      rejected = (ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value) < 0.05) * 1,
      top_p = rank(-CPI_r$CPI) <= length(CPI_r$Variable) / 2,
      method = if (is.null(cond)) "CARFFI" else paste0("CARFFI (", paste(cond, collapse = ", "), ")"),
      time = time
    )
  })
  
  do.call("rbind", res)
}

# CPI Sequential Knockoff ------------------------------------------------------
cpi_seq <- function(data, job, instance, learner, num_cpus = 1, repls = 1, use_rmse = FALSE, ...) {
  
  # Set number of CPUs
  doParallel::registerDoParallel(num_cpus)
  
  # Create mlr3 instance
  my_mlr3 <- make_mlr3(instance = instance, learner = learner, use_rmse = use_rmse)
  
  # Apply CPI with sequential knockoff
  time <- apply_fun({
    ko <- replicate(repls, seqknockoff::knockoffs_seq(as.data.frame(my_mlr3$task$data()[, -1]), parallel = TRUE),
                    simplify = FALSE)
    
    CPI_r <- cpi(task = my_mlr3$task,
                 learner = my_mlr3$learner,
                 resampling = rsmp("holdout"),
                 log = FALSE, 
                 x_tilde = ko,
                 test = "t")
  })
  
  
  data.frame(
    Variable = CPI_r$Variable,
    value = CPI_r$CPI,
    SE = CPI_r$SE,
    p.value = ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value),
    rank = rank(-CPI_r$CPI),
    rejected = (ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value) < 0.05) * 1,
    top_p = rank(-CPI_r$CPI) <= length(CPI_r$Variable) / 2,
    method = "CPI (Seq.)",
    time = time
  )
}

# CPI Gaussian Knockoff --------------------------------------------------------
cpi_gauss <- function(data, job, instance, learner, num_cpus = 1, repls = 1, use_rmse = FALSE, ...) {
  
  # Note: need to dummy encode data set
  my_mlr3 <- make_mlr3(instance = instance, learner = learner, dummy_encoding = TRUE, use_rmse = use_rmse)
  
  # Set number of CPUs
  doParallel::registerDoParallel(num_cpus)
  
  # Apply CPI with Gaussian knockoff
  time <- apply_fun({
    ko <- replicate(repls, create.second_order(as.matrix(my_mlr3$task$data()[, -1])),
                    simplify = FALSE)
    
    CPI_r <- cpi(task = my_mlr3$task,
                 learner = my_mlr3$learner,
                 resampling = rsmp("holdout"),
                 log = FALSE,
                 x_tilde = ko,
                 test = "t",
                 groups = my_mlr3$groups)
  })
  
  # Return result
  data.frame(
    Variable = CPI_r$Group,
    value = CPI_r$CPI,
    SE = CPI_r$SE,
    p.value = ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value),
    rank = rank(-CPI_r$CPI),
    rejected = (ifelse(CPI_r$p.value == 0, 1, CPI_r$p.value) < 0.05) * 1,
    top_p = rank(-CPI_r$CPI) <= length(CPI_r$Variable) / 2,
    method = "CPI (Gauss.)",
    time = time
  )
}


# LOCO -------------------------------------------------------------------------
loco <- function(data,job,  instance, learner, num_cpus = 1, use_rmse = FALSE, ...) {
  
  instance <- as.data.frame(instance)
  
  # full model 
  full_model <- make_mlr3(instance = instance, learner = learner, num_threads = num_cpus, use_rmse = use_rmse)
  
  # Use holdout resampling
  test_data <- instance[sample(1:nrow(instance), nrow(instance) %/% 3), ]
  
  time <- apply_fun({
    full_fit <- full_model$learner$train(full_model$task)
    full_pred <-  full_model$learner$predict_newdata(newdata = test_data)
    full_loss <-  full_pred$score(measures = full_model$measure)
    # leave one out
    # modify task:
    loco_res <-  lapply(2:ncol(instance), function(i) {
      red_model <- make_mlr3(instance = instance[, -i], learner = learner, num_threads = num_cpus, use_rmse = use_rmse)
      red_fit <- red_model$learner$train(red_model$task)
      red_pred <- red_model$learner$predict_newdata(newdata = test_data[,-i])                 
      red_loss <- red_pred$score(measures = red_model$measure)
      delta <- as.numeric(red_loss - full_loss)
      data.frame(Variable =  colnames(instance)[i], value = delta) # CAUTION -- might double check reordering in mlr3 of feature names if not X1--X2--X3 order
    })
    res = do.call("rbind", args = loco_res)
  })
  
  data.frame(
    res,
    SE = NA,
    p.value = NA,
    rank = rank(-res$value),
    rejected = NA,
    top_p = rank(-res$value) <= length(res$Variable) / 2,
    method = "LOCO",
    time = time
  )
}

# Conditional Subgroups --------------------------------------------------------
cond_subgroup <- function(data, job, instance, learner, num_cpus = 1, use_rmse = FALSE, ...) {
  
  instance <- as.data.frame(instance)
  
  model <- make_mlr3(instance = instance, learner = learner, num_threads = num_cpus, use_rmse = use_rmse)
  
  # Use holdout resampling
  test_data <- instance[sample(1:nrow(instance), nrow(instance) %/% 3), ]
  
  time <- apply_fun({
    fit <- model$learner$train(model$task)
    pred = Predictor$new(fit, data = test_data, y = "y")
    
    # 1. fit tree
    feature_names = setdiff(colnames(instance), "y")
    my_ctrl = partykit::ctree_control(maxdepth = 3, minbucket = 20, cores = num_cpus) 
    my_conds = fit_conditionals(data = instance[, feature_names], 
                                ctrl = my_ctrl, 
                                type = "trtf")
    
    # 2. calculate cPFI 
    res = grouped_pfi(pred = pred, 
                      loss = model$loss_subgroup, 
                      conds = my_conds, repetitions = 5) #special loss for cond subgroup approach -- double check! 
  })
  
  data.frame(
    Variable = res$feature,
    value = res$importance,
    SE = NA,
    p.value = NA,
    rank = rank(-res$importance),
    rejected = NA,
    top_p = rank(-res$importance) <= length(res$feature) / 2,
    method = "csPFI",
    time = time
  )
}

# Permutation Feature Importance -----------------------------------------------
pfi <- function(data,job,  instance, learner,  num_cpus = 1, use_rmse = FALSE, ...) {
  # Set number of CPUs
  doParallel::registerDoParallel(num_cpus)
  
  # Create mlr3 instance
  my_mlr3 <- make_mlr3(instance = instance, learner = learner, use_rmse = use_rmse)
  
  # Apply CPI with sequential knockoff
  time <- apply_fun({
    CPI_r <- cpi(task = my_mlr3$task,
                 learner = my_mlr3$learner,
                 resampling = rsmp("holdout"),
                 log = FALSE, 
                 knockoff_fun = function(x) apply(x, 2, sample),
                 test = "t")
  })
  
  
  data.frame(
    Variable = CPI_r$Variable,
    value = CPI_r$CPI,
    SE = NA, # not proven yet
    p.value = NA, # not proven yet
    rank = rank(-CPI_r$CPI),
    rejected = NA, # <- not proven yet 
    top_p = rank(-CPI_r$CPI) <= length(CPI_r$Variable) / 2,
    method = "PFI",
    time = time
  )
}

run_sage <- function(data, job, instance, learner, num_cpus = 1, use_rmse = FALSE, ...) {
  
  instance <- as.data.frame(instance)
  
  source(here("methods/SAGE_R.R"))
  data.table::setDTthreads(num_cpus)
  model <- make_mlr3(instance = instance, learner = learner, num_threads = num_cpus, use_rmse = use_rmse)
  
  # Use holdout resampling
  test_data <- instance[sample(1:nrow(instance), nrow(instance) %/% 3), ]
  
  time <- apply_fun({
    fit <- model$learner$train(model$task)
    res <- sage(model = fit, data = test_data, target = "y", loss = model$loss_instance)
  })
  
  data.frame(
    Variable = names(res),
    value = as.numeric(res),
    SE = NA,
    p.value = NA,
    rank = rank(-as.numeric(res)),
    rejected = NA,
    top_p = rank(-as.numeric(res)) <= length(res) / 2,
    method = "SAGE",
    time = time
  )
}

################################################################################
#                               Helper functions
################################################################################

# Define function for applying a function including time measurement
apply_fun <- function(..., name = "") {
  time <- microbenchmark(..., times = 1L)$time / 1e9
  time
}

dummy_encoding <- function(x) {
  if (is.numeric(as.matrix(x))) {
    res <- x
  } else {
    res <- dummy_cols(x, 
               remove_selected_columns = TRUE, 
               remove_first_dummy = TRUE)
    res <- res[, c("y", sort(colnames(res)[-1]))]
  }
}

make_mlr3 <- function(instance, learner, num_threads = 1, dummy_encoding = FALSE,
                      use_rmse = FALSE) {
  
  # Apply dummy encoding if requested
  if (dummy_encoding) {
    instance <-  dummy_encoding(instance)
    
    # Define groups of dummys that encode the same variable
    feat_names <- colnames(instance[, -1])
    vars = unique(gsub("_.*", "", feat_names))
    level_cols <- lapply(vars, FUN = function(x) which(x == gsub("_.*", "", feat_names)))
    grps = mapply(function(vars, level_cols) { level_cols }, vars, level_cols, 
                  SIMPLIFY = FALSE, USE.NAMES = TRUE)
  } else {
    grps = NULL
  }
  
  if (learner == "regression") {
    my_learner <- lrn("regr.lm", predict_type = "response")
    set_threads(my_learner, num_threads)
  } else if (learner == "ranger") {
    my_learner <- lrn("regr.ranger", predict_type = "response")
    my_learner$param_set$values = list(num.threads = num_threads)
  } else {
    stop("Learner not supported! Only 'regression' and 'ranger' are supported.")
  }
  
  list(
    "learner" = my_learner, 
    "task" = as_task_regr(x = instance, target = "y"), 
    "measure" = if (use_rmse) msr("regr.rmse") else msr("regr.mse"), 
    "loss" = if (use_rmse) Metrics::rmse else Metrics::mse, 
    "loss_instance"= Metrics::se,
    "loss_subgroup" = Metrics::rmse,
    "groups" = grps
  )
}
