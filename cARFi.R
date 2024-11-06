
library(mlr3)
library(mlr3learners)
library(foreach)
library(arf)
library(cpi)
library(data.table)



#' Conditional ARF Feature Importance
#'
#' @param task An mlr3 task.
#' @param learner An mlr3 learner.
#' @param resampling An mlr3 resampling object.
#' @param repls Number of replicates. For each replicate, one perturbed datasat is sampled and the loss is computed. The losses are then averaged over the replicates before calculating the CPI.
#' @param arf_args Arguments to pass to `arf::adversarial_rf()`.
#' @param forde_args Arguments to pass to `arf::forde()`.
#' @param feat_interest The feature names to calculate the feature importance for.
#' @param feat_cond The feature names to condition on.
#' @param vectorize Use vectorized conditioning. Requires arf 0.2.1 from this PR: https://github.com/bips-hb/arf/pull/17
#' @param arf_obj Argument to pass a fitted arf object
#' @param arf_forde Argument to pass a fitted density from an arf object
#' @param ... Arguments to pass to cpi::cpi().
#'
#' @return A data.table with conditional feature importance and test results.
#'
#' @example
#' library(mlr3)
#' carfi(
#'   task = tsk("mtcars"),
#'   learner = lrn("regr.ranger"),
#'   forde_args = list(finite_bounds = FALSE)
#' )
carfi <- function(task, learner,
                  resampling = rsmp("holdout"),
                  repls = 1,
                  arf_args = list(),
                  forde_args = list(),
                  feat_interest = NULL,
                  feat_cond = NULL,
                  vectorize = TRUE,
                  arf_obj = NULL,
                  arf_forde = NULL,
                  ...) {
  
  # Check 'cpi' package version
  if (utils::installed.packages()["cpi", "Version"] < "0.1.5") {
    stop("Please update the 'cpi' package to version 0.1.5 or higher.")
  }
  
  # Data
  x <- task$data(cols = task$feature_names)
  
  if(!is.null(arf_args$parallel)) {
    parallel <- arf_args$parallel
  } else {
    parallel <- T
  }
  
  # Fit ARF if not provided
  if (is.null(arf_obj)){
    arf <- do.call("adversarial_rf", c(arf_args, list(x = x)))
  } else {
    arf <-  arf_obj
  }
  
  # Estimate density if not provided
  if (is.null(arf_forde)){
    psi <- do.call("forde", args = c(forde_args, list(arf = arf, x = x, parallel = parallel)))
    # psi <- forde(arf, x, parallel = parallel)
  } else {
    psi <- arf_forde
  }
  
  # Features of interest
  if (is.null(feat_interest)) {
    # All features
    feat_interest <- task$feature_names
  }
  
  # Features to condition on
  if (is.null(feat_cond)) {
    # All features
    feat_cond <- task$feature_names
  }
  
  # Create data to replace
  if (vectorize & utils::packageVersion("arf") == "0.2.2") {
    feat_uncond <- setdiff(names(x), feat_cond)
    condition <- rbindlist(replicate(length(feat_interest), x, F))
    condition[,(feat_interest) := sapply(seq_along(feat_interest), \(i,data) {
      data[((i-1)*nrow(x) + 1):(i*nrow(x)),i] <- NA
      data[,.SD,.SDcols = i]
    }, .SD), .SDcols = feat_interest]
    
    if (length(feat_uncond > 0)) {
      condition[, (feat_uncond) := NA]
    }
    x_syn <- as.data.table(forge(psi, repls, condition, parallel = parallel))
    
    x_replace <- x_syn[,sapply(seq_along(feat_interest), \(i,data) {
      data[((i-1)*repls*nrow(x) + 1):(i*repls*nrow(x)),.SD, .SDcols = i]
    }, .SD), .SDcols = feat_interest]
  } else if (vectorize & utils::packageVersion("arf") == "0.2.1") {
    x_replace <- foreach(col = feat_interest, .combine = cbind) %dopar% {
      evi <- subset(x, select = setdiff(feat_cond, col))
      if (ncol(evi) == 0) {
        forge(psi, nrow(x))[, ..col]
      } else {
        forge(psi, repls, evidence = evi)[, ..col]
      }
    }
  } else {
    warning("Using slow version with ARF CRAN version ...")
    x_replace <- foreach(col = feat_interest, .combine = cbind) %do% {
      evi <- subset(x, select = setdiff(feat_cond, col))
      if (ncol(evi) == 0) {
        forge(psi, nrow(x))[, ..col]
      } else {
        foreach(i = seq_len(nrow(evi)), .combine = rbind) %dopar%
          forge(psi, repls, evidence = evi[i, , drop = FALSE])[, ..col]
      }
    }
  }
  
  # Split up in chunks according to repl
  x_replace[, .repl := rep(seq_len(repls), nrow(x))]
  x_replace <- split(x_replace, by = ".repl")
  
  # Replace data in x
  x_tilde <- lapply(x_replace, function(xr) {
    xr[, .repl := NULL]
    x_temp <- copy(x)
    x_temp[, (feat_interest)] <- xr
    x_temp
  })
  
  # Run CPI
  res <- as.data.table(
    cpi(task = task, learner = learner,
        resampling = resampling,
        x_tilde = x_tilde, ...))
  
  # TODO: Should not even calculate for the features we don't want -> implement in cpi package
  # Return only selected features
  res[Variable %in% feat_interest, ]
}