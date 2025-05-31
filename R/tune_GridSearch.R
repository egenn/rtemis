# tune_GridSearch.R
# ::rtemis::
# 2025 EDG rtemis.org

#' \pkg{rtemis} internal: Grid Search for Hyperparameter Tuning of \pkg{rtemis} Learners
#'
#' Train models using a combination of parameter values for model selection
#'
#' Note that weights, if defined (and not NULL), should be passed directly to `grid_search`
#' as they need to be resampled along `x` and `y`, and should not be passed along with
#' `grid_params`. `ifw` and `ifw_type` should be passed as part of `grid_params`
#' and will be passed on to the learner.
#' Includes a special case for training [s_H2OGBM] or [s_GBM] which requires extracting and averaging n.trees
#' along with params.
#'
#' @param x data.frame or similar: Training set.
#' @param hyperparameters List: Hyperparameters.
#' @param tuner_parameters List: Tuner parameters.
#' @param weights Vector: Class weights.
#' @param verbosity Integer: Verbosity level.
#'
#' @return GridSearch object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tune_GridSearch <- function(
  x,
  hyperparameters,
  tuner_parameters,
  weights = NULL,
  save_mods = FALSE,
  verbosity = 1L,
  parallel_type = "none"
) {
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(tuner_parameters, TunerParameters)
  stopifnot(needs_tuning(hyperparameters))

  # Dependencies ----
  check_dependencies(c("future", "future.apply"))

  # Intro ----
  start_time <- intro(
    newline_pre = TRUE,
    caller = "tune_GridSearch",
    verbosity = verbosity - 1L
  )

  # Arguments ----
  algorithm <- hyperparameters@algorithm

  # Make Grid ----
  grid_params <- get_params_need_tuning(hyperparameters)
  n_params <- length(grid_params)
  n_resamples <- tuner_parameters[["resampler_parameters"]][["n"]]
  search_type <- tuner_parameters[["search_type"]]
  # expand_grid convert NULL to "null" for expansion to work.
  param_grid <- expand_grid(grid_params, stringsAsFactors = FALSE)
  # param_grid <- expand.grid(grid_params, stringsAsFactors = FALSE)
  param_grid <- cbind(param_combo_id = seq_len(NROW(param_grid)), param_grid)
  n_param_combinations <- NROW(param_grid)
  res_param_grid <- expand_grid(
    c(list(resample_id = seq_len(n_resamples)), grid_params),
    stringsAsFactors = FALSE
  )
  n_res_x_comb <- NROW(res_param_grid)
  if (search_type == "randomized") {
    index_per_resample <- sample(
      n_param_combinations,
      round(tuner_parameters[["randomize_p"]] * n_param_combinations)
    )
    res_param_grid <- res_param_grid[rep(index_per_resample, n_resamples), ]
  }

  # Resamples ----
  res <- resample(
    x = x,
    parameters = tuner_parameters[["resampler_parameters"]],
    verbosity = verbosity
  )

  # learner1 ----
  if (parallel_type != "mirai") {
    ptn <- progressr::progressor(steps = NROW(res_param_grid))
  }
  learner1 <- function(
    index,
    x,
    res,
    res_param_grid,
    hyperparameters,
    weights,
    verbosity,
    save_mods,
    n_res_x_comb
  ) {
    if (verbosity > 0L) {
      msg2(
        "Running grid line #",
        hilite(index),
        "/",
        NROW(res_param_grid),
        "...",
        caller = "tune_GridSearch",
        sep = ""
      )
    }
    res1 <- res[[res_param_grid[index, "resample_id"]]]
    dat_train1 <- x[res1, ]
    weights1 <- weights[res1]
    dat_valid1 <- x[-res1, ]
    hyperparams1 <- hyperparameters
    hyperparams1 <- update(
      hyperparams1,
      as.list(res_param_grid[index, 2:NCOL(res_param_grid), drop = FALSE]),
      tuned = -9L # Hyperparameters are being tuned
    )

    mod1 <- do_call(
      "train",
      args = list(
        x = dat_train1,
        dat_validation = dat_valid1,
        algorithm = hyperparams1@algorithm,
        hyperparameters = hyperparams1,
        weights = weights1,
        verbosity = verbosity - 1L
      )
    )

    out1 <- list(
      id = index,
      resample_id = res_param_grid[index, "resample_id"],
      metrics_training = mod1@metrics_training,
      metrics_validation = mod1@metrics_validation,
      type = mod1@type,
      hyperparameters = hyperparams1
    )

    # Algorithm-specific params ----
    # => add to hyperparameters
    if (algorithm == "GLMNET") {
      out1[["hyperparameters"]]@hyperparameters[["lambda.min"]] <- mod1@model[[
        "lambda.min"
      ]]
      out1[["hyperparameters"]]@hyperparameters[["lambda.1se"]] <- mod1@model[[
        "lambda.1se"
      ]]
    }
    if (algorithm == "LightGBM") {
      # Check best_iter is meaningful, otherwise issue message and set to 100L
      best_iter <- mod1@model[["best_iter"]]
      if (is.null(best_iter) || best_iter == -1 || best_iter == 0) {
        info(bold(italic(
          "best_iter returned from lightgbm:",
          best_iter,
          " - setting to 100L"
        )))
        best_iter <- 100L
      }
      out1[["hyperparameters"]]@hyperparameters[["best_iter"]] <- best_iter
    }
    # if (algorithm %in% c("LINAD", "LINOA")) {
    #   out1$est.n.leaves <- mod1$mod$n.leaves
    # }
    # if (algorithm == "LIHADBoost") {
    #   out1$sel.n.steps <- mod1$mod$selected.n.steps
    # }
    if (save_mods) out1[["mod1"]] <- mod1
    if (parallel_type != "mirai") {
      ptn(sprintf("Tuning resample %i/%i", index, n_res_x_comb))
    }
    out1
  } # /learner1

  # Train Grid ----
  if (verbosity > 0L) {
    msg2(
      hilite("Tuning", algorithm, "by", search_type, "grid search.")
    )
    msg20(
      hilite(n_param_combinations),
      ngettext(
        n_param_combinations,
        " parameter combination x ",
        " parameter combinations x "
      ),
      hilite(n_resamples),
      " resamples: ",
      hilite(n_res_x_comb),
      " models total",
      # hilite(n_res_x_comb), " models total running on ",
      # singorplu(n_workers, "worker"),
      " (",
      Sys.getenv("R_PLATFORM"),
      ")."
    )
  }

  if (parallel_type == "none") {
    grid_run <- lapply(
      X = seq_len(n_res_x_comb),
      FUN = learner1,
      x = x,
      res = res,
      hyperparameters = hyperparameters,
      res_param_grid = res_param_grid,
      weights = weights,
      verbosity = verbosity,
      save_mods = save_mods,
      n_res_x_comb = n_res_x_comb
    )
  } else if (parallel_type == "future") {
    grid_run <- future.apply::future_lapply(
      X = seq_len(n_res_x_comb),
      FUN = learner1,
      x = x,
      res = res,
      hyperparameters = hyperparameters,
      res_param_grid = res_param_grid,
      weights = weights,
      verbosity = verbosity,
      save_mods = save_mods,
      n_res_x_comb = n_res_x_comb,
      future.seed = TRUE,
      future.globals = FALSE # See: https://github.com/futureverse/globals/issues/93
    )
  } else if (parallel_type == "mirai") {
    grid_run <- mirai::mirai_map(
      .x = seq_len(n_res_x_comb),
      .f = learner1,
      .args = list(
        x = x,
        res = res,
        hyperparameters = hyperparameters,
        res_param_grid = res_param_grid,
        weights = weights,
        verbosity = verbosity,
        save_mods = save_mods,
        n_res_x_comb = n_res_x_comb
      )
    )
  }

  # Metric ----
  type <- supervised_type(x)
  metric <- tuner_parameters@parameters[["metric"]]
  maximize <- tuner_parameters@parameters[["maximize"]]
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced_Accuracy"
    } else if (type == "Regression") {
      metric <- "MSE"
    } else {
      metric <- "Concordance"
    }
    tuner_parameters@parameters[["metric"]] <- metric
  }
  if (is.null(maximize)) {
    maximize <- metric %in%
      c("Accuracy", "Balanced_Accuracy", "Concordance", "Rsq", "r")
    tuner_parameters@parameters[["maximize"]] <- maximize
  }
  select_fn <- if (maximize) which.max else which.min
  verb <- if (maximize) "maximize" else "minimize"

  # Aggregate ----
  # Average test errors
  # if using mirai, wait for all to finish
  if (parallel_type == "mirai") {
    # Appease R CMD check
    .progress_cli <- NULL
    grid_run <- grid_run[.progress_cli]
    # grid_run <- mirai::collect_mirai(grid_run)
  }
  if (type %in% c("Regression", "Survival")) {
    metrics_training_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r[["metrics_training"]]@metrics)
    )))
    metrics_validation_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r[["metrics_validation"]]@metrics)
    )))
  } else if (type == "Classification") {
    metrics_training_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r[["metrics_training"]]@metrics[["Overall"]])
    )))
    metrics_validation_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r[["metrics_validation"]]@metrics[["Overall"]])
    )))
  }
  metrics_validation_all[["param_combo_id"]] <- rep(
    seq_len(n_param_combinations),
    each = n_resamples
  )
  metrics_training_all[["param_combo_id"]] <- rep(
    seq_len(n_param_combinations),
    each = n_resamples
  )
  metrics_training_by_combo_id <- aggregate(
    . ~ param_combo_id,
    data = metrics_training_all,
    FUN = tuner_parameters[["metrics_aggregate_fn"]]
  )
  metrics_validation_by_combo_id <- aggregate(
    . ~ param_combo_id,
    data = metrics_validation_all,
    FUN = tuner_parameters[["metrics_aggregate_fn"]]
  )

  tune_results <- list(
    param_grid = param_grid,
    metrics_training = metrics_training_by_combo_id,
    metrics_validation = metrics_validation_by_combo_id
  )

  # Algorithm-specific collection ----
  # N of iterations is the one hyperparameter that may be determined
  # automatically, we therefore need to extract it and average it
  ## GLMNET ----
  if (algorithm == "GLMNET") {
    if (is.null(grid_params[["lambda"]])) {
      # if lambda was NULL, cv.glmnet was run and optimal lambda was estimated
      # For each i in grid_run, get grid_run[[i]]$hyperparameters[[grid_run[[i]]$hyperparameters$which.cv.lambda]]
      if (verbosity > 1L) {
        info("Extracting best lambda from GLMNET models...")
      }
      lambda_cv2 <- data.frame(
        lambda = sapply(
          grid_run,
          \(x)
            x[["hyperparameters"]][[x[["hyperparameters"]][[
              "which.cv.lambda"
            ]]]]
        )
      )
      lambda_cv2[["param_combo_id"]] <- rep(
        1:n_param_combinations,
        each = n_resamples
      )
      lambda_by_param_combo_id <- aggregate(
        lambda ~ param_combo_id,
        lambda_cv2,
        tuner_parameters[["metrics_aggregate_fn"]]
      )
      # Replace NULL lambda in tune_results$param_grid with average value of CV-squared lambda
      stopifnot(tune_results[["param_grid"]][["lambda"]] == "null")
      param_grid[["lambda"]] <- tune_results[["param_grid"]][[
        "lambda"
      ]] <- lambda_by_param_combo_id[["lambda"]]
    }
  } # /GLMNET

  ## LightGBM ----
  if (algorithm == "LightGBM") {
    if (is.null(grid_params[["nrounds"]])) {
      if (verbosity > 1L) {
        info("Extracting best N of iterations from LightGBM models...")
      }
      nrounds_cv <- data.frame(
        nrounds = sapply(grid_run, \(x) x[["hyperparameters"]][["best_iter"]])
      )
      nrounds_cv[["param_combo_id"]] <- rep(
        seq_len(n_param_combinations),
        each = n_resamples
      )
      nrounds_by_param_combo_id <- aggregate(
        nrounds ~ param_combo_id,
        nrounds_cv,
        tuner_parameters[["metrics_aggregate_fn"]]
      )
      # Replace NULL nrounds in tune_results$param_grid with average value of CV nrounds
      stopifnot(tune_results[["param_grid"]][["nrounds"]] == "null")
      param_grid[["nrounds"]] <- tune_results[["param_grid"]][["nrounds"]] <-
        as.integer(round(nrounds_by_param_combo_id[["nrounds"]]))
    }
  } # /LightGBM

  ## GBM, H2OGBM ----
  # if (algorithm %in% c("H2OGBM", "GBM", "GBM3")) {
  #   est.n.trees.all <- data.frame(n.trees = plyr::laply(
  #     grid_run,
  #     function(x) x$est.n.trees
  #   ))
  #   est.n.trees.all$param_combo_id <- rep(seq_len(n_param_combinations), each = n_resamples)
  #   est.n.trees.by.param_combo_id <- aggregate(
  #     n.trees ~ param_combo_id, est.n.trees.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.trees = round(est.n.trees.by.param_combo_id$n.trees),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /GBM, H2OGBM

  ## XGBoost ----
  # if (algorithm == "XGBoost") {
  #   if (verbosity > 1L) {
  #     msg2(hilite("Extracting best N of iterations from XGBoost models..."))
  #   }
  #   est.nrounds.all <- data.frame(nrounds = plyr::laply(
  #     grid_run,
  #     \(m) m$best_iteration
  #   ))
  #   est.nrounds.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.nrounds.by.param_combo_id <- aggregate(
  #     nrounds ~ param_combo_id, est.nrounds.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     nrounds = round(est.nrounds.by.param_combo_id$nrounds),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } /XGBoost

  ## LINAD ----
  # if (algorithm %in% c("LINAD", "LINOA")) {
  #   if (verbosity > 1L) {
  #     info("Extracting best N leaves from LINAD models...")
  #   }
  #   est.n.leaves.all <- data.frame(n.leaves = plyr::laply(
  #     grid_run,
  #     \(x) ifelse(length(x$est.n.leaves) == 0, 1, x$est.n.leaves)
  #   ))
  #   est.n.leaves.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.n.leaves.by.param_combo_id <- aggregate(
  #     n.leaves ~ param_combo_id, est.n.leaves.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.leaves =
  #       round(est.n.leaves.by.param_combo_id$n.leaves), tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /LINAD, LINOA

  ## LIHADBoost ----
  # if (algorithm == "LIHADBoost") {
  #   if (verbosity > 1L) {
  #     msg2(hilite("Extracting best N steps from LIHADBoost models..."))
  #   }
  #   est.n.steps.all <- data.frame(n.steps = plyr::laply(
  #     grid_run,
  #     \(x) x$sel.n.steps
  #   ))
  #   est.n.steps.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.n.steps.by.param_combo_id <- aggregate(
  #     n.steps ~ param_combo_id, est.n.steps.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.steps = round(est.n.steps.by.param_combo_id$n.steps),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /LIHADBoost

  # Consider explicitly sorting hyperparam values in increasing order,
  # so that in case of tie, lowest value is chosen -
  # if that makes sense, e.g. n.leaves, etc.
  best_param_combo_id <- as.integer(
    tune_results[["metrics_validation"]][
      select_fn(tune_results[["metrics_validation"]][[metric]]),
      1
    ]
  )
  best_param_combo <- as.list(param_grid[best_param_combo_id, -1, drop = FALSE])
  if (verbosity > 0L) {
    cat("\n")
    msg2(hilite(paste0("Best parameters to ", paste(verb, metric), ":")))
    print_tune_finding(grid_params, best_param_combo)
  }

  # Outro ----
  # Since this is always called from within `train()`, we don't want to print "Completed..."
  outro(start_time, verbosity = verbosity - 1)

  # => add optional mods field to GridSearch
  # if (save_mods) mods <- grid_run

  GridSearch(
    hyperparameters = hyperparameters,
    tuner_parameters = tuner_parameters,
    tuning_results = list(
      param_grid = param_grid,
      training = metrics_training_by_combo_id,
      validation = metrics_validation_by_combo_id
    ),
    best_hyperparameters = best_param_combo
  )
} # /rtemis::tune_GridSearch


# Print tuning results ----
# Print set of search values and best value in the form {1, 3, 5} => 3
# for each hyperparameter that was tuned.
print_tune_finding <- function(grid_params, best_param_combo) {
  # Make list of search values and best value
  tfl <- lapply(seq_along(grid_params), function(i) {
    paste0(
      "{",
      paste(grid_params[[i]], collapse = ", "),
      "}",
      " => ",
      bold(best_param_combo[[names(grid_params)[i]]])
    )
  })
  names(tfl) <- names(grid_params)
  printls(tfl, print_class = FALSE)
} # /rtemis::print_tune_finding
