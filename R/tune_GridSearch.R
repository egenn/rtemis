# tune_GridSearch.R
# ::rtemis::
# 2025 EDG rtemis.org

#' \pkg{rtemis} internal: Grid Search for Hyperparameter Tuning of \pkg{rtemis} Learners
#'
#' Train models using a combination of parameter values for model selection
#'
#' Note that weights, if defined (and not NULL), should be passed directly to `grid_search`
#' as they need to be resampled along `x` and `y`, and should not be passed along with
#' `grid_params`. `ifw` and `ifw.type` should be passed as part of `grid_params`
#' and will be passed on to the learner.
#' Includes a special case for training [s_H2OGBM] or [s_GBM] which requires extracting and averaging n.trees
#' along with params.
#'
#' @param dat_training data.frame or similar: Training set
#' @param hyperparameters List: Hyperparameters.
#' @param tuner_parameters List: Tuner parameters.
#' @param weights Vector: Class weights.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @keywords internal
#' @noRd

tune_GridSearch <- function(dat_training,
                            hyperparameters,
                            tuner_parameters,
                            weights = NULL,
                            save_mods = FALSE,
                            verbosity = 1) {
  # Dependencies ----
  check_dependencies(c("future", "future.apply"))

  # Intro ----
  start_time <- intro(
    message = "Running grid search...",
    call_depth = 2,
    newline_pre = TRUE,
    verbosity = verbosity
  )

  # Arguments ----
  algorithm <- hyperparameters@algorithm

  # future plan ----
  n_workers <- check_plan_for_learner(
    algorithm = hyperparameters@algorithm,
    plan = tuner_parameters$future_plan,
    n_workers = tuner_parameters$n_workers
  )
  if (!is.null(tuner_parameters$future_plan)) {
    # => Only allow parallelization on same machine, if learner is NOT parallelized itself.
    og_plan <- plan()
    on.exit(plan(og_plan), add = TRUE)
    # => Only pass n_workers if arg workers is supported by plan
    future::plan(tuner_parameters$future_plan, workers = tuner_parameters$n_workers)
    if (verbosity > 0) {
      msg2(hilite2(
        "Tuning crossvalidation (inner resampling) future plan set to",
        underline(tuner_parameters$future_plan), "with", underline(n_workers), "workers."
      ))
    }
  }
  # rtemis_init(n_cores, context = "Inner resampling")

  # Arguments ----
  # if (verbosity > 1) {
  #   msg2(hilite2(
  #     "Tuning crossvalidation (inner resampling) future plan set to:"
  #   ))
  #   print(future::plan())
  # }

  # Make Grid ----
  grid_params <- get_params_need_tuning(hyperparameters)
  n_params <- length(grid_params)
  n_resamples <- tuner_parameters$resample_params$n
  search_type <- tuner_parameters$search_type
  param_grid <- expand.grid(grid_params, stringsAsFactors = FALSE)
  param_grid <- cbind(param_combo_id = seq_len(NROW(param_grid)), param_grid)
  n_param_combinations <- NROW(param_grid)
  res_param_grid <- expand.grid(
    c(list(resample_id = seq_len(n_resamples)), grid_params),
    stringsAsFactors = FALSE
  )
  n_res_x_comb <- NROW(res_param_grid)
  if (search_type == "randomized") {
    index_per_resample <- sample(
      n_param_combinations,
      round(tuner_parameters$randomize_p * n_param_combinations)
    )
    res_param_grid <- res_param_grid[rep(index_per_resample, n_resamples), ]
  }

  # Resamples ----
  res <- resample(
    dat_training,
    parameters = tuner_parameters$resample_params,
    verbosity = verbosity
  )

  # learner1 ----
  p <- progressr::progressor(steps = NROW(res_param_grid))
  learner1 <- function(index,
                       res,
                       res_param_grid,
                       hyperparameters,
                       weights,
                       verbosity,
                       save_mods,
                       n_res_x_comb) {
    if (verbosity > 0) {
      msg2("Running grid line #", hilite(index), "/",
        NROW(res_param_grid), "...",
        sep = ""
      )
    }
    res1 <- res[[res_param_grid[index, "resample_id"]]]
    dat_train1 <- dat_training[res1, ]
    weights1 <- weights[res1]
    dat_valid1 <- dat_training[-res1, ]
    hyperparams1 <- hyperparameters
    hyperparams1 <- update(
      hyperparams1,
      as.list(res_param_grid[index, 2:NCOL(res_param_grid), drop = FALSE]),
      tuned = -9L # Hyperparameters are being tuned
    )

    mod1 <- do.call(
      "train",
      args = list(
        dat_training = dat_train1,
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
    if (algorithm == "H2OGBM") {
      out1$est.n.trees <-
        mod1$mod@model$model_summary$number_of_trees
    }
    if (algorithm == "s_GBM" || algorithm == "s_GBM3") {
      out1$est.n.trees <- which.min(mod1$mod$valid.error)
      if (length(out1$est.n.trees) == 0) out1$est.n.trees <- NA
    }
    if (algorithm == "s_LightGBM") {
      out1$best_iter <- mod1$mod$best_iter
      out1$best_score <- mod1$mod$best_score
    }
    if (algorithm == "s_XGBoost") {
      out1$best_iteration <- mod1$mod$best_iteration
      out1$best_score <- mod1$mod$best_score
    }
    if (algorithm == "s_GLMNET") {
      out1$lambda.min <- mod1$mod$lambda.min
      out1$lambda.1se <- mod1$mod$lambda.1se
    }
    if (algorithm %in% c("s_LINAD", "s_LINOA")) {
      out1$est.n.leaves <- mod1$mod$n.leaves
    }
    if (algorithm == "s_LIHADBoost") {
      out1$sel.n.steps <- mod1$mod$selected.n.steps
    }
    if (save_mods) out1$mod1 <- mod1
    p(sprintf("Inner resample: %i/%i...", index, n_res_x_comb))
    out1
  } # /learner1

  # Train Grid ----
  if (verbosity > 0) {
    msg2(
      hilite(
        "Tuning", hyperparameters@algorithm, "by",
        search_type, "grid search."
      )
    )
    msg20(
      hilite(n_param_combinations), " parameter combinations x ",
      hilite(n_resamples), " resamples: ",
      hilite(n_res_x_comb), " models total running on ",
      singorplu(n_workers, "worker"),
      " (", Sys.getenv("R_PLATFORM"), ")."
    )
  }

  grid_run <- future.apply::future_lapply(
    X = seq_len(n_res_x_comb),
    FUN = learner1,
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

  # Metric ----
  type <- supervised_type(dat_training)
  metric <- tuner_parameters@parameters$metric
  maximize <- tuner_parameters@parameters$maximize
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced_Accuracy"
    } else if (type == "Regression") {
      metric <- "MSE"
    } else {
      metric <- "Concordance"
    }
    tuner_parameters@parameters$metric <- metric
  }
  if (is.null(maximize)) {
    maximize <- metric %in% c("Accuracy", "Balanced_Accuracy", "Concordance", "Rsq", "r")
    tuner_parameters@parameters$maximize <- maximize
  }
  select_fn <- if (maximize) which.max else which.min
  verb <- if (maximize) "maximize" else "minimize"

  # Aggregate ----
  # Average test errors
  if (type %in% c("Regression", "Survival")) {
    metrics_training_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$metrics_training@metrics)
    )))
    metrics_validation_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$metrics_validation@metrics)
    )))
  } else if (type == "Classification") {
    metrics_training_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$metrics_training@metrics$Overall)
    )))
    metrics_validation_all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$metrics_validation@metrics$Overall)
    )))
  }
  metrics_validation_all$param_combo_id <- rep(seq_len(n_param_combinations), each = n_resamples)
  metrics_training_all$param_combo_id <- rep(seq_len(n_param_combinations), each = n_resamples)
  metrics_training_by_combo_id <- aggregate(
    . ~ param_combo_id,
    data = metrics_training_all, FUN = tuner_parameters$metrics_aggregate_fn
  )
  metrics_validation_by_combo_id <- aggregate(
    . ~ param_combo_id,
    data = metrics_validation_all, FUN = tuner_parameters$metrics_aggregate_fn
  )

  tune_results <- list(
    param_grid = param_grid,
    metrics_training = metrics_training_by_combo_id,
    metrics_validation = metrics_validation_by_combo_id
  )

  # Algorithm-specific collection ----
  # N of iterations is the one hyperparameter that may be determined
  # automatically, we therefore need to extract it and average it

  ## GBM, H2OGBM ----
  if (algorithm %in% c("s_H2OGBM", "s_GBM", "s_GBM3")) {
    est.n.trees.all <- data.frame(n.trees = plyr::laply(
      grid_run,
      function(x) x$est.n.trees
    ))
    est.n.trees.all$param_combo_id <- rep(seq_len(n_param_combinations), each = n_resamples)
    est.n.trees.by.param_combo_id <- aggregate(
      n.trees ~ param_combo_id, est.n.trees.all,
      metrics_aggregate_fn
    )
    tune_results <- cbind(
      n.trees = round(est.n.trees.by.param_combo_id$n.trees),
      tune_results
    )
    n_params <- n_params + 1
  }

  ## LightGBM ----
  if (algorithm == "s_LightGBM") {
    if (verbosity > 1) {
      msg2(hilite("Extracting best N of iterations from LightGBM models..."))
    }
    est.nrounds.all <- data.frame(
      nrounds = plyr::laply(grid_run, \(m) m$best_iter)
    )
    est.nrounds.all$param_combo_id <- rep(seq_len(n_param_combinations),
      each = n_resamples
    )
    est.nrounds.by.param_combo_id <- aggregate(
      nrounds ~ param_combo_id, est.nrounds.all,
      metrics_aggregate_fn
    )
    tune_results <- cbind(
      nrounds = round(est.nrounds.by.param_combo_id$nrounds),
      tune_results
    )
    n_params <- n_params + 1
  }

  ## XGBoost ----
  if (algorithm == "s_XGBoost") {
    if (verbosity > 1) {
      msg2(hilite("Extracting best N of iterations from XGBoost models..."))
    }
    est.nrounds.all <- data.frame(nrounds = plyr::laply(
      grid_run,
      \(m) m$best_iteration
    ))
    est.nrounds.all$param_combo_id <- rep(seq_len(n_param_combinations),
      each = n_resamples
    )
    est.nrounds.by.param_combo_id <- aggregate(
      nrounds ~ param_combo_id, est.nrounds.all,
      metrics_aggregate_fn
    )
    tune_results <- cbind(
      nrounds = round(est.nrounds.by.param_combo_id$nrounds),
      tune_results
    )
    n_params <- n_params + 1
  }

  ## GLMNET ----
  if (algorithm == "s_GLMNET") {
    if (verbosity > 1) {
      msg2(hilite("Extracting best lambda from GLMNET models..."))
    }
    if (is.null(grid_params$lambda)) {
      # if lambda was NULL, cv.glmnet was run and optimal lambda was estimated
      lambda.all <- data.frame(lambda = plyr::laply(grid_run, \(x) x[[hyperparameters$which.cv.lambda]]))
      lambda.all$param_combo_id <- rep(1:n_param_combinations, each = n_resamples)
      lambda.by.param_combo_id <- aggregate(
        lambda ~ param_combo_id, lambda.all,
        metrics_aggregate_fn
      )
      tune_results <- cbind(lambda = lambda.by.param_combo_id$lambda, tune_results)
      n_params <- n_params + 1
    }
  }

  ## LINAD ----
  if (algorithm %in% c("s_LINAD", "s_LINOA")) {
    if (verbosity > 1) {
      msg2(hilite("Extracting best N leaves from LINAD models..."))
    }
    est.n.leaves.all <- data.frame(n.leaves = plyr::laply(
      grid_run,
      \(x) ifelse(length(x$est.n.leaves) == 0, 1, x$est.n.leaves)
    ))
    est.n.leaves.all$param_combo_id <- rep(seq_len(n_param_combinations),
      each = n_resamples
    )
    est.n.leaves.by.param_combo_id <- aggregate(
      n.leaves ~ param_combo_id, est.n.leaves.all,
      metrics_aggregate_fn
    )
    tune_results <- cbind(
      n.leaves =
        round(est.n.leaves.by.param_combo_id$n.leaves), tune_results
    )
    n_params <- n_params + 1
  }

  ## LIHADBoost ----
  if (algorithm == "s_LIHADBoost") {
    if (verbosity > 1) {
      msg2(hilite("Extracting best N steps from LIHADBoost models..."))
    }
    est.n.steps.all <- data.frame(n.steps = plyr::laply(
      grid_run,
      \(x) x$sel.n.steps
    ))
    est.n.steps.all$param_combo_id <- rep(seq_len(n_param_combinations),
      each = n_resamples
    )
    est.n.steps.by.param_combo_id <- aggregate(
      n.steps ~ param_combo_id, est.n.steps.all,
      metrics_aggregate_fn
    )
    tune_results <- cbind(
      n.steps = round(est.n.steps.by.param_combo_id$n.steps),
      tune_results
    )
    n_params <- n_params + 1
  }

  # Consider explicitly sorting hyperparam values in increasing order,
  # so that in case of tie, lowest value is chosen -
  # if that makes sense, e.g. n.leaves, etc.
  best_param_combo_id <- as.integer(
    tune_results$metrics_validation[select_fn(tune_results$metrics_validation[[metric]]), 1]
  )
  best_param_combo <- as.list(param_grid[best_param_combo_id, -1])
  if (verbosity > 0) {
    msg2(hilite(paste0("Best parameters to ", paste(verb, metric), ":")))
    printls(best_param_combo)
  }

  # Outro ----
  outro(start_time, verbosity = verbosity)

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
} # rtemis::grid_search
