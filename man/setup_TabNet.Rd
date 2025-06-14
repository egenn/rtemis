% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_S7_Hyperparameters.R
\name{setup_TabNet}
\alias{setup_TabNet}
\title{Setup TabNet Hyperparameters}
\usage{
setup_TabNet(
  batch_size = 1024^2,
  penalty = 0.001,
  clip_value = NULL,
  loss = "auto",
  epochs = 50L,
  drop_last = FALSE,
  decision_width = NULL,
  attention_width = NULL,
  num_steps = 3L,
  feature_reusage = 1.3,
  mask_type = "sparsemax",
  virtual_batch_size = 256^2,
  valid_split = 0,
  learn_rate = 0.02,
  optimizer = "adam",
  lr_scheduler = NULL,
  lr_decay = 0.1,
  step_size = 30,
  checkpoint_epochs = 10L,
  cat_emb_dim = 1L,
  num_independent = 2L,
  num_shared = 2L,
  num_independent_decoder = 1L,
  num_shared_decoder = 1L,
  momentum = 0.02,
  pretraining_ratio = 0.5,
  device = "auto",
  importance_sample_size = NULL,
  early_stopping_monitor = "auto",
  early_stopping_tolerance = 0,
  early_stopping_patience = 0,
  num_workers = 0L,
  skip_importance = FALSE,
  ifw = FALSE
)
}
\arguments{
\item{batch_size}{(Tunable) Positive integer: Batch size.}

\item{penalty}{(Tunable) Numeric: Regularization penalty.}

\item{clip_value}{Numeric: Clip value.}

\item{loss}{Character: Loss function.}

\item{epochs}{(Tunable) Positive integer: Number of epochs.}

\item{drop_last}{Logical: If TRUE, drop last batch.}

\item{decision_width}{(Tunable) Positive integer: Decision width.}

\item{attention_width}{(Tunable) Positive integer: Attention width.}

\item{num_steps}{(Tunable) Positive integer: Number of steps.}

\item{feature_reusage}{(Tunable) Numeric: Feature reusage.}

\item{mask_type}{Character: Mask type.}

\item{virtual_batch_size}{(Tunable) Positive integer: Virtual batch size.}

\item{valid_split}{Numeric: Validation split.}

\item{learn_rate}{(Tunable) Numeric: Learning rate.}

\item{optimizer}{Character or torch function: Optimizer.}

\item{lr_scheduler}{Character or torch function: "step", "reduce_on_plateau".}

\item{lr_decay}{Numeric: Learning rate decay.}

\item{step_size}{Positive integer: Step size.}

\item{checkpoint_epochs}{(Tunable) Positive integer: Checkpoint epochs.}

\item{cat_emb_dim}{(Tunable) Positive integer: Categorical embedding dimension.}

\item{num_independent}{(Tunable) Positive integer: Number of independent Gated Linear Units (GLU)
at each step of the encoder.}

\item{num_shared}{(Tunable) Positive integer: Number of shared Gated Linear Units (GLU) at each
step of the encoder.}

\item{num_independent_decoder}{(Tunable) Positive integer: Number of independent GLU layers for
pretraining.}

\item{num_shared_decoder}{(Tunable) Positive integer: Number of shared GLU layers for
pretraining.}

\item{momentum}{(Tunable) Numeric: Momentum.}

\item{pretraining_ratio}{(Tunable) Numeric: Pretraining ratio.}

\item{device}{Character: Device "cpu" or "cuda".}

\item{importance_sample_size}{Positive integer: Importance sample size.}

\item{early_stopping_monitor}{Character: Early stopping monitor. "valid_loss", "train_loss",
"auto".}

\item{early_stopping_tolerance}{Numeric: Minimum relative improvement to reset the patience
counter.}

\item{early_stopping_patience}{Positive integer: Number of epochs without improving before
stopping.}

\item{num_workers}{Positive integer: Number of subprocesses for data loacding.}

\item{skip_importance}{Logical: If TRUE, skip importance calculation.}

\item{ifw}{Logical: If TRUE, use Inverse Frequency Weighting in classification.}
}
\value{
TabNetHyperparameters object.
}
\description{
Setup hyperparameters for TabNet training.
}
\author{
EDG
}
