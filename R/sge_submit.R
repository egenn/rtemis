# sge_submit.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Submit expression to SGE grid
#'
#' @param expr R expression
#' @param obj_names Character vector: Names of objects to copy to cluster R session
#' @param packages Character vector: Names of packages to load in cluster R session
#' @param queue Characer: Name of SGE queue to submit to
#' @param n_threads Integer: Number of threads to request from scheduler
#' @param sge_out Character: Path to directory to write standard out message files
#' @param sge_error Character: Path to directory to write error message files
#' @param sge_env Character: Shell environment for script to be submitted to SGE
#' @param sge_opts Character: SGE options that will be written in shell script. Default = "#$ -cwd"
#' @param system_command Character: system command to be run by shell script before executing R code.
#' For example a command that export the R executable to use
#' @param h_rt Character: Max time to request. Default = "00:25:00", i.e. 25 minutes
#' @param mem_free Character: Amount of memory to request from the scheduler
#' @param temp_dir Character: Temporary directory that is accessible to all execution nodes.
#' Default = \code{file.path(getwd(), ".sge_tempdir")}
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @param trace Integer: If > 0 print diagnostic messages to console. Default = 1
#'
#' @author E.D. Gennatas
#' @export

sge_submit <- function(expr,
                       obj_names = NULL,
                       packages = NULL,
                       queue = NULL,
                       n_threads = 4,
                       sge_out = file.path(getwd(), "./sge_out"),
                       sge_error = sge_out,
                       sge_env = "#! /usr/bin/env bash",
                       sge_opts = "#$ -cwd",
                       R_command = NULL,
                       system_command = NULL,
                       h_rt = "00:25:00",
                       mem_free = NULL,
                       temp_dir = file.path(getwd(), ".sge_tempdir"),
                       verbose = TRUE,
                       trace = 1) {

  expr <- as.character(as.expression(substitute(expr)))

  # Create temp_dir ====
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
    stopifnot(dir.exists(temp_dir))
    if (trace > 0) msg("Created temp_dir", temp_dir)
  }

  # Save obj_names to temp ====
  if (!is.null(obj_names)) {
    .temp <- tempfile(pattern = "guava", tmpdir = temp_dir)
    do.call(save, list(list = obj_names,
                       file = .temp))
    if (trace > 0) {
      msg("Temp file set to", .temp)
      msg("Objects written to temp file:", paste(obj_names, collapse = ", "))
    }
  }

  # sge_out and sge_error ====
  if (!dir.exists(sge_out)) {
    dir.create(sge_out, recursive = TRUE)
    if (trace > 0) msg("Created sge_out", sge_out)
  }
  if (!dir.exists(sge_error)) {
    dir.create(sge_error, recursive = TRUE)
    if (trace > 0) msg("Created sge_error", sge_error)
  }
  if (trace > 0) {
    msg("sge_out set to:", sge_out)
    msg("sge_error set to:", sge_error)
  }

  # Write {.R file} to temp_dir ====
  Rfilepath <- tempfile(pattern = "Rsub", tmpdir = temp_dir)
  if (trace > 0) msg("Rfilepath set to", Rfilepath)

  ## init file ====
  cat("# rtemis sge_submit", date(), "\n", file = Rfilepath)

  ## R_command ====
  if (!is.null(R_command)) {
    cat(R_command, "\n", file = Rfilepath, append = TRUE)
  }

  ## Load packages ====
  if (!is.null(packages)) {
    cat(sapply(packages, function(p) paste0("library(", p, ")\n")),
        sep = "", file = Rfilepath, append = TRUE)
  }

  ## Diag ====
  cat("rtemis::msg('Running on', Sys.getenv('HOSTNAME'), 'as', Sys.getenv('USER'))",
      "\n", file = Rfilepath, append = TRUE)

  ## Load data ====
  if (!is.null(obj_names)) {
    cat("load('", .temp, "')\n", sep = "", file = Rfilepath, append = TRUE)
  }

  ## Expression ====
  cat(as.character(expr), "\n", file = Rfilepath, append = TRUE)
  stopifnot(file.exists(Rfilepath))

  # Write {.sh file} to temp_dir ====
  shfilepath <- tempfile(pattern = "SHsub", tmpdir = temp_dir)
  if (trace > 0) msg("shfile set to:", shfilepath)

  cat(sge_env, "\n", file = shfilepath)
  cat(sge_opts, "\n", file = shfilepath, append = TRUE)
  if (!is.null(system_command)) {
    cat(system_command, "\n", file = shfilepath, append = TRUE)
  }
  cat("Rscript", Rfilepath, "\n", file = shfilepath, append = TRUE)

  stopifnot(file.exists(shfilepath))

  # Submit .sh to grid ====
  qsub <- paste(
    "qsub -pe smp", n_threads,
    "-o", sge_out,
    "-e", sge_error)
  if (!is.null(queue)) {
    qsub <- paste(qsub, "-q", queue)
  }
  if (!is.null(h_rt)) {
    qsub <- paste(qsub, paste0("-l h_rt=", h_rt))
  }
  if (!is.null(mem_free)) {
    qsub <- paste(qsub, paste0("-l mem_free=", mem_free))
  }
  qsub <- paste(qsub, shfilepath)

  system(qsub)

}


#' SGE qstat
#'
#' Run SGE qstat
#'
#' alias for \code{system("qstat")}
#'
#' @export

qstat <- function() {
  system("qstat")
}
