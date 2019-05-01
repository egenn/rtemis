# [ rtModLog ] ====
#' \pkg{rtemis} Supervised Model Log Class
#'
#' @docType class
#' @name rtModLog-class
#' @field mod.name Learner algorithm name
#' @field parameters List of hyperparameters used when building model
#' @field error.train Training error
#' @field error.test Testing error
#' @field sessionInfo The output of \code{sessionInfo()} at the time the model was trained
#' @author Efstathios D. Gennatas
#' @export

rtModLog <- R6::R6Class("rtModLog",
                        public = list(
                          ### Attributes
                          mod.name = NULL,
                          parameters = NULL,
                          error.train = NULL,
                          error.test = NULL,
                          sessionInfo = NULL,
                          ### Initialize
                          initialize = function(mod.name = character(),
                                                parameters = list(),
                                                error.train = list(),
                                                error.test = NULL) {
                            self$mod.name <-  mod.name
                            self$parameters <- parameters
                            self$error.train <- error.train
                            self$error.test <- error.test
                            self$sessionInfo <- sessionInfo()
                          },
                          ### Methods
                          print = function() {
                            "show / print method for rtModLog"
                            boxcat(".:rtemis Supervised Model Log", newline.pre = FALSE)
                            cat(self$mod.name, " (", modSelect(self$mod.name, desc = TRUE),
                                ")\n", sep = "")
                            boxcat("Training Error")
                            print(self$error.train)
                            if (length(self$error.test) > 0) {
                              boxcat("Testing Error")
                              print(self$error.test)
                            }
                          }
                        )) # /rtModLog

# [ rtModLogger ] ====
#' \pkg{rtemis} model logger
#'
#' R6 class to save trained models' parameters and performance. Keep your experiment results tidy in
#' one place, with an option to write out to a multi-sheet Excel file.
#'
#' @author Efstathios D. Gennatas
#' @export

rtModLogger <- R6::R6Class("rtModLogger",
                           public = list(
                             ### Attributes
                             mods = list(),
                             ### Initialize
                             initialize = function(mods = list()) {
                               self$mods <- mods
                             },
                             ### Methods
                             print = function() {
                               "show / print method for rtModLogger"
                               boxcat(".:rtemis Supervised Model Logger", newline.pre = FALSE)
                               n.mods <- length(self$mods)
                               cat("\n   Contents:",
                                   bold(
                                     switch(paste(n.mods),
                                            `0` = "no models yet",
                                            `1` = "1 model",
                                            paste(n.mods, "models"))),
                                   "\n\n")
                             },
                             add = function(mod, verbose = TRUE) {
                               "add model to logger"
                               id <- paste0(mod$mod.name, "_", length(self$mods) + 1)
                               self$mods[[id]] <- rtModLog$new(mod.name = mod$mod.name,
                                                               parameters = mod$parameters,
                                                               error.train = mod$error.train,
                                                               error.test = mod$error.test)
                               if (verbose) msg("Added 1 model to logger;", length(self$mods), "total")
                             },
                             summarize = function(class.metric = "Balanced Accuracy",
                                                reg.metric = "Rsq",
                                                surv.metric = "Coherence",
                                                decimal.places = 3,
                                                print.metric = FALSE) {
                               "Print model names and performance"
                               lst <- vector("list", length(self$mods))
                               metric <- vector("character", length(self$mods))
                               names(lst) <- names(metric) <- names(self$mods)
                               for (i in seq(self$mods)) {
                                 if (inherits(self$mods[[i]]$error.train, "classError")) {
                                   metric[i] <- match.arg(class.metric, colnames(self$mods[[i]]$error.train$Overall))
                                   lst[[i]] <- paste0(ddSci(self$mods[[i]]$error.train$Overall[[metric[i]]],
                                                       decimal.places = decimal.places),
                                                 if (print.metric) c(" (", metric[i], ")") else NULL)
                                 } else if (inherits(self$mods[[i]]$error.train, "regError")) {
                                   metric[i] <- match.arg(reg.metric, colnames(self$mods[[i]]$error.train))
                                   lst[[i]] <- paste0(ddSci(self$mods[[i]]$error.train[[metric[i]]],
                                                       decimal.places = decimal.places),
                                                 if (print.metric) c(" (", metric[i], ")") else NULL)
                                 } else {
                                   metric[i] <- match.arg(surv.metric, colnames(self$mods[[i]]$error.train))
                                   lst[[i]] <- paste0(ddSci(self$mods[[i]]$error.train[[metric[i]]],
                                                       decimal.places = decimal.places),
                                                 if (print.metric) c(" (", metric[i], ")") else NULL)
                                 }
                                 names(lst[[i]]) <- metric[i]
                               }
                               .metric <- length(unique(metric)) == 1
                               printls(lst,
                                       color = rtHighlight$bold,
                                       title = if (.metric) metric[1] else NULL,
                                       title.newline = FALSE, newline.pre = FALSE)
                               invisible(lst)
                             },
                             summary = function(class.metric = "Balanced Accuracy",
                                                reg.metric = "Rsq",
                                                surv.metric = "Coherence") {
                               "Get model performance"
                               metric <- vector("character", length(self$mods))
                               tbl <- matrix(NA, length(self$mods), 2)
                               colnames(tbl) <- c("Train", "Test")
                               rownames(tbl) <- names(self$mods)
                               for (i in seq(self$mods)) {
                                 if (inherits(self$mods[[i]]$error.train, "classError")) {
                                   metric[i] <- match.arg(class.metric, colnames(self$mods[[i]]$error.train$Overall))
                                   tbl[i, 1] <- self$mods[[i]]$error.train$Overall[[metric[i]]]
                                   tbl[i, 2] <- self$mods[[i]]$error.test$Overall[[metric[i]]]
                                 } else if (inherits(self$mods[[i]]$error.train, "regError")) {
                                   metric[i] <- match.arg(reg.metric, colnames(self$mods[[i]]$error.train))
                                   tbl[i, 1] <- self$mods[[i]]$error.train[[metric[i]]]
                                   tbl[i, 2] <- self$mods[[i]]$error.test[[metric[i]]]
                                 } else {
                                   metric[i] <- match.arg(surv.metric, colnames(self$mods[[i]]$error.train))
                                   tbl[i, 1] <- self$mods[[i]]$error.train[[metric[i]]]
                                   tbl[i, 2] <- self$mods[[i]]$error.test[[metric[i]]]
                                 }
                               }
                               if (length(unique(metric)) == 1) {
                                 .metric <- metric[1]
                                 colnames(tbl) <- paste0(colnames(tbl), " ", .metric)
                                 attr(tbl, "metric") <- .metric
                               } else {
                                 rownames(tbl) <- paste0(rownames(tbl), " ", metric)
                                 attr(tbl, "metric") <- metric
                               }
                               invisible(tbl)
                             },
                             tabulate = function(filename = NULL) {
                               "Write parameters and performance for each model to an .xlsx file"
                               paramsl <- lapply(self$mods, function(i) {
                                 out <- data.frame(ModelName = i$mod.name)
                                 params <- reduceList(i$parameters)
                                 if (!is.null(params)) {
                                   out <- cbind(out, params)
                                 }
                                 if (inherits(i$error.train, "classError")) {
                                   out <- cbind(out, Train = i$error.train$Overall)
                                 } else {
                                   out <- cbind(out, Train = i$error.train)
                                 }

                                 if (!is.null(i$error.test)) {
                                   if (inherits(i$error.test, "classError")) {
                                     out <- cbind(out, Train = i$error.test$Overall)
                                   } else {
                                     out <- cbind(out, Test = i$error.test)
                                   }
                                 }
                                 out })
                               if (!is.null(filename)) {
                                 openxlsx::write.xlsx(paramsl, filename)
                                 if (file.exists(filename)) rtOut("Saved file", filename)
                               }
                               invisible(paramsl)
                             },
                             plot = function(names = NULL,
                                             col = unlist(ucsfPalette), ...) {
                               "Plot barplot of models' performance"
                               tbl <- self$summary()
                               n.mods <- NROW(tbl)
                               n.cols <- if (all(is.na(tbl[, 2]))) 1 else 2
                               col <- rep(unlist(ucsfPalette)[seq(n.mods)], n.cols)
                               if (!is.null(names)) {
                                 legend <- names
                               } else {
                                 legend <- labelify(rownames(tbl))
                               }
                               .metric <- attr(tbl, "metric")
                               if (length(.metric) == 1) {
                                 ylab <- .metric
                               } else {
                                 ylab <- "Performance"
                               }
                               barplot(tbl[, seq(n.cols)],
                                       # names.arg = c,
                                       ylab = ylab,
                                       xpd = T,
                                       space = c(0, .33),
                                       border = NA,
                                       beside = TRUE,
                                       col = col)
                               mtext(legend, 3, adj = 1, font = 2,
                                     line = rev(seq(n.mods)) - 1, col = col[seq(n.mods)])
                             }
                           )) # /rtModLogger

#' Reduce List to one value per element
#'
# reduceList <- function(x, char.max = 50, return.df = TRUE) {
#
#   if (length(x) == 0) {
#     xl <- NULL
#   } else {
#     xl <- x
#     # Functions to characters
#     fn.index <- which(sapply(xl, is.function))
#     for (i in fn.index) xl[[i]] <- substr(paste(deparse(x$na.action), collapse = " ## "), 1, char.max)
#     vec.index <- which(sapply(xl, function(i) length(i) > 1))
#     for (i in vec.index) xl[[i]] <- paste(xl[[i]], collapse = ", ")
#     null.index <- which(sapply(xl, is.null))
#     for (i in null.index) xl[[i]] <- "NULL"
#     if (return.df) xl <- as.data.frame(xl)
#   }
#   xl
# }


reduceList <- function(x, char.max = 50, return.df = TRUE) {

  if (length(x) > 0) {
    xl <- x
    # Functions to characters
    fn.index <- which(sapply(xl, is.function))
    for (i in fn.index) xl[[i]] <- substr(paste(deparse(x$na.action), collapse = " ## "), 1, char.max)
    vec.index <- which(sapply(xl, function(i) length(i) > 1))
    for (i in vec.index) xl[[i]] <- paste(xl[[i]], collapse = ", ")
    null.index <- which(sapply(xl, is.null))
    for (i in null.index) xl[[i]] <- "NULL"
    if (return.df) xl <- as.data.frame(xl)
    return(xl)
  }

} # rtemis::reduceList
