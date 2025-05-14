# dt_ops.R
# ::rtemis::
# 2022-3 EDG rtemis.org

#' Number of unique values per feature
#'
#' @param x data.table
#' @param excludeNA Logical: If TRUE, exclude NA values
#'
#' @author E.D. Gennatas
#' @export

nunique_perfeat <- function(x, excludeNA = FALSE) {
  sapply(x, \(i) data.table::uniqueN(i, na.rm = excludeNA))
} # rtemis::nunique_perfeat


#' Long to wide key-value reshaping
#'
#' Reshape a long format `data.table` using key-value pairs with
#' `data.table::dcast`
#'
#' @param x A `data.table` object
#' @param id_name Character: Name of column in `x` that defines the IDs
#' identifying individual rows
#' @param key_name Character: Name of column in `x` that holds the key
#' @param positive Numeric or Character: Used to fill id ~ key combination
#' present in the long format input `x`
#' @param negative Numeric or Character: Used to fill id ~ key combination
#' NOT present in the long format input `x`
#' @param xname Character: Name of `x` to be used in messages
#' @param verbose Logical: If TRUE, print messages to the console
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- data.table(
#'   ID = rep(1:3, each = 2),
#'   Dx = c("A", "C", "B", "C", "D", "A")
#' )
#' dt_keybin_reshape(x, id_name = "ID", key_name = "Dx")
#' }
dt_keybin_reshape <- function(
  x,
  id_name,
  key_name,
  positive = 1,
  negative = 0,
  xname = NULL,
  verbose = TRUE
) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  stopifnot(inherits(x, "data.table"))
  x <- copy(x)

  # Assign positive value to all in long form
  value_name <- "Bin__"
  x[, (value_name) := positive]

  .formula <- as.formula(paste(
    paste(id_name, collapse = " + "),
    "~",
    key_name
  ))
  if (verbose) {
    msg2("Reshaping", hilite(xname), "to wide format...")
    catsize(x, "Input size")
  }
  # Reshape to wide, filling all absent with negative value
  x <- dcast(
    x,
    .formula,
    fun.aggregate = length,
    value.var = value_name,
    drop = FALSE,
    fill = negative
  )

  if (verbose) catsize(x, "Output size")
  x
} # rtemis::dt_keybin_reshape


#' Merge data.tables
#'
#' @param left data.table
#' @param right data.table
#' @param on Character: Name of column to join on
#' @param left_on Character: Name of column on left table
#' @param right_on Character: Name of column on right table
#' @param how Character: Type of join: "inner", "left", "right", "outer".
#' @param left_name Character: Name of left table
#' @param right_name Character: Name of right table
#' @param left_suffix Character: If provided, add this suffix to all left column names,
#' excluding on/left_on
#' @param right_suffix Character: If provided, add this suffix to all right column names,
#' excluding on/right_on
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional arguments to be passed to `data.table::merge`
#'
#' @author E.D. Gennatas
#' @export

dt_merge <- function(
  left,
  right,
  on = NULL,
  left_on = NULL,
  right_on = NULL,
  how = "left",
  left_name = NULL,
  right_name = NULL,
  left_suffix = NULL,
  right_suffix = NULL,
  verbose = TRUE,
  ...
) {
  if (is.null(left_name)) left_name <- deparse(substitute(left))
  if (is.null(right_name)) right_name <- deparse(substitute(right))
  if (is.null(left_on)) left_on <- on
  if (is.null(right_on)) right_on <- on
  if (verbose) {
    icon <- switch(
      how,
      inner = "\u2A1D",
      left = "\u27D5",
      right = "\u27D6",
      "\u27D7"
    )
    if (left_on == right_on) {
      msg20(
        bold(green(icon)),
        " Merging ",
        hilite(left_name),
        " & ",
        hilite(right_name),
        " on ",
        hilite(left_on),
        "..."
      )
    } else {
      msg20(
        bold(green(icon)),
        " Merging ",
        hilite(left_name),
        " & ",
        hilite(right_name),
        " on ",
        hilite(left_on),
        " & ",
        hilite(right_on),
        "..."
      )
    }

    catsize(left, left_name)
    catsize(right, right_name)
  }

  if (how == "left") {
    all.x <- TRUE
    all.y <- FALSE
  } else if (how == "right") {
    all.x <- FALSE
    all.y <- TRUE
  } else if (how == "inner") {
    all.x <- FALSE
    all.y <- FALSE
  } else {
    all.x <- all.y <- TRUE
  }
  if (!is.null(left_suffix)) {
    left_names <- setdiff(names(left), left_on)
    setnames(left, left_names, paste0(left_names, left_suffix))
  }
  if (!is.null(right_suffix)) {
    right_names <- setdiff(names(right), right_on)
    setnames(right, right_names, paste0(right_names, right_suffix))
  }
  dat <- merge(
    left,
    right,
    by.x = left_on,
    by.y = right_on,
    all.x = all.x,
    all.y = all.y,
    ...
  )
  if (verbose) {
    catsize(dat, "Merged")
  }
  dat
} # rtemis::dt_merge


#' Clean factor levels of data.table in-place
#'
#' Finds all factors in a data.table and cleans factor levels to include
#' only underscore symbols
#'
#' @param x data.table
#' @param prefix_digits Character: If not NA, add this prefix to all factor levels that
#' are numbers
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- as.data.table(iris)
#' levels(x$Species) <- c("setosa:iris", "versicolor$iris", "virginica iris")
#' dt_set_cleanfactorlevels(x)
#' x
#' }
dt_set_cleanfactorlevels <- function(x, prefix_digits = NA) {
  stopifnot(inherits(x, "data.table"))
  idi <- names(x)[sapply(x, is.factor)]
  for (i in idi) {
    x[,
      (i) := factor(
        x[[i]],
        labels = clean_names(levels(x[[i]]), prefix_digits = prefix_digits)
      )
    ]
  }
} # rtemis::dt_set_cleanfactorlevels


#' Check if all levels in a column are unique
#'
#' @param x data.frame or data.table
#' @param on Integer or character: column to check
#'
#' @author E.D. Gennatas
#' @export
dt_check_unique <- function(x, on) {
  length(unique(x[[on]])) == NROW(x)
}


#' Get index of duplicate values
#'
#' @param x data.frame or data.table
#' @param on Integer or character: column to check
#'
#' @author E.D. Gennatas
#' @export
dt_get_duplicates <- function(x, on) {
  # appease R CMD check
  ..on <- NULL
  x[x[[on]] %in% x[[on]][duplicated(x[[on]])], ..on][[1]]
}


#' Index columns by attribute name & value
#'
#' @param x data.frame or compatible
#' @param name Character: Name of attribute
#' @param value Character: Value of attribute
#'
#' @author E.D. Gennatas
#' @export
dt_index_attr <- function(x, name, value) {
  colattr <- unlist(sapply(x, \(i) attr(i, name)))
  which(colattr == value)
}


#' Get N and percent match of values between two columns of two data.tables
#'
#' @param x data.table
#' @param y data.table
#' @param on Integer or character: column to read in `x` and `y`, if it is the
#' same
#' @param left_on Integer or character: column to read in `x`
#' @param right_on Integer or character: column to read in `y`
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export
dt_pctmatch <- function(
  x,
  y,
  on = NULL,
  left_on = NULL,
  right_on = NULL,
  verbose = TRUE
) {
  if (is.null(left_on)) left_on <- on
  if (is.null(right_on)) right_on <- on
  xv <- unique(x[[left_on]])
  n <- length(xv)
  yv <- unique(y[[right_on]])
  nmatch <- sum(xv %in% yv)
  matchpct <- nmatch / n * 100
  if (verbose) {
    by_final <- paste(unique(c(left_on, right_on)), collapse = ", ")
    msg20(
      "Matched ",
      hilite(nmatch),
      "/",
      hilite(n),
      " on ",
      bold(by_final),
      " (",
      hilite(ddSci(matchpct)),
      "%)"
    )
  }
  invisible(list(nmatch = nmatch, matchpct = matchpct))
}


#' Get percent of missing values from every column
#'
#' @param x data.frame or data.table
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export
dt_pctmissing <- function(x, verbose = TRUE) {
  nmissing <- sapply(x, \(i) sum(is.na(i)))
  pctmissing <- nmissing / NROW(x)
  if (verbose) {
    cat("Percent missing per column:\n")
    printls(pctmissing)
  }
  invisible(list(nmissing = nmissing, pctmissing = pctmissing))
}


#' Convert data.table logical columns to factor with custom labels in-place
#'
#' @param x data.table
#' @param cols Integer or character: columns to convert, if NULL, operates on all
#' logical columns
#' @param labels Character: labels for factor levels
#' @param maintain_attributes Logical: If TRUE, maintain column attributes
#' @param fillNA Character: If not NULL, fill NA values with this constant
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' x <- data.table(a = 1:5, b = c(T, F, F, F, T))
#' x
#' dt_set_logical2factor(x)
#' x
#' z <- data.table(alpha = 1:5, beta = c(T, F, T, NA, T), gamma = c(F, F, T, F, NA))
#' z
#' # You can usee fillNA to fill NA values with a constant
#' dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"), fillNA = "No")
#' z
#' w <- data.table(mango = 1:5, banana = c(F, F, T, T, F))
#' w
#' dt_set_logical2factor(w, cols = 2, labels = c("Ugh", "Huh"))
#' w
#' # Column attributes are maintained by default:
#' z <- data.table(alpha = 1:5, beta = c(T, F, T, NA, T), gamma = c(F, F, T, F, NA))
#' for (i in seq_along(z)) setattr(z[[i]], "source", "Guava")
#' str(z)
#' dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"))
#' str(z)
#' }
dt_set_logical2factor <- function(
  x,
  cols = NULL,
  labels = c("False", "True"),
  maintain_attributes = TRUE,
  fillNA = NULL
) {
  if (is.null(cols)) cols <- names(x)[sapply(x, is.logical)]
  for (i in cols) {
    if (maintain_attributes) .attr <- attributes(x[[i]])
    x[, (i) := factor(x[[i]], levels = c(FALSE, TRUE), labels = labels)]
    if (!is.null(fillNA)) x[is.na(x[[i]]), (i) := fillNA]
    if (maintain_attributes) {
      for (j in seq_along(.attr)) setattr(x[[i]], names(.attr)[j], .attr[[j]])
    }
  }
  invisible(x)
}


#' Tabulate column attributes
#'
#' @param x data.table
#' @param attr Character: Attribute to get
#' @param useNA Character: Passed to `table`
#'
#' @author E.D. Gennatas
#' @export
dt_get_column_attr <- function(x, attr = "source", useNA = "always") {
  attrs <- sapply(x, \(i) {
    if (is.null(attr(i, attr, exact = TRUE))) NA_character_ else
      attr(i, attr, exact = TRUE)
  })
  table(attrs, useNA = useNA)
}

# dt_missing_by_class <- function(x) {

# }

# All data can be represented as a character string. A numeric variable may be read as
# a character variable if there are non-numeric characters in the data.
# It is important to be able to automatically detect such variables and convert them,
# which would mean introducing NA values.

#' Inspect character and factor vector
#'
#' Checks character or factor vector to determine whether it might be best to convert to
#' numeric.
#'
#' @param x Character or factor vector.
#' @param xname Character: Name of input vector `x`.
#' @param verbose Logical: If TRUE, print messages to console.
#' @param thresh Numeric: Threshold for determining whether to convert to numeric.
#' @param na.omit Logical: If TRUE, remove NA values before checking.
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- c("3", "5", "undefined", "21", "4", NA)
#' inspect_type(x)
#' z <- c("mango", "banana", "tangerine", NA)
#' inspect_type(z)
#' }
inspect_type <- function(
  x,
  xname = NULL,
  verbose = TRUE,
  thresh = .5,
  na.omit = TRUE
) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  if (na.omit) x <- na.omit(x)
  xclass <- class(x)[1]
  xlen <- length(x)
  raw_na <- sum(is.na(x))
  n_non_na <- xlen - raw_na
  # char_na <- sum(is.na(as.character(x)))
  suppressWarnings({
    num_na <- if (xclass == "character") {
      sum(is.na(as.numeric(x)))
    } else {
      sum(is.na(as.numeric(as.character(x))))
    }
  })
  if (raw_na == xlen) {
    "NA"
  } else if (
    xclass %in% c("character", "factor") && (num_na / n_non_na) < thresh
  ) {
    if (verbose) {
      msg20(
        "Possible type error: ",
        hilite(xname),
        " is a ",
        bold(xclass),
        ", but perhaps should be ",
        bold("numeric"),
        "."
      )
    }
    "numeric"
  } else {
    xclass
  }
}


#' Inspect column types
#'
#' Will attempt to identify columns that should be numeric but are either character or
#' factor by running [inspect_type] on each column.
#'
#' @param x data.table
#' @param cols Character vector: columns to inspect.
#' @param verbose Logical: If TRUE, print messages to console.
#'
#' @author E.D. Gennatas
#' @export
dt_inspect_type <- function(x, cols = NULL, verbose = TRUE) {
  if (is.null(cols)) {
    char_factor_idi <- which(sapply(x, is.character) | sapply(x, is.factor))
    cols <- names(x[, .SD, .SDcols = char_factor_idi])
  }
  current_types <- sapply(x[, .SD, .SDcols = cols], class)
  suggested_types <- sapply(
    cols,
    \(cn) inspect_type(x[[cn]], xname = cn, verbose = verbose)
  )
  to_convert <- suggested_types != current_types
  names(to_convert)[to_convert]
}


#' Set column types automatically
#'
#' This function inspects a data.table and attempts to identify columns that should be
#' numeric but have been read in as character, because one or more fields contain
#' non-numeric characters
#'
#' @param x data.table
#' @param cols Character vector: columns to work on. If not defined, will work on all
#' columns
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export
dt_set_autotypes <- function(x, cols = NULL, verbose = TRUE) {
  if (is.null(cols)) cols <- names(x)
  for (i in cols) {
    if (inspect_type(x[[i]], i, verbose = FALSE) == "numeric") {
      if (verbose) msg2("Converting", hilite(i), "to", bold("numeric"))
      x[, (i) := as.numeric(x[[i]])]
    }
  }
  invisible(x)
}


#' List column names by class
#'
#' @param x data.table
#' @param sorted Logical: If TRUE, sort the output
#' @param item.format Function: Function to format each item
#' @param maxlength Integer: Maximum number of items to print
#'
#' @author E.D. Gennatas
#' @export
dt_names_by_class <- function(
  x,
  sorted = TRUE,
  item.format = hilite,
  maxlength = 24
) {
  classes <- sapply(x, class)
  vals <- unique(classes)
  out <- if (sorted) {
    sapply(vals, \(i) sort(names(x)[classes == i]))
  } else {
    sapply(vals, \(i) names(x)[classes == i])
  }
  printls(out, item.format = item.format, maxlength = maxlength)
}


#' List column names by attribute
#'
#' @param x data.table
#' @param which Character: name of attribute
#' @param exact Logical: If TRUE, use exact matching
#' @param sorted Logical: If TRUE, sort the output
#'
#' @author E.D. Gennatas
#' @export
dt_names_by_attr <- function(x, which, exact = TRUE, sorted = TRUE) {
  attrs <- unlist(lapply(x, \(i) attr(i, which)))
  attrs <- sapply(x, \(i) {
    .attr <- attr(i, which, exact = exact)
    if (is.null(.attr)) "NA" else .attr
  })
  vals <- unique(attrs)
  if (sorted) {
    sapply(vals, \(i) sort(names(x)[attrs == i]))
  } else {
    sapply(vals, \(i) names(x)[attrs == i])
  }
}


#' Clean column names and factor levels in-place
#'
#' @param x data.table
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip
#'
#' Note: If `x` is not a data.table, it will be converted in place, i.e. the original
#' object will be modified.
#'
#' @author E.D. Gennatas
#' @export
dt_set_clean_all <- function(x, prefix_digits = NA) {
  if (!is.data.table(x)) {
    setDT(x)
  }
  data.table::setnames(x, names(x), clean_colnames(x))
  idi <- names(x)[sapply(x, is.factor)]
  for (i in idi) {
    x[,
      (i) := factor(
        x[[i]],
        labels = clean_names(levels(x[[i]]), prefix_digits = prefix_digits)
      )
    ]
  }
} # rtemis::dt_set_clean_all


#' Get factor levels from data.table
#'
#' @param dat data.table
#'
#' Note: If `dat` is not a data.table, it will be converted in place, i.e. the original
#' object will be modified.
#'
#' @returns Named list of factor levels. Names correspond to column names.
#' @export
dt_get_factor_levels <- function(dat) {
  if (!is.data.table(dat)) {
    dat <- as.data.table(dat)
  }
  factor_index <- which(sapply(dat, is.factor))
  lapply(
    dat[, factor_index, with = FALSE],
    levels
  )
}
