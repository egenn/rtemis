# ddbops.R
# ::rtemis::
# 2022 EDG lambdamd.org

#' Read CSV using DuckDB
#'
#' Lazy-read a CSV file, optionally filter rows, remove duplicates,
#' clean column names, convert character to factor, and collect.
#'
#' @param filename Character: file name; either full path or just the file name,
#' if \code{datadir} is also provided
#' @param datadir Character: Optional path if \code{filename} is not full path
#' @param sep Character: Field delimiter/separator
#' @param header Logical: If TRUE, first line will be read as column names
#' @param quotechar Character: Quote character
#' @param ignore_error Logical: If TRUE, ignore parsing errors (sometimes it's
#' either this or no data, so)
#' @param make_unique Logical: If TRUE, keep only unique rows
#' @param select_columns Character vector: Column names to select
#' @param filter_column Character: Name of column to filter on, e.g. "ID"
#' @param filter_vals Numeric or Character vector: Values in
#' \code{filter_column} to keep.
#' @param character2factor Logical: If TRUE, convert character columns to
#' factors
#' @param collect Logical: If TRUE, collect data and return structure class
#' as defined by \code{returnobj}
#' @param progress Logical: If TRUE, print progress (no indication this works)
#' @param returnobj Character: "data.frame" or "data.table" object class to
#' return. If "data.table", data.frame object returned from
#' \code{DBI::dbGetQuery} is passed to \code{data.table::setDT}; will add to
#' execution time if very large, but then that's when you need a data.table
#' @param clean_colnames Logical: If TRUE, clean colnames with
#' \link{clean_colnames}
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export
#' @examples \dontrun{
#' ir <- ddb_data("/Data/massive_dataset.csv",
#'     filter_column = "ID",
#'     filter_vals = 8001:9999
#' )
#' }
ddb_data <- function(filename,
                     datadir = NULL,
                     sep = ",",
                     header = TRUE,
                     quotechar = "",
                     ignore_errors = TRUE,
                     make_unique = TRUE,
                     select_columns = NULL,
                     filter_column = NULL,
                     filter_vals = NULL,
                     character2factor = FALSE,
                     collect = TRUE,
                     progress = TRUE,
                     returnobj = c("data.table", "data.frame"),
                     data.table.key = NULL,
                     clean_colnames = TRUE,
                     verbose = TRUE) {
    # Intro ----
    dependency_check("DBI", "duckdb")
    returnobj <- match.arg(returnobj)
    if (!is.null(data.table.key)) returnobj <- "data.table"
    path <- if (is.null(datadir)) {
        normalizePath(filename)
    } else {
        file.path(normalizePath(datadir), filename)
    }
    check_files(path, verbose = FALSE)
    out <- paste(
        bold(green("\u25B6")),
        ifelse(collect, "Reading", "Lazy-reading"),
        hilite(basename(path))
    )
    if (!is.null(filter_column)) {
        out <- paste(
            out, bold(green("\u29e8")),
            "filtering on", bold(filter_column)
        )
    }
    startTime <- intro(out, verbose = verbose)
    distinct <- ifelse(make_unique, "DISTINCT ", NULL)
    select <- if (!is.null(select_columns)) {
        ls2sel(select_columns)
    } else {
        "*"
    }
    # SQL ----
    sql <- paste0(
        "SELECT ",
        paste0(distinct, select),
        " FROM
        read_csv_auto('", path, "',
        sep='", sep, "', quote='", quotechar, "',
        header=", header, ", ignore_errors=", ignore_errors, ")"
    )

    sql <- if (!is.null(filter_column)) {
        vals <- if (is.numeric(filter_vals)) {
            paste0(filter_vals, collapse = ", ")
        } else {
            paste0("'", paste0(filter_vals, collapse = "', '"), "'")
        }
        paste(
            sql,
            "WHERE", filter_column, "in (", vals, ");"
        )
    } else {
        paste0(sql, ";")
    }

    # Collect ----
    if (collect) {
        conn <- DBI::dbConnect(duckdb::duckdb())
        on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
        if (progress) DBI::dbExecute(conn, "PRAGMA enable_progress_bar;")
        out <- DBI::dbGetQuery(conn, sql)
        if (clean_colnames) {
            names(out) <- clean_colnames(out)
        }
        if (returnobj == "data.table") {
            data.table::setDT(out)
            if (!is.null(data.table.key)) {
                data.table::setkeyv(out, data.table.key)
            }
        }
        if (character2factor) {
            out <- preprocess(out, character2factor = TRUE)
        }
    } else {
        out <- sql
    }

    # Outro ----
    outro(startTime, verbose = verbose)
    out
} # rtemis::ddb_data


# output: '"alpha", "beta", "gamma"'
ls2sel <- function(x) {
    paste0(
        '"', paste0(x, collapse = '", "'), '"'
    )
}


#' Collect a lazy-read duckdb table
#' 
#' Collect a table read with \code{ddb_data(x, collect = FALSE)}
#' 
#' @param sql Character: DuckDB SQL query, usually output of 
#' \link{ddb_data(x, collect = FALSE)}
#' @param progress Logical: If TRUE, show progress bar
#' @param returnobj Character: data.frame or data.table: class of object to return
#' 
#' @author E.D. Gennatas
#' @export
#' @examples 
#' \dontrun{
#' sql <- ddb_data("/Data/iris.csv", collect = FALSE)
#' ir <- ddb_ollect(sql)
#' }
ddb_collect <- function(sql,
                        progress = TRUE,
                        returnobj = c("data.frame", "data.table")) {
    returnobj <- match.arg(returnobj)
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
    if (progress) DBI::dbExecute(conn, "PRAGMA enable_progress_bar;")
    out <- DBI::dbGetQuery(conn, sql)
    if (returnobj == "data.table") {
        setDT(out)
    }
    out
} # rtemis::ddb_collect
