% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read}
\alias{read}
\title{Read tabular data from a variety of formats}
\usage{
read(
  filename,
  datadir = NULL,
  make.unique = TRUE,
  character2factor = FALSE,
  clean.colnames = TRUE,
  delim.reader = c("data.table", "vroom", "duckdb", "arrow"),
  xlsx.sheet = 1,
  sep = NULL,
  quote = "\\"",
  na.strings = c(""),
  output = c("data.table", "default"),
  attr = NULL,
  value = NULL,
  verbose = TRUE,
  fread_verbose = FALSE,
  timed = verbose,
  ...
)
}
\arguments{
\item{filename}{Character: filename or full path if \code{datadir = NULL}}

\item{datadir}{Character: Optional path to directory where \code{filename}
is located. If not specified, \code{filename} must be the full path.}

\item{make.unique}{Logical: If TRUE, keep unique rows only}

\item{character2factor}{Logical: If TRUE, convert character variables to
factors}

\item{clean.colnames}{Logical: If TRUE, clean columns names using
\link{clean_colnames}}

\item{delim.reader}{Character: package to use for reading delimited data}

\item{xlsx.sheet}{Integer or character: Name or number of XLSX sheet to read}

\item{sep}{Single character: field separator. If \code{delim.reader = "fread"}
and \code{sep = NULL}, this defaults to "auto", otherwise defaults to ","}

\item{quote}{Single character: quote character}

\item{na.strings}{Character vector: Strings to be interpreted as NA values.
For \code{delim.reader = "duckdb"}, this must be a single string.}

\item{output}{Character: "default" or "data.table", If default, return the delim.reader's
default data structure, otherwise convert to data.table}

\item{attr}{Character: Attribute to set (Optional)}

\item{value}{Character: Value to set (if \code{attr} is not NULL)}

\item{verbose}{Logical: If TRUE, print messages to console}

\item{fread_verbose}{Logical: Passed to \code{data.table::fread}}

\item{timed}{Logical: If TRUE, time the process and print to console}

\item{...}{Additional parameters to pass to \code{data.table::fread},
\code{arrow::read_delim_arrow()}, \code{vroom::vroom()},
or \code{openxlsx::read.xlsx()}}
}
\description{
Read data and optionally clean column names, keep unique rows, and convert
characters to factors
}
\details{
\code{read} is a convenience function to read:
\itemize{
\item \strong{Delimited} files using \code{data.table:fread()}, \code{arrow:read_delim_arrow()},
\code{vroom::vroom()}, \code{duckdb::duckdb_read_csv()}
\item \strong{ARFF} files using \code{farff::readARFF()}
\item \strong{Parquet} files using \code{arrow::read_parquet()}
\item \strong{XLSX} files using \code{readxl::read_excel()}
\item \strong{DTA} files from Stata using \code{haven::read_dta()}
\item \strong{FASTA} files using \code{seqinr::read.fasta()}
\item \strong{RDS} files using \code{readRDS()}
}
}
\examples{
\dontrun{
datadir <- "~/icloud/Data"
dat <- read("iris.csv", datadir)
}
}
\author{
E.D. Gennatas
}
