% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniprot_get.R
\name{uniprot_get}
\alias{uniprot_get}
\title{Get protein sequence from UniProt}
\usage{
uniprot_get(
  accession = "Q9UMX9",
  baseURL = "https://rest.uniprot.org/uniprotkb",
  verbose = TRUE
)
}
\arguments{
\item{accession}{Character: UniProt Accession number - e.g. "Q9UMX9"}

\item{baseURL}{Character: UniProt rest API base URL.
Default = "https://rest.uniprot.org/uniprotkb"}

\item{verbose}{Logical: If TRUE, print messages to console}
}
\value{
List with two elements: Annotation & Sequence
}
\description{
Get protein sequence from UniProt
}
\examples{
\dontrun{
matp <- uniprot_get("Q9UMX9")
}
}
\author{
E.D. Gennatas
}
