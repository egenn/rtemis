% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtModLog.R
\docType{class}
\name{rtModLog-class}
\alias{rtModLog-class}
\alias{rtModLog}
\title{\pkg{rtemis} Supervised Model Log Class}
\description{
\pkg{rtemis} Supervised Model Log Class

\pkg{rtemis} Supervised Model Log Class
}
\author{
E.D. Gennatas
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{mod.name}}{Learner algorithm name}

\item{\code{parameters}}{List of hyperparameters used when building model}

\item{\code{error.train}}{Training error}

\item{\code{error.test}}{Testing error}

\item{\code{sessionInfo}}{The output of \code{sessionInfo()} at the time the model was trained}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-rtModLog-new}{\code{rtModLog$new()}}
\item \href{#method-rtModLog-print}{\code{rtModLog$print()}}
\item \href{#method-rtModLog-clone}{\code{rtModLog$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-rtModLog-new"></a>}}
\if{latex}{\out{\hypertarget{method-rtModLog-new}{}}}
\subsection{Method \code{new()}}{
Initialize \code{rtModLog} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{rtModLog$new(
  mod.name = character(),
  parameters = list(),
  error.train = list(),
  error.test = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mod.name}}{Learner algorithm name}

\item{\code{parameters}}{List of hyperparameters used when building model}

\item{\code{error.train}}{Training error}

\item{\code{error.test}}{Testing error}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-rtModLog-print"></a>}}
\if{latex}{\out{\hypertarget{method-rtModLog-print}{}}}
\subsection{Method \code{print()}}{
Print method for \code{rtModLog} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{rtModLog$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-rtModLog-clone"></a>}}
\if{latex}{\out{\hypertarget{method-rtModLog-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{rtModLog$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
