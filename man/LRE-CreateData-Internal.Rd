% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LRE-CreateData-Internal.r
\name{LRE-CreateData-Internal}
\alias{LRE-CreateData-Internal}
\alias{AssignVal}
\alias{CreateCQDataset}
\alias{CreateLD}
\alias{CreateLoadVars}
\alias{CreateQregDataset}
\alias{CreateRegDate}
\alias{PlotCQ}
\title{Internal functions for \code{CreateData} function.}
\arguments{
\item{Q}{Flow data}

\item{Conc}{Concentration data}

\item{date.range}{Date range}

\item{Ytype}{Year type. One of water year ("WY" or "WY2") or financial year ("FY")}

\item{Qflush}{Quantile for "first flush"}

\item{Reg}{Parameters for regularisation (type, rainfall, date)}

\item{Qreg}{Regularised flow data}

\item{startdate}{Starting date}

\item{x}{vector of x-values}

\item{y}{vector of y-values}

\item{csQ}{Cumulative sum of flow}

\item{samp.unit}{sampling unit}

\item{Date}{Date vector}

\item{Y}{Year type}
}
\description{
Internal functions for curating the data
}
\keyword{internal}
