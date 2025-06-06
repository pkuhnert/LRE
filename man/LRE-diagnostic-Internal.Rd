% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LRE-diagnostic-Internal.r
\name{LRE-diagnostic-Internal}
\alias{LRE-diagnostic-Internal}
\alias{gg_qq}
\title{Functions to examine diagnostics of fitted model}
\arguments{
\item{x}{residuals}

\item{distribution}{distribution (Default: "norm")}

\item{line.estimate}{line estimate (Default: NULL)}

\item{conf}{confidence limits (Default: 0.95)}

\item{labels}{labels (Default: names(x))}
}
\description{
Produces a ggplot version of a Q-Q plot for the residuals
}
\details{
Taken from GitHub rentrop/gg_qq.r (Parts of code copied from car:::qqPlot)
}
\keyword{internal}
