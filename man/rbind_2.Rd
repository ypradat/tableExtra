% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtable_utils.R
\name{rbind_2}
\alias{rbind_2}
\title{rbind two gtables}
\usage{
rbind_2(x, y, size = "max", height = NULL)
}
\arguments{
\item{x}{a \code{gtable} object}

\item{y}{a \code{gtable} object}

\item{size}{how should the widths be calculated?
\enumerate{
\item \code{max} maximum of all widths
\item \code{min} minimum of all widths
\item \code{first} widths/heights of first gtable
\item \code{last} widths/heights of last gtable
}}

\item{height}{padding height between grobs}
}
\value{
a \code{gtable} object with containing row-binded gtables \code{x} and \code{y}
}
\description{
rbind two gtables
}
\keyword{internal}
