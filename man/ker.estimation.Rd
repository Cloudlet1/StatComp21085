% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ker.estimation.R
\name{ker.estimation}
\alias{ker.estimation}
\title{ker.estimation}
\usage{
ker.estimation(data, bw, kernel = "gaussian")
}
\arguments{
\item{data}{numeric vector, the data from which the estimate is to be computed}

\item{bw}{a single number, specifying the bandwidth. There is also a function to help the user to find the appropriate bandwidth}

\item{kernel}{a character string, naming the type of kernel function to use. This must match one of "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine", with default "gaussian"}
}
\value{
a function, the kernel estimate function, with one input which is the point x
}
\description{
compute kernel density estimates of a data set
}
\examples{
\dontrun{
library("gss")
data(buffalo)
bw <- ker.bandwidth(buffalo, "Silverman")
density <- ker.estimation(buffalo, bw)
}
}
