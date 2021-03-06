% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFunctions.R
\name{as.numeric.factor}
\alias{as.numeric.factor}
\title{Convert factor with numeric labels to numeric}
\usage{
\method{as.numeric}{factor}(x)
}
\arguments{
\item{x}{A factor vector.}
}
\value{
A numeric vector.
}
\description{
Convert factor with numeric labels to numeric
}
\examples{
factor_vector <- factor(c("2", "4", "6", "3", "1"))
numeric_vector <- as.numeric.factor(factor_vector)
factor_vector
numeric_vector
}
\author{
Caspar J. van Lissa
}
