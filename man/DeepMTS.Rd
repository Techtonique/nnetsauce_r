% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deep.R
\name{DeepMTS}
\alias{DeepMTS}
\title{Deep MTS models}
\usage{
DeepMTS(obj, n_layers = 3L, ...)
}
\arguments{
\item{obj}{a model object}

\item{n_layers}{number of hidden layers}

\item{...}{additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}}
}
\description{
See also https://techtonique.github.io/nnetsauce/
}
\examples{

set.seed(123)
X <- matrix(rnorm(300), 100, 3)

obj <- sklearn$linear_model$ElasticNet()
obj2 <- DeepMTS(obj)

obj2$fit(X)
obj2$predict()


}
