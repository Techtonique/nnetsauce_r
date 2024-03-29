% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazy.R
\name{LazyDeepMTS}
\alias{LazyDeepMTS}
\title{Automated Machine Learning for deep time series models}
\usage{
LazyDeepMTS(
  verbose = 0,
  ignore_warnings = TRUE,
  custom_metric = NULL,
  predictions = FALSE,
  random_state = 42L,
  estimators = "all",
  preprocess = FALSE,
  show_progress = TRUE,
  n_layers = 3L,
  ...
)
}
\arguments{
\item{verbose}{monitor progress (\code{0}, default, is false and \code{1} is true)}

\item{ignore_warnings}{print trace when model fitting failed}

\item{custom_metric}{defining a custom metric (default is \code{NULL})}

\item{predictions}{obtain predictions (default is \code{FALSE})}

\item{random_state}{reproducibility seed}

\item{estimators}{specify regressors to be adjusted (default is 'all')}

\item{preprocess}{preprocessing input covariates (default is FALSE \code{FALSE})}

\item{n_layers}{number of layers for the deep model}

\item{...}{additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}}
}
\value{
a list that you can \code{$fit}
}
\description{
See also https://techtonique.github.io/nnetsauce/
}
\examples{

set.seed(123)
X <- matrix(rnorm(300), 100, 3)

(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- data.frame(X[index_train, ])
X_test <- data.frame(X[-index_train, ])

obj <- LazyDeepMTS()

res <- obj$fit(X_train, X_test)
print(res[[1]])

}
