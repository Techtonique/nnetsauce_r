



# 1 - Classifiers ---------------------------------------------------------

#' Automated Machine Learning for classification models
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param classifiers specify classifiers to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomClassifier}}
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(datasets)
#'
#' set.seed(123)
#' X <- as.matrix(iris[, 1:4])
#' y <- as.integer(iris$Species) - 1L
#'
#' (index_train <- base::sample.int(n = nrow(X),
#'                                  size = floor(0.8*nrow(X)),
#'                                  replace = FALSE))
#' X_train <- X[index_train, ]
#' y_train <- y[index_train]
#' X_test <- X[-index_train, ]
#' y_test <- y[-index_train]
#'
#' obj <- LazyClassifier()
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
LazyClassifier <- function(verbose = 0,
                           ignore_warnings = TRUE,
                           custom_metric = NULL,
                           predictions = FALSE,
                           random_state = 42L,
                           classifiers = "all",
                           preprocess = FALSE,
                           ...)
{
  ns$LazyClassifier(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    classifiers = classifiers,
    preprocess = preprocess,
    ...
  )
}

# 2 - Regressors ---------------------------------------------------------

#' Automated Machine Learning for regression models
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param regressors specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return
#' @export
#'
#' @examples
#'
#' X <- MASS::Boston[,-14] # dataset has an ethical problem
#' y <- MASS::Boston$medv
#'
#' set.seed(13)
#' (index_train <- base::sample.int(n = nrow(X),
#'                                  size = floor(0.8*nrow(X)),
#'                                  replace = FALSE))
#' X_train <- X[index_train, ]
#' y_train <- y[index_train]
#' X_test <- X[-index_train, ]
#' y_test <- y[-index_train]
#'
#' obj <- LazyRegressor()
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
#'
LazyRegressor <- function(verbose = 0,
                          ignore_warnings = TRUE,
                          custom_metric = NULL,
                          predictions = FALSE,
                          random_state = 42L,
                          regressors = "all",
                          preprocess = FALSE,
                          ...)
{
  ns$LazyRegressor(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    regressors = regressors,
    preprocess = preprocess,
    ...
  )
}

# 3 - Time series ---------------------------------------------------------

#' Automated Machine Learning for time series models
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param regressors specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' X <- matrix(rnorm(300), 100, 3)
#'
#' (index_train <- base::sample.int(n = nrow(X),
#'                                  size = floor(0.8*nrow(X)),
#'                                  replace = FALSE))
#' X_train <- data.frame(X[index_train, ])
#' X_test <- data.frame(X[-index_train, ])
#'
#' obj <- LazyMTS()
#'
#' res <- obj$fit(X_train, X_test)
#' print(res[[1]])
#'
LazyMTS <- function(verbose = 0,
                    ignore_warnings = TRUE,
                    custom_metric = NULL,
                    predictions = FALSE,
                    random_state = 42L,
                    regressors = "all",
                    preprocess = FALSE,
                    show_progress=TRUE,
                    ...)
{
  ns$LazyMTS(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    regressors = regressors,
    preprocess = preprocess,
    show_progress = show_progress,
    ...
  )
}
