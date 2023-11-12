





# 1 - Classifiers ---------------------------------------------------------

#' Automated Machine Learning for deep classification models
#'
#' @param n_layers number of hidden layers
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param classifiers specify classifiers to be adjusted (default is 'all')
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
#' obj <- LazyDeepClassifier(n_layers = 3L)
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
LazyDeepClassifier <- function(n_layers = 3L,
                               verbose = 0,
                               ignore_warnings = TRUE,
                               custom_metric = NULL,
                               predictions = FALSE,
                               random_state = 42L,
                               classifiers = "all",
                               ...)
{
  ns$LazyDeepClassifier(
    n_layers = n_layers,
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    classifiers = classifiers,
    ...
  )
}

# 2 - Regressors ---------------------------------------------------------

#' Automated Machine Learning for deep regression models
#'
#' @param n_layers number of hidden layers
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param regressors specify regressors to be adjusted (default is 'all')
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
#' obj <- LazyDeepRegressor(n_layers = 3L, n_clusters=2L)
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
#'
LazyDeepRegressor <- function(n_layers = 3L,
                              verbose = 0,
                              ignore_warnings = TRUE,
                              custom_metric = NULL,
                              predictions = FALSE,
                              random_state = 42L,
                              regressors = "all",
                              ...)
{
  ns$LazyDeepRegressor(
    n_layers = n_layers,
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    regressors = regressors,
    ...
  )
}
