# 1 - Classifiers ---------------------------------------------------------

#' Automated Machine Learning for classification models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify classifiers to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomClassifier}}
#'
#' @return a list that you can \code{$fit}
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
                           estimators = "all",
                           preprocess = FALSE, 
                           venv_path = "./venv",
                           ...) {
  
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  ns <- reticulate::import("nnetsauce", 
                           delay_load = TRUE)
  
  ns$LazyClassifier(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    ...
  )
}

#' Automated Machine Learning for deep classification models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify classifiers to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param n_layers number of layers for the deep model
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomClassifier}}
#'
#' @return a list that you can \code{$fit}
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
#' obj <- LazyDeepClassifier()
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
LazyDeepClassifier <- function(verbose = 0,
                           ignore_warnings = TRUE,
                           custom_metric = NULL,
                           predictions = FALSE,
                           random_state = 42L,
                           estimators = "all",
                           preprocess = FALSE,
                           n_layers = 3L, 
                           venv_path = "./venv",
                           ...) {
  
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  ns <- reticulate::import("nnetsauce", 
                           delay_load = TRUE)
  
  ns$LazyDeepClassifier(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    n_layers = n_layers,
    ...
  )
}

# 2 - Regressors ---------------------------------------------------------

#' Automated Machine Learning for regression models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return a list that you can \code{$fit}
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
                          estimators = "all",
                          preprocess = FALSE, 
                          venv_path = "./venv",
                          ...) {
  
  # Lazy load sklearn only when needed
  ns <- get_ns(venv_path)
  
  ns$LazyRegressor(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    ...
  )
}

#' Automated Machine Learning for deep regression models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param n_layers number of layers for the deep model
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return a list that you can \code{$fit}
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
#' obj <- LazyDeepRegressor()
#' res <- obj$fit(X_train, X_test, y_train, y_test)
#' print(res[[1]])
#'
#'
LazyDeepRegressor <- function(verbose = 0,
                          ignore_warnings = TRUE,
                          custom_metric = NULL,
                          predictions = FALSE,
                          random_state = 42L,
                          estimators = "all",
                          preprocess = FALSE,
                          n_layers = 3L, 
                          venv_path = "./venv",
                          ...) {
  
  # Lazy load sklearn only when needed
  ns <- get_ns(venv_path)
  
  ns$LazyDeepRegressor(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    n_layers = n_layers,
    ...
  )
}

# 3 - Time series ---------------------------------------------------------

#' Automated Machine Learning for time series models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return a list that you can \code{$fit}
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
                    estimators = "all",
                    preprocess = FALSE,
                    show_progress=TRUE, 
                    venv_path = "./venv",
                    ...) {
  
  # Lazy load sklearn only when needed
  ns <- get_ns(venv_path)
  
  ns$LazyMTS(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    show_progress = show_progress,
    ...
  )
}

#' Automated Machine Learning for deep time series models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param verbose monitor progress (\code{0}, default, is false and \code{1} is true)
#' @param ignore_warnings print trace when model fitting failed
#' @param custom_metric defining a custom metric (default is \code{NULL})
#' @param predictions obtain predictions (default is \code{FALSE})
#' @param random_state reproducibility seed
#' @param estimators specify regressors to be adjusted (default is 'all')
#' @param preprocess preprocessing input covariates (default is FALSE \code{FALSE})
#' @param n_layers number of layers for the deep model
#' @param ... additional parameters to be passed to \code{\link{nnetsauce::CustomRegressor}}
#'
#' @return a list that you can \code{$fit}
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
#' obj <- LazyDeepMTS()
#'
#' res <- obj$fit(X_train, X_test)
#' print(res[[1]])
#'
LazyDeepMTS <- function(verbose = 0,
                    ignore_warnings = TRUE,
                    custom_metric = NULL,
                    predictions = FALSE,
                    random_state = 42L,
                    estimators = "all",
                    preprocess = FALSE,
                    show_progress=TRUE,
                    n_layers = 3L, 
                    venv_path = "./venv",
                    ...) {
  
  # Lazy load sklearn only when needed
  ns <- get_ns(venv_path)
  
  ns$LazyDeepMTS(
    verbose = verbose,
    ignore_warnings = ignore_warnings,
    custom_metric = custom_metric,
    predictions = predictions,
    random_state = random_state,
    estimators = estimators,
    preprocess = preprocess,
    show_progress = show_progress,
    n_layers = n_layers,
    ...
  )
}
