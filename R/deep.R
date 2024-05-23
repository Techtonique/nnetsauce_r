# 1 - Classifiers ---------------------------------------------------------

#' Deep classification models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param obj a model object
#' @param n_layers number of hidden layers
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
#' obj2 <- sklearn$linear_model$LogisticRegressionCV()
#'
#' obj <- DeepClassifier(obj2, n_layers = 3L)
#' res <- obj$fit(X_train, y_train)
#'
#' # accuracy
#' print(mean(obj$predict(X_test)==y_test))
#'
DeepClassifier <- function(obj,
                           n_layers = 3L,
                           ...)
{
  ns$DeepClassifier(obj,
    n_layers = n_layers,
    ...
  )
}

# 2 - Regressors ---------------------------------------------------------

#' Deep regression models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param obj a model object
#' @param n_layers number of hidden layers
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
#' obj2 <- sklearn$linear_model$ElasticNet()
#'
#' obj <- DeepRegressor(obj2, n_layers = 3L, n_clusters=2L)
#' res <- obj$fit(X_train, y_train)
#' print(obj$predict(X_test))
#'
#'
DeepRegressor <- function(obj,
                          n_layers = 3L,
                          ...)
{
  ns$DeepRegressor(obj,
    n_layers = n_layers,
    ...
  )
}


# 3 - Multivariate Time Series (MTS) ---------------------------------------------------------

#' Deep MTS models
#'
#' See also https://techtonique.github.io/nnetsauce/
#'
#' @param obj a model object
#' @param n_layers number of hidden layers
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
#' obj <- sklearn$linear_model$ElasticNet()
#' obj2 <- DeepMTS(obj)
#'
#' obj2$fit(X)
#' obj2$predict()
#'
#'
DeepMTS <- function(obj,
                    n_layers = 3L,
                    ...)
{
  ns$DeepMTS(obj,
    n_layers = n_layers,
    ...
  )
}
