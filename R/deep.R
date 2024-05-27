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
                    start_input = NULL,
                    frequency_input = NULL,
                    n_layers = 3L,
                    n_hidden_features=5L,
                    activation_name="relu",
                    a=0.01,
                    nodes_sim="sobol",
                    bias=TRUE,
                    dropout=0,
                    direct_link=TRUE,
                    n_clusters=2L,
                    cluster_encode=TRUE,
                    type_clust="kmeans",
                    lags=1L,
                    replications=NULL,
                    kernel=NULL,
                    agg="mean",
                    seed=123L,
                    backend=c("cpu", "gpu", "tpu"),
                    verbose=0,
                    ...)
{
  out <- ns$DeepMTS(obj,
    n_layers = n_layers,
    n_hidden_features=n_hidden_features,
    activation_name=activation_name,
    a=a,
    nodes_sim=nodes_sim,
    bias=bias,
    dropout=dropout,
    direct_link=direct_link,
    n_clusters=n_clusters,
    cluster_encode=cluster_encode,
    type_clust=type_clust,
    lags=lags,
    replications=replications,
    kernel=kernel,
    agg=agg,
    seed=seed,
    backend=backend,
    verbose=verbose,
    ...
  )

  res <- list()
  res$model <- out
  res$method <- "DeepMTS"

  if (is.null(start_input))
  {
    res$mean <- ts(as.matrix(out$mean))
    res$lower <- ts(as.matrix(out$lower))
    res$upper <- ts(as.matrix(out$upper))
    res$level <- out$level
    res$x <- ts(as.matrix(out$df_))
    if (n_series <= 1L)
    {
      class(res) <- "forecast"
    } else {
      class(res) <- "mforecast"
    }
    return(res)

  } else {

    # returns a time series object
    stopifnot(!is.null(frequency_input))
    end_x <- calculate_end(start_input,
                           frequency_input,
                           nrow(out$df_))
    start_preds <- calculate_next(end_x,
                                  frequency_input)
    res$mean <- ts(as.matrix(out$mean),
                   start=start_preds,
                   frequency=frequency_input)
    res$lower <- ts(as.matrix(out$lower),
                    start=start_preds,
                    frequency=frequency_input)
    res$upper <- ts(as.matrix(out$upper),
                    start=start_preds,
                    frequency=frequency_input)
    res$level <- out$level
    res$x <- ts(as.matrix(out$df_),
                start=start_input,
                frequency=frequency_input)
    res$residuals <- NULL
    res$fitted <- NULL
    if (n_series <= 1L)
    {
      class(res) <- "forecast"
    } else {
      class(res) <- "mforecast"
    }
  }

  return(res)
}
