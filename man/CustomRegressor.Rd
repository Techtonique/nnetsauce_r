% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom.R
\name{CustomRegressor}
\alias{CustomRegressor}
\title{Custom regressor with quasi-randomized layer}
\usage{
CustomRegressor(
  obj,
  n_hidden_features = 5L,
  activation_name = "relu",
  a = 0.01,
  nodes_sim = "sobol",
  bias = TRUE,
  dropout = 0,
  direct_link = TRUE,
  n_clusters = 2L,
  cluster_encode = TRUE,
  type_clust = "kmeans",
  col_sample = 1,
  row_sample = 1,
  seed = 123L,
  backend = c("cpu", "gpu", "tpu")
)
}
\description{
Parameters' description can be found at \url{https://techtonique.github.io/nnetsauce/}
}
\examples{

set.seed(123)
n <- 50 ; p <- 3
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)

(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]

obj <- sklearn$linear_model$ElasticNet()
obj2 <- CustomRegressor(obj)
obj2$fit(X_train, y_train)
print(obj2$score(X_test, y_test))

}
