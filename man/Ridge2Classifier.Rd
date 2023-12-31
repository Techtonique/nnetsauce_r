% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge2.R
\name{Ridge2Classifier}
\alias{Ridge2Classifier}
\title{Multinomial logit, quasi-randomized classification model with 2 shrinkage parameters}
\usage{
Ridge2Classifier(
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
  lambda1 = 0.1,
  lambda2 = 0.1,
  seed = 123L,
  backend = c("cpu", "gpu", "tpu")
)
}
\description{
Parameters' description can be found at \url{https://techtonique.github.io/nnetsauce/}
}
\examples{

library(datasets)

X <- as.matrix(iris[, 1:4])
y <- as.integer(iris[, 5]) - 1L

(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]

obj <- Ridge2Classifier()
obj$fit(X_train, y_train)
print(obj$score(X_test, y_test))
print(obj$predict_proba(X_train))


}
