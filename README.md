# nnetsauce

**Randomized and quasi-randomized nnetworks for supervised learning and multivariate time series forecasting**

[![nnetsauce status badge](https://techtonique.r-universe.dev/badges/nnetsauce)](https://techtonique.r-universe.dev/nnetsauce)

[![CodeFactor](https://www.codefactor.io/repository/github/techtonique/nnetsauce_r/badge)](https://www.codefactor.io/repository/github/techtonique/nnetsauce_r)

[![HitCount](https://hits.dwyl.com/Techtonique/nnetsauce_r.svg?style=flat-square)](http://hits.dwyl.com/Techtonique/nnetsauce_r)

This is the `R` version of Python's [Techtonique/nnetsauce](https://github.com/Techtonique/nnetsauce). See [these posts](https://thierrymoudiki.github.io/blog/#QuasiRandomizedNN) for more details.

If you encounter errors on Windows, envisage using the [Windows Subsystems for Linux](https://learn.microsoft.com/en-us/windows/wsl/install).

## Install 

**From GitHub**

```bash
remotes::install_github("Techtonique/nnetsauce_r")
```

**From R-universe**

```bash
install.packages('nnetsauce', repos = c('https://techtonique.r-universe.dev',
'https://cloud.r-project.org'))
```

## Examples 

Keep in mind that there are many other models implemented. See [these posts](https://thierrymoudiki.github.io/blog/#QuasiRandomizedNN).

### Classification

```R
library(datasets)
#'
set.seed(123)
X <- as.matrix(iris[, 1:4])
y <- as.integer(iris$Species) - 1L
#'
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
#'
obj <- LazyClassifier()
res <- obj$fit(X_train, X_test, y_train, y_test)
print(res[[1]])
```

### Regression 

```R
X <- MASS::Boston[,-14] # dataset has an ethical problem
y <- MASS::Boston$medv

set.seed(13)
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]

obj <- LazyRegressor()
res <- obj$fit(X_train, X_test, y_train, y_test)
print(res[[1]])
```

### Time series 

```R
set.seed(123)
X <- matrix(rnorm(300), 100, 3)

(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- data.frame(X[index_train, ])
X_test <- data.frame(X[-index_train, ])

obj <- LazyMTS()

res <- obj$fit(X_train, X_test)
print(res[[1]])
```
