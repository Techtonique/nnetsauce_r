# nnetsauce

**Randomized and quasi-randomized nnetworks for supervised learning and multivariate time series forecasting**

[![Documentation](https://img.shields.io/badge/documentation-is_here-green)](https://techtonique.github.io/nnetsauce_r/)
![Downloads](https://r-packages.techtonique.net/badges/downloads/last-month/nnetsauce.svg)
![Total Downloads](https://r-packages.techtonique.net/badges/downloads/grand-total/nnetsauce.svg?color=brightgreen)


This is the `R` version of Python's [Techtonique/nnetsauce](https://github.com/Techtonique/nnetsauce). See [these posts](https://thierrymoudiki.github.io/blog/#QuasiRandomizedNN) for more details.

If you encounter errors on Windows, envisage using the [Windows Subsystems for Linux](https://learn.microsoft.com/en-us/windows/wsl/install).

## Install 

```bash
# pip install uv # if necessary
uv venv venv
source venv/bin/activate
uv pip install pip nnetsauce
```

**From GitHub**

```bash
install.packages("remotes")
remotes::install_github("Techtonique/nnetsauce_r")
```

## Examples 

Keep in mind that there are many other models implemented. See [these posts](https://thierrymoudiki.github.io/blog/#QuasiRandomizedNN).

Read [vignettes](./vignettes)


### Classification

```R
library(nnetsauce)

 set.seed(123)
 X <- as.matrix(iris[, 1:4])
 y <- as.integer(iris$Species) - 1L

 (index_train <- base::sample.int(n = nrow(X),
                                  size = floor(0.8*nrow(X)),
                                  replace = FALSE))
 X_train <- X[index_train, ]
 y_train <- y[index_train]
 X_test <- X[-index_train, ]
 y_test <- y[-index_train]

 obj <- nnetsauce::GLMClassifier(venv_path = "./venv")
 obj$fit(X_train, y_train)
 print(obj$score(X_test, y_test))
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

obj <- LazyRegressor(venv_path = "./venv")
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

obj <- LazyMTS(venv_path = "./venv")

res <- obj$fit(X_train, X_test)
print(res[[1]])
```
