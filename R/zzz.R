# global reference to scipy (will be initialized in .onLoad)
ns <- NULL
sklearn <- NULL

.onLoad <- function(libname, pkgname) {
  utils::install.packages("reticulate",
                          repos = list(CRAN = "https://cloud.r-project.org"))

  reticulate::py_require("scikit-learn")
  reticulate::py_require("nnetsauce")

  try(reticulate::py_install(
    "scikit-learn",
    pip = TRUE,
    pip_options = "--upgrade",
    pip_ignore_installed = TRUE
  ), silent = TRUE)
  try(reticulate::py_install(
    "nnetsauce",
    pip = TRUE,
    pip_options = "--upgrade",
    pip_ignore_installed = TRUE
  ), silent = TRUE)

  # use superassignment to update global reference to package
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
}
