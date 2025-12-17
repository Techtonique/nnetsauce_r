
get_ns <- function(venv_path = "./venv") {
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  reticulate::import("nnetsauce", 
                     delay_load = TRUE)
}
get_ns <- memoise::memoize(get_ns)

get_sklearn <- function(venv_path = "./venv") {
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  reticulate::import("sklearn", 
                      delay_load = TRUE)
}
get_sklearn <- memoise::memoize(get_sklearn)

