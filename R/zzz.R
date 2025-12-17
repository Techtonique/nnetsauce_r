.onLoad <- function(libname, pkgname) {
  venv_path <- Sys.getenv("YOURPACKAGE_VENV", "venv")
  
  if (dir.exists(venv_path)) {
    packageStartupMessage("Python environment detected: ", venv_path)
  } else {
    packageStartupMessage(
      "To use this package:\n",
      "  1. Create venv: uv venv venv\n",
      "  2. Activate: source venv/bin/activate\n",
      "  3. Setup: setup_sklearn()"
    )
  }
}





















































































#' @export
VENV_PATH <- NULL 

#' @export 
ns <- NULL
#' @export
sklearn <- NULL  
#' @export 
numpy <- NULL
#' @export
pandas <- NULL

#' @import reticulate
.onLoad <- function(libname, pkgname) {
  tryCatch({
  # Specify the name of the virtual environment
  env_name <- "nnetsauce_env"
  
  # Path to the virtual environment (persistent location)
  VENV_PATH <<- file.path(Sys.getenv("HOME"), ".nnetsauce", env_name)
  
  # Create the parent directory if it doesn't exist
  if (!dir.exists(dirname(VENV_PATH))) {
    dir.create(dirname(VENV_PATH), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check if the virtual environment exists
  if (!dir.exists(VENV_PATH)) {
    message("Creating Python virtual environment...")
    
    # Create the virtual environment
    tryCatch({
      reticulate::virtualenv_create(VENV_PATH, python = "python3")
    }, error = function(e) {
      stop("Failed to create virtual environment: ", e$message)
    })
    
    # Install required Python packages
    message("Installing Python packages...")
    tryCatch({
      reticulate::virtualenv_install(
        VENV_PATH,
        packages = c("scikit-learn", "nnetsauce",
        "numpy", "pandas"),
        pip_options = "--upgrade --no-cache-dir",
        ignore_installed = TRUE
      )
    }, error = function(e) {
      stop("Failed to install Python packages: ", e$message)
    })
  }
  # Use the 'Global', persistent virtual environment
  tryCatch({
    reticulate::use_virtualenv(VENV_PATH, required = TRUE)
    sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
    ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
    numpy <<- reticulate::import("numpy", delay_load = TRUE)
    pandas <<- reticulate::import("pandas", delay_load = TRUE)    
  }, error = function(e) {
    message("Failed to use persistent virtual environment: ", e$message)
    # Import sklearn lazily
    tryCatch({
      message("Installing packages in environment r-reticulate...")
      reticulate::py_install("scikit-learn", ignore_installed = TRUE)      
      reticulate::py_install("numpy", ignore_installed = TRUE)
      reticulate::py_install("pandas", ignore_installed = TRUE)
      reticulate::py_install("nnetsauce", ignore_installed = TRUE)
      reticulate::use_virtualenv("r-reticulate", required = TRUE)
      message("Importing sklearn...")
      sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
      ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
      numpy <<- reticulate::import("numpy", delay_load = TRUE)
      pandas <<- reticulate::import("pandas", delay_load = TRUE)    
    }, error = function(e) {        
      message("Importing sklearn from Global Env...")
      sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
      numpy <<- reticulate::import("numpy", delay_load = TRUE)
      pandas <<- reticulate::import("pandas", delay_load = TRUE)        
      system("pip install nnetsauce --upgrade --no-cache-dir")
      ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
    })
  })    
 }, error = function(e) { # If using 'Global' virtual environment fails, use  the default local 'r-reticulate'
    tryCatch({
      message("Installing sklearn from r-reticulate...")
      reticulate::py_install("scikit-learn", ignore_installed = TRUE)
      reticulate::py_install("nnetsauce", ignore_installed = TRUE)
      reticulate::py_install("numpy", ignore_installed = TRUE)
      reticulate::py_install("pandas", ignore_installed = TRUE)
      reticulate::use_virtualenv("r-reticulate", required = TRUE)
      message("Importing sklearn...")
      sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
      ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
      numpy <<- reticulate::import("numpy", delay_load = TRUE)
      pandas <<- reticulate::import("pandas", delay_load = TRUE)    
    }, error = function(e) { # If using 'r-reticulate' fails, use the default local 'Global' virtual environment, e.g in Colab     
      message("Importing sklearn from Global Env...")
      sklearn <<- reticulate::import("sklearn", delay_load = TRUE)      
      numpy <<- reticulate::import("numpy", delay_load = TRUE)
      pandas <<- reticulate::import("pandas", delay_load = TRUE)        
      system("pip install nnetsauce --upgrade --no-cache-dir")
      ns <<- reticulate::import("nnetsauce", delay_load = TRUE)
    })
  })  
}