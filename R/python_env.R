#' Check if python is installed
#' This function checks whether Python is installed on the system by attempting
#' to call teh C drive apps and searching for C:/Program Files/Python312/
#' @returns True/False.
#' @importFrom reticulate py_config
#'
#' @examples check_python_config()
check_python_installed <- function() {
  # Try to get python version
  p_paths <- list.files(
    "C:/Program Files/",
    pattern = "Python",
    recursive = TRUE,
    full.names = TRUE
  )
  p <- p_paths[grep("C:/Program Files/Python3", p_paths, ignore.case = TRUE)]

  if (length(p) > 0) {
    cat("Python is installed\n")
    return(TRUE) # Python found
  } else {
    cat(
      "Cannot find any python installation in your machine. If there is one but it cannot be found contact the package mantainers.\n"
    )
    cat(
      "Python is NOT installed, go to the Software Centre and install it. After you install it roboot your machine and try the app again.\n"
    )

    return(FALSE) # python not found
  }
}


#' Set up a python environment for the RainfallQC package in the userprofile folder.
#' This function checks whether Python is installed then it chekces if the right environment exists. If it doesn't then it will automatically set up the conda environment in your machine and call it rainfallqc-env in your userprofile folder. this evnironment will have all the dependencies needed for rainfallqc to work. It will install all dependencies via pip. It will never have to install the environment again if you keep it in userprofile.
#' @returns a set up environment called rainfallqc-env
#' @importFrom reticulate
#'
#' @examples setup_python()
setup_python <- function() {
  if (check_python_installed()) {
    # Choose a persistent path for the virtual environment:
    # For example, in your user's Documents folder or a specific project folder.
    # You can customize this path.
    env_path <- file.path(Sys.getenv("USERPROFILE"), "rainfallqc-env")

    # Check if virtualenv exists:
    if (!virtualenv_exists(env_path)) {
      cat("Creating virtual environment at:", env_path, "\n")
      virtualenv_create(env_path)
      cat("Installing RainfallQC package and dependencies...\n")
      virtualenv_install(
        env_path,
        packages = c("git+https://github.com/Thomasjkeel/RainfallQC.git")
      )
    } else {
      cat("Virtual environment already exists at:", env_path, "\n")
    }

    # Use the virtual environment:
    use_virtualenv(env_path, required = TRUE)
    cat("Virtual environment setup complete.\n")
    return(invisible(TRUE))
  }
}
