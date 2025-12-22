setup_packages <- function(packages, repos = "https://cloud.r-project.org") {
  
  installed <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed]
  
  # Install if needed
  if (length(to_install) > 0) {
    message("Installing ", length(to_install), " package(s): ", 
            paste(to_install, collapse = ", "))
    
    # Try installation with error handling
    tryCatch({
      install.packages(to_install, repos = repos, dependencies = TRUE)
    }, error = function(e) {
      stop("Failed to install package(s): ", paste(to_install, collapse = ", "),
           "\nError: ", e$message)
    })
    
    # Verify installation was successful
    newly_installed <- rownames(installed.packages())
    still_missing <- to_install[!to_install %in% newly_installed]
    
    if (length(still_missing) > 0) {
      stop("Failed to install: ", paste(still_missing, collapse = ", "))
    }
  }
  
  # Load/attach all packages
  for (pkg in packages) {
    # Use requireNamespace for checking without attaching
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is not available")
    }
    
    # Use library to attach to search path
    if (!suppressPackageStartupMessages(
      require(pkg, character.only = TRUE, quietly = TRUE))) {
      stop("Cannot load package: ", pkg)
    }
  }
  
  message("Successfully loaded ", length(packages), " package(s)")
  return(invisible(TRUE))
}

# Required packages
required_pkgs <- c('data.table', 'stringr', 'tidyverse', 'KnowBR', 'sf', 'terra',
                   'rnaturalearth', 'ggplot2', 'svglite', 'patchwork', 'here',
                   'ggExtra', 'biscale', 'cowplot', 'tidyterra', 'maps',
                   'nFactors', 'rSDM', 'psych', 'ggside', 'gridExtra',
                   'sampbias', 'viridis', 'tidyr', 'stringr', 'exactextractr')


# Setup packages
setup_packages(required_pkgs, repos = 'https://cran.r-project.org')


