setup_packages <- function(packages, repos = "https://cloud.r-project.org") {
  
  installed <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed]
  
  if (length(to_install) > 0) {
    message("Installing packages: ", paste(to_install, collapse = ", "))
    install.packages(to_install, repos = repos)
  }
  
  invisible(lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop("Cannot load: ", pkg)
    }
  }))
}
# Required packages
required_pkgs <- c('data.table', 'stringr', 'tidyverse', 'KnowBR', 'sf', 'terra',
                   'rnaturalearth', 'ggplot2', 'svglite', 'patchwork', 'here',
                   'ggExtra', 'biscale', 'cowplot', 'tidyterra', 'maps',
                   'nFactors', 'rSDM', 'psych', 'ggside', 'gridExtra',
                   'sampbias', 'viridis', 'tidyr', 'stringr', 'exactextractr')


# Setup packages
setup_packages(required_pkgs)


