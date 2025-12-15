# # Create working directory
# create_and_set_directory <- function(dir_e) {
#   # Create directory if it doesn't exist
#   if (!dir.exists(dir_e)) {
#     dir.create(dir_e, recursive = TRUE)
#     message(paste("Directory created:", dir_e))
#   } else {
#     message(paste("Directory already exists:", dir_e))
#   }
#
#   # Set the working directory using here::here()
#   setwd(dir_e)
#   message(paste("Working directory set to:", getwd()))
#   return(getwd())
# }


create_and_set_directory <- function(
    dir_e = NULL,
    root_dir = wd
) {
  # Validar que root_dir no sea NULL
  if (is.null(root_dir) || root_dir == "") {
    stop("Root working directory empty")
  }

  # Construir path completo
  target_dir <- if (!is.null(dir_e)) {
    normalizePath(file.path(root_dir, dir_e), mustWork = FALSE)
  } else {
    normalizePath(root_dir, mustWork = FALSE)
  }

  # Crear directorio si no existe
  if (!dir.exists(target_dir)) {
    success <- tryCatch({
      dir.create(target_dir, recursive = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (!success) {
      stop(paste("Error creating directory:", target_dir))
    }
    message(paste("✓ Directory created:", target_dir))
  } else {
    message(paste("✓ Directory exists:", target_dir))
  }

  # Establecer directorio de trabajo
  old_wd <- getwd()
  setwd(target_dir)
  message(paste("✓ Working directory changed from:", old_wd))
  message(paste("✓ Working directory set to:", getwd()))

  return(invisible(getwd()))
}
