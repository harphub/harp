# Creates or opens a verification config file in the default editor.

#' Verification configuration files
#'
#' These functions help you to make, edit and read configuration files for
#' verification in harp. `make_*_verif_config()` will open an example
#' configuration that you can edit and save. `edit_config()` opens an editor
#' for an existing file. More documentation about the variables in the
#' configuration file can found in \code{\link{run_point_verif()}}.
#'
#' In _RStudio_ the file will be opened in the _Source_ pain, otherwise your
#' default editor will be used.
#'
#' @param config_file_name The name of the configuration file to be made
#'
#' @export
#' @seealso [run_point_verif()]
make_point_verif_config <- function(config_file_name) {
  file.copy(
    system.file("config", "point_verif_config.txt", package = "harp"),
    config_file_name
  )
  file_edit(file = config_file_name)
}

#' @rdname make_point_verif_config
#' @export
edit_config <- function(config_file_name) {
  if (!file.exists(config_file_name)) {
    cli::cli_abort(c(
      "File not found: {.file config_file_name}",
      "i" = paste(
        "Use {.fn make_point_verif_config()}",
        "to make a new configration file."
      )
    ))
  }
  file_edit(config_file_name)
}

#' Read a configuration file
#'
#' A configuration file is used to provide information for a verification
#' run, such as paths, file templates, file formats, and other basic settings.
#'
#' @param config_file_name The full path to the configuration file
#'
#' @return A named list
#' @export
#'
read_config <- function(config_file_name) {
  read_text_setup(config_file_name)
}

file_edit <- function(file_name) {
  if (!dir.exists(dirname(file_name))) {
    dir.create(dirname(file_name), recursive = TRUE)
  }
  file_opening <- "Modifying "
  if (!file.exists(file_name)) {
    file.create(file_name)
    file_opening <- "Creating "
  }
  cli::cli_bullets(c("*" = "{file_opening} file: {cli::col_red(file_name)}"))
  has_rstudio <- requireNamespace("rstudioapi", quietly = TRUE)
  if (
    has_rstudio &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("navigateToFile"
    )
  ) {
    rstudioapi::navigateToFile(file_name)
  }
  else {
    utils::file.edit(file_name)
  }
  invisible(file_name)
}

