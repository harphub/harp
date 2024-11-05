# Creates or opens a verification config file in the default editor.

#' Verification configuration files
#'
#' These functions help you to make, edit and read configuration files for
#' verification in harp. `make_*_verif_config()` will open an example
#' configuration that you can edit and save. `edit_config()` is a simple
#' wrapper around \code{\link[usethis]{edit_file}} so can actually be used to
#' open and edit any file. More documentation about the variables in the
#' configuration file can found in \code{\link{run_point_verif()}}.
#'
#' In _RStudio_ the file will be opened in the _Source_ pain, otherwise your
#' default editor will be used.
#'
#' @param config_file_name The name of the configuration file
#'
#' @export
#' @seealso [run_point_verif()]
make_point_verif_config <- function(config_file_name) {
  file.copy(system.file("config", "point_verif_config.txt"), config_file_name)
  usethis::edit_file(file = config_file_name)
}

#' @rdname make_point_verif_config
#' @export
edit_config <- function(config_file_name) {
  usethis::edit_file(config_file_name)
}

#' Title
#'
#' @param config_file_name
#'
#' @return
#' @export
#'
#' @examples
read_config <- function(config_file_name) {
  read_text_setup(config_file_name)
}

