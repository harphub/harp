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
#' @param edit Logical. Whether to open the file in an editor.
#'
#' @export
#' @seealso [run_point_verif()]
make_point_verif_config <- function(config_file_name, edit = TRUE) {
  file.copy(
    system.file("config", "example_point_verif_config.txt", package = "harp"),
    config_file_name
  )
  if (edit) {
    file_edit(file = config_file_name)
  }
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

#' Create a new point verification project
#'
#' This function copies all the files that you need to run a point verification
#' job from a shell terminal into the specified directory. This includes a
#' parameter definitions file, a configuration file, the run script and a
#' README. The names of these files can optionally be changed, though it is
#' recommended to use the defaults until you become accustomed with the system.
#' You can also optionally automatically open the necessary files for editing.
#'
#' @param dir The directory for the verification project. If it does not exist,
#'   you will prompted to confirm that you want to create the directory.
#' @param permission  The permissions for `dir`. The default is 755.
#' @param config_file The name of the configuration file. This defaults to
#'   `harp_point_verif_config.txt`.
#' @param params_file The name of the parameter definitions file. This defaults
#'   to `harp_point_verif_params.R`, which contains R code using helper
#'   functions to define some aspects of the parameters. You may also give a
#'   file name with a `.json` or a `.txt` extension, in which case the params
#'   file will be in that format.
#' @param script_file The name of the script that will run the verification.
#'   This defaults to `harp_point_verify.R` and in most cases should not be
#'   edited once it is created.
#'
#' @export
new_point_verif_project <- function(
  dir,
  permission  = "755",
  config_file = "harp_point_verif_conf.txt",
  params_file = "harp_point_verif_params.R",
  script_file = "harp_point_verify.R"
) {
  if (dir.exists(dir)) {
    empty_dir <- TRUE
    num_files <- length(list.files(dir, include.dirs = TRUE))
    if (num_files > 0) {
      cli::cli_inform("{cli::col_blue(dir)} is non-empty:")
      print(noquote(dir))
      continue()
    }
  } else {
    cli::cli_inform(paste("Creating directory:", cli::col_blue(dir)))
    continue()
    dir.create(dir, recursive = TRUE, mode = permission)
  }
  make_point_verif_config(file.path(dir, config_file), edit = FALSE)
  params_file_type <- harpIO::get_file_ext(params_file)
  if (nchar(params_file_type) < 1) {
    params_file_type <- "R"
    params_file      <- paste0(params_file, ".R")
  }
  make_verif_params_file(file.path(dir, params_file), params_file_type, edit = FALSE)
  file.copy(
    system.file("R/point_verify.R", package = "harp"), file.path(dir, script_file)
  )
  file.copy(
    system.file("doc/README", package = "harp"), file.path(dir, "README")
  )
  cat("Do you want to open project files for editing?\n")
  continue(FALSE)
  file_edit(file.path(dir, config_file))
  file_edit(file.path(dir, params_file))
}

continue <- function(ask = TRUE) {
  yes_no   <- c("Yes", "No")
  yn_order <- sample(c(1, 2), 2)
  choices <- paste(c("1.", "2."), yes_no[yn_order])
  if (ask) {
    cat(cli::col_red("Do you want to continue?\n"))
  }
  invisible(sapply(
    1:2,
    function(i) cat(i, ". ", yes_no[yn_order][i], "\n", sep = "")
  ))
  answer  <- readline(": ")
  choice <- switch(
    answer,
    "1" = yes_no[yn_order][1],
    "2" = yes_no[yn_order][2],
    NA
  )
  switch(
    choice,
    "Yes" = return(),
    "No"  = stop_quietly("Exiting"),
  )
  continue()
}

stop_quietly <- function(message = "") {
  if (nchar(message) > 0) {
    cat(cli::col_br_magenta(message))
  }
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
