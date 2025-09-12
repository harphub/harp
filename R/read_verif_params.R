#' Verification parameter files
#'
#' `read_verif_params()` reads a parameter file for verification. This can be a
#' file that contains R code, a text file or a json file.
#'
#' @param file_name The name of the parameters file
#'
#' @return A named list
#' @export
#'
#' @examples
#' read_verif_params(system.file("config", "define_params.R"))
#' read_verif_params(system.file("config", "params.txt"))
#' read_verif_params(system.file("config", "params.json"))
read_verif_params <- function(file_name) {
  if (harpIO::get_file_ext(file_name) == "R") {
    source(file_name, local = TRUE)
    return(list(
      params = get("params"),
      defaults = get("defaults")
    ))
  } else {
    read_text_setup(file_name, is_param_file = TRUE)
  }
}

#' Copy and open an example verification parameters file
#'
#' Create a new verification parameters from an example. You can choose whether
#' to have this file as an R script (the recommended type), a json file or a
#' text file. The example files are quite comprehensive so should be edited
#' to your needs.
#'
#' @param file_name The name of the file you want to make. It should either
#'   have no extension, or an extension that matches `type`.
#' @param type The type of file to create. Can be "R" (the default), "json", or
#'   "txt".
#'
#' @export
make_verif_params_file <- function(file_name, type = c("R", "json", "txt")) {
  type <- match.arg(type)
  file_ext <- harpIO::get_file_ext(file_name)
  if (nchar(file_ext) < 1) {
    file_name <- paste(file_name, type, sep = ".")
  } else {
    if (file_ext != type) {
      cli::cli_abort(c(
        "File extension does not match type.",
        "x" = paste(
          "You suppied a file name with extension {cli::col_red(file_ext)}",
          "and {.arg type} = {cli::col_red(type)}."
        ),
        "i" = "Either do not include an extension or make it match {.arg type}."
      ))
    }
  }

  file.copy(
    system.file(
      "config", paste0("define_params", type), package = "harp"
    ),
    file_name
  )

  file_edit(file_name)
}

split_on_empty <- function(x) {
  if (nchar(x[[length(x)]]) > 0) {
    x[[length(x) + 1]] <- ""
  }
  idx <- which(nchar(x) < 1)
  if (length(idx) < 1) {
    return(list(x))
  }
  start <- dplyr::lag(idx, default = 0) + 1
  end <- idx - 1
  mapply(function(a, b) x[a:b], start, end, SIMPLIFY = FALSE)
}

read_text_setup <- function(file_name, is_param_file = FALSE) {
  setup <- split_on_empty(remove_comments_and_no_equals(readLines(file_name)))
  if (!is_param_file) {
    setup <- unlist(setup, recursive = FALSE)
    setup <- setup[vapply(setup, function(x) nchar(x) > 0, logical(1))]
  }
  setup <- lapply(
    setup,
    function(x) unlist(lapply(x, parse_setup_line), recursive = FALSE)
  )
  if (!is_param_file) {
    return(lapply(
      unlist(setup, recursive = FALSE),
      function(x) {
        if (length(x) == 1 && x == "NULL") x <- NULL
        if (length(x) == 1 && x == "NA") x <- NA
        x
      }
    ))
  }

  setup <- lapply(
    setup,
    function(x) {
      names(x) <- gsub("^fc_", "fcst_", names(x))
      x[
        vapply(x, function(y) length(y) == 1 && y == "NULL", logical(1))
      ] <- NULL
      x[
        vapply(x, function(y) length(y) == 1 && y == "NA", logical(1))
      ] <- NA
      x
    }
  )

  if (any(vapply(setup, function(x) x$param == "defaults", logical(1)))) {
    params_idx <- vapply(setup, function(x) x$param != "defaults", logical(1))
    return(list(
      params = do.call(c, lapply(
        setup[params_idx], function(x) do.call(make_verif_param, x)
      )),
      defaults = as_verif_defaults(setup[[which(!params_idx)]])
    ))
  }

  list(
    params = do.call(c, lapply(setup, function(x) do.call(make_verif_param, x)))
  )
}

remove_comments_and_no_equals <- function(x) {
  x <- x[vapply(x, function(l) !grepl("^#", l), logical(1))]
  x <- lapply(x, function(l) sub("\\s*$", "", sub("#(.*)$", "", l)))
  x[vapply(x, function(l) grepl("^\\s*$|=", l), logical(1))]
}

parse_setup_line <- function(x) {
  regex <- "^[[:graph:]]+[[:space:]]*"
  res <- list(sub(paste0(regex, "\\=[[:space:]]*"), "", x))
  names(res) <- sub(
    "[[:space:]]+$",
    "",
    regmatches(x, regexpr(paste0(regex, "(?=\\=)"), x, perl = TRUE))
  )
  res[[1]] <- strsplit(res[[1]], "\\s*;\\s*")[[1]]
  res[[1]] <- get_list_names(res[[1]])
  res[[1]] <- sapply(
    res[[1]], parse_list_el,
    USE.NAMES = FALSE, simplify = FALSE
  )

  if (names(res) == "verif_groups") {
    if (is.list(res[[1]]) && length(res[[1]]) > 1) {
      time_groups <- which(sapply(
        res[[1]],
        function(x) {
          length(intersect(x, c("lead_time", "valid_dttm", "valid_hour"))) > 0
        }
      ))
      if (length(time_groups) == 1 && time_groups < length(res[[1]])) {
        verif_groups <- harpCore::make_verif_groups(
          res[[1]][[time_groups]], res[[1]][[time_groups + 1]]
        )
        if (time_groups > 1) {
          res[[1]] <- c(res[[1]][1:(time_groups - 1)], verif_groups)
        } else {
          res[[1]] <- verif_groups
        }
      }
      res[[1]] <- lapply(res[[1]], function(x) {
        if (length(x) == 1 && x == "NULL") {
          NULL
        } else {
          x
        }
      })
    } else {
      res[[1]] <- unlist(res[[1]])
    }
    return(res)
  }

  if (is.list(res[[1]]) && length(res[[1]]) > 1) {

    res[[1]] <- lapply(
      res[[1]], function(x) tryCatch(as.numeric(x), warning = function(w) x)
    )

  } else {

    res[[1]] <- unlist(res[[1]])

    if (
      !is.null(res[[1]]) &&
        any(res[[1]] != "NULL") &&
        grepl("_scaling", names(res))
    ) {
      res[[1]]        <- as.list(res[[1]])
      res[[1]][[1]]   <- as.numeric(res[[1]][[1]])
      names(res[[1]]) <- c("scaling", "new_units", "mult")[1:length(res[[1]])]

      if (length(res[[1]]) == 3) {
        res[[1]][[3]]      <- as.logical(res[[1]][[3]])
      }

      res[[1]] <- do.call(make_scaling, res[[1]])
      return(res)
    }

    res[[1]] <- tryCatch(as.numeric(res[[1]]), warning = function(w) res[[1]])

    if (length(res[[1]]) == 1) {
      if (toupper(res[[1]]) %in% c("TRUE", "FALSE")) {
        res[[1]] <- as.logical(res[[1]])
      }
    }

  }
  res
}

parse_list_el <- function(x) {
  x <- strsplit(x, "\\s*,\\s*")[[1]]
  seq_idx <- grep("..", x, fixed = TRUE)
  if (length(seq_idx) < 1) {
    return(x)
  }
  seq_start <- as.numeric(regmatches(
    x[seq_idx], regexpr("-?\\d*\\.{0,1}\\d+(?=..)", x[seq_idx], perl = TRUE)
  ))
  seq_end <- as.numeric(sub("-?\\d*\\.{0,1}\\d+\\..", "", x[seq_idx]))
  if (seq_idx == 1) {
    seq_by = 1
  } else {
    seq_by = seq_start - as.numeric(x[seq_idx - 1])
  }
  seq_all <- seq(seq_start, seq_end, seq_by)
  if (seq_idx > 1) {
    seq_all <- c(as.numeric(x[1:(seq_idx - 1)]), seq_all)
  }
  if (seq_idx < length(x)) {
    seq_all <- c(seq_all, as.numeric(x[(seq_idx + 1):length(x)]))
  }
  seq_all
}

get_list_names <- function(x) {
  res <- lapply(x, function(l) {
    name_l <- regmatches(l, regexpr("^[[:graph:]]+(?=:)", l, perl = TRUE))
    if (length(name_l) < 1) {
      name_l <- NULL
    } else {
      l <- sub("^[[:graph:]]+:\\s*", "", l)
    }
    list(value = l, key = name_l)
  })
  if (any(vapply(res, function(v) is.null(v$key), logical(1)))) {
    return(x)
  }
  names(res) <- vapply(res, function(v) v$key, character(1))
  lapply(res, function(l) l$value)
}

as_verif_defaults <- function(x) {
  x <- x[names(x) %in% c("verif_groups", "obs_error_sd")]
  do.call(make_verif_defaults, x)
}

