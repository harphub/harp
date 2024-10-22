read_config <- function(file_name) {
  read_text_setup(file_name)
}

read_verif_params <- function(file_name) {
  if (get_file_ext(file_name) == "R") {
    source(file_name, local = TRUE)
    return(list(
      params = get("params"),
      defaults = get("defaults")
    ))
  } else {
    read_text_setup(file_name, is_param_file = TRUE)
  }
}

get_file_ext <- function(file_name) {
  pos <- regexpr("\\.([[:alnum:]]+)$", file_name)
  ifelse(pos > -1L, substring(file_name, pos + 1L), "")
}

split_on_empty <- function(x) {
  idx <- which(nchar(x) < 1)
  if (length(idx) < 1) {
    return(list(x))
  }
  start <- dplyr::lag(idx, default = 0) + 1
  end <- idx - 1
  mapply(function(a, b) x[a:b], start, end, SIMPLIFY = FALSE)
}

read_text_setup <- function(file_name, is_param_file = FALSE) {
  setup <- split_on_empty(readLines(file_name))
    setup <- lapply(
    setup,
    function(x) unlist(lapply(x, parse_setup_line), recursive = FALSE)
  )
  if (!is_param_file) {
    return(unlist(setup, recursive = FALSE))
  }
  if (any(vapply(setup, function(x) x$param == "defaults", logical(1)))) {
    params_idx <- vapply(setup, function(x) x$param != "defaults", logical(1))
    return(list(
      params = do.call(c, lapply(
        setup[params_idx], function(x) do.call(make_verif_param, x)
      )),
      defaults = as_verif_defaults(setup[[which(!params_idx)]])
    ))
  }
  list(params = do.call(c, lapply(setup, function(x) do.call(make_verif_param, x))))
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

    if (grepl("_scaling", names(res))) {
      res[[1]]             <- as.list(res[[1]])
      res[[1]][[1]]        <- as.numeric(res[[1]][[1]])
      names(res[[1]][1:2]) <- c("scaling", "new_units")

      if (length(res[[1]]) == 3) {
        res[[1]][[3]]      <- as.logical(res[[1]][[3]])
        names(res[[1]][3]) <- "mult"
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

