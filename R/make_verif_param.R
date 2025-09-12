#' Make a parameter object for verification
#'
#' `make_verif_param()` makes a list of options for a verification parameter to
#' be passed to \code{\link{run_point_verif}()}. At a minimum the parameter
#' name is required. Other options including different names for reading the
#' parameter from forecasts and observations; scaling; jittering; groups,
#' thresholds and comparators for verification can be set here.
#'
#' Multiple parameters can be defined by wrapping calls to `make_verif_param()`
#' in `c()`, i.e. `c(make_verif_param(...), make_verif_param(...)`.
#'
#' @param param The name of the parameter.
#' @param fcst_param The name of the parameter to be used for reading forecast
#'   data. If NULL, `param` is used.
#' @param fcst_scaling Any scaling to apply to the forecast data. Should be set
#'   with \code{\link{make_scaling}()}.
#' @param fcst_jitter_func A function to jitter ensemble forecast data to take
#'   account of observation errors. See \code{\link[harpPoint]{jitter_fcst}()}.
#' @param obs_param The name of the parameter to be used for reading
#' observations data. If NULL, `param` is used.
#' @param obs_scaling Any scaling to apply to the observations data. Should be set
#'   with \code{\link{make_scaling}()}.
#' @param obs_min,obs_max The minimum and maximum values that an observation can
#'   have - used in the gross error check at read time, so should be in the same
#'   units as in the observations file.
#' @param obs_error_sd The maximum number of standard deviations that an
#'   observation can be different from a forecast. Used as a sort of
#'   represntativeness check. See
#'   \code{\link[harpPoint]{check_obs_against_fcst}()} for more details.
#' @param verif_groups The groups for which to compute verification scores. That
#'   is to say the stratification of the verification scores.
#'   \code{\link[harpCore]{make_verif_groups}()} can be used to define the
#'   verification groups. The default is for the verification to be grouped by
#'   lead time.
#' @param verif_thresholds The thresholds to be used for computing contingency
#'   table based and probabilistic scores for the parameter. If different
#'   thresholds are to be used with different comparators, `thresholds` should
#'   be a list with the same length as `verif_comparator`.
#' @param verif_comparator The comparator used to determine how the thresholds
#'   define classes. By default this is `"ge"` for >=. Can also be `"lt"`, `"le"`,
#'   `"eq"`, `"gt"`, `"between"`, or `"outside"`.
#' @param verif_comp_inc_low,verif_comp_inc_high For the `"between"` and
#'   `"outside"` comparators, whether to include the lowest and highest values
#'   in the range.
#' @param verif_circle For cyclic parameters, like wind direction, the distance
#'   around the circle in the units of the parameter. So, for degrees this would
#'   be `360` and radians `2 * pi`.
#' @param verif_members Whether to verify the members individually as
#'   deterministic forecasts for ensemble forecasts.
#' @param vertical_coordinate For upper air parameters, the coordinate system
#'   used in the vertical. Can be `"pressure"`, `"height"`, or `"model"`.
#'
#' @return A list of options for a parameter.
#' @export
#'
#' @examples
#' make_verif_param("T2m")
#'
#' # Add scaling for forecasts and observations
#' make_verif_param(
#'   "T2m",
#'   fcst_scaling = make_scaling(-273.15, "degC"),
#'   obs_scaling  = make_scaling(-273.15, "degC")
#' )
#'
#' # Different names reading forecasts and observations
#' make_verif_param(
#'   "2m temperature",
#'   fcst_param = "T2m",
#'   obs_param  = "T2m"
#' )
#'
#' # Make thresholds for wind direction with the major directions in the
#' # middle of the class
#' make_verif_param(
#'   "D10m",
#'   verif_thresholds    = list(seq(22.5, 337.5, 45), c(22.5, 337.5)),
#'   verif_comparator    = c("between", "outside"),
#'   verif_comp_inc_low  = c(TRUE, FALSE),
#'   verif_comp_inc_high = c(FALSE, TRUE),
#'   verif_circle        = 360
#' )
#'
#' # Groups for different time groups and other stratifications
#' make_verif_param(
#'   "T2m",
#'    verif_groups = make_verif_groups(
#'      c("lead_time", "valid_dttm", "valid_hour"),
#'      c("fcst_cycle", "station_group")
#'    )
#' )
#'
#' # Make multiple parameters
#' c(
#'   make_verif_param("T2m", verif_thresholds = seq(-25, 35, 5)),
#'   make_verif_param("S10m", verif_thresholds = c(1, 2, 4, 8)),
#'   make_verif_param(
#'     "CCtot", verif_thresholds = seq(0, 8), verif_comparator = "eq"
#'   ),
#'   make_verif_param("T", vertical_coordinate = "pressure")
#' )
make_verif_param <- function(
  param,
  fcst_param          = NULL,
  fcst_scaling        = NULL,
  fcst_jitter_func    = NULL,
  obs_param           = NULL,
  obs_scaling         = NULL,
  obs_min             = NULL,
  obs_max             = NULL,
  obs_error_sd        = NULL,
  verif_groups        = NULL,
  verif_thresholds    = NULL,
  verif_comparator    = "ge",
  verif_comp_inc_low  = TRUE,
  verif_comp_inc_high = TRUE,
  verif_circle        = NULL,
  verif_members       = TRUE,
  vertical_coordinate = NA_character_
) {
  out <- list()
  out[[param]] <- list(
    fc_param            = fcst_param,
    fc_scaling          = fcst_scaling,
    fc_jitter_func      = fcst_jitter_func,
    obs_param           = obs_param,
    obs_scaling         = obs_scaling,
    obs_min             = obs_min,
    obs_max             = obs_max,
    obs_error_sd        = obs_error_sd,
    verif_groups        = verif_groups,
    verif_comparator    = verif_comparator,
    verif_comp_inc_low  = verif_comp_inc_low,
    verif_comp_inc_high = verif_comp_inc_high,
    verif_thresholds    = verif_thresholds,
    verif_circle        = verif_circle,
    verif_members       = verif_members,
    vertical_coordinate = vertical_coordinate
  )
  class(out) <- c("harp_verif_param", class(out))
  out
}

#' @export
c.harp_verif_param <- function(...) {
  structure(NextMethod(), class = "harp_verif_param")
}

#' @export
`[.harp_verif_param` <- function(x, i, ...) {
  structure(NextMethod(), class = "harp_verif_param")
}

#' Make defualts for point verification
#'
#' `make_verif_defaults()` can be used to set default values for point
#' verification regardless of the parameter. This is currently restricted to
#' verification groups and the number of standard deviations allowed between
#' observations and forecasts for observations to be accepted into the
#' verification. All other options are set at the parameter level using
#' \code{\link{make_verif_param()}}
#'
#' @inheritParams make_verif_param
#' @export
make_verif_defaults <- function(
  verif_groups = "lead_time",
  obs_error_sd = NULL
) {
  out <- list(verif_groups = verif_groups, obs_error_sd = obs_error_sd)
  class(out) <- c("harp_verif_defaults", class(out))
  out
}

#' Make a list for scaling forecasts or observations
#'
#' `make_scaling()` is a helper function to be used when defining the scaling
#' for verification parameters with \code{\link{make_verif_param}}.
#'
#' @inheritParams harpCore::scale_param
#' @export
make_scaling <- function(
  scaling,
  new_units,
  mult = FALSE
) {

  check_type(scaling, "numeric", "scaling")
  check_type(new_units, "character", "new_units")
  check_type(mult, "logical", "mult")

  check_len(scaling, 1, "scaling")
  check_len(new_units, 1, "new_units")
  check_len(mult, 1, "mult")

  list(
    scaling   = scaling,
    new_units = new_units,
    mult      = mult
  )
}

#' Write parameter definitions to a file
#'
#' Once parameter definitions have been made using
#' \code{\link{make_verif_param}()} `write_verif_params()` can be used to
#' save them to a file for future use. You have a choice to save as R source
#' code, or JSON such that the files are editable outside of R, or as an rds
#' file that can be read into an R session.
#'
#' @param param A list containing one or more parameter created using
#'   \code{\link{make_verif_param}()}
#' @param file_name The full path to the file where the parameters are to be
#'   saved.
#' @param file_type The type of file to save. Can be "R" for an R source file,
#'   "rds", or "json". If one of these is the file extension in `file_name`
#'   then `file_type` is ignored.
#'
#' @export
write_verif_params <- function(
  param, file_name, file_type = c("R", "rds", "json")
) {
  file_type <- match.arg(file_type)
  file_extension <- harpIO::get_file_ext(file_name)
  if (length(file_extension) > 0 && file_extension %in% c("R", "rds", "json")) {
    file_type <- file_extension
  } else {
    file_type <- match.arg(file_type)
    file_name <- paste(file_name, file_type, sep = ".")
  }
  cli::cli_inform(
    "Writing params to {file_name}"
  )
  switch(
    file_type,
    "R"    = write_params_r(param, file_name),
    "rds"  = saveRDS(param, file_name),
    "json" = write_params_json(param, file_name)
  )
}

write_params_json <- function(param, file_name) {
  param <- lapply(
    param,
    lapply,
    function(x) {
      if (is.function(x)) {
        fun_to_source(x)
      } else {
        x
      }
    }
  )
  jsonlite::write_json(
    param,
    file_name,
    auto_unbox = TRUE,
    pretty     = TRUE,
    force      = TRUE,
    null       = "null",
    na         = "string"
  )
}

read_params_json <- function(file_name) {
  prm <- jsonlite::read_json(file_name, simplifyVector = TRUE)
  prm <- lapply(prm, lapply, function(x) ifelse(x == "NA", NA_character_, x))
  prm <- lapply(prm, lapply, function(x) {if (length(x) < 1) {NULL} else {x}})
  prm <- lapply(
    prm,
    lapply,
    function(x) {
      if (any(grepl("^function", x))) {
        eval(parse(text = paste(x, collapse = "")))
      } else {
        x
      }
    }
  )
  defaults <- prm$defaults
  list(
    params = structure(
      prm[names(prm) != "deafaults"], class = "harp_verif_param"
    ),
    defaults = if (is.null(defaults)) {
      NULL
    } else {
      do.call(make_verif_defaults, defaults)
    }
  )
}

write_params_r <- function(param, file_name, params_var = "params") {
  out_file    <- file(file_name, "w")
  source_code <- list_to_source(param)
  source_code <- paste0(
    params_var, " <- ", "structure(\n  ", source_code,
    ',\n  class = "harp_verif_param"\n)'
  )
  writeLines(source_code, out_file)
  close(out_file)
}

write_params_txt <- function(param, file_name) {
  out_file   <- file(file_name, "w")
  params_txt <- list_to_txt(param)
  writeLines(params_txt, out_file)
  close(out_file)
}

check_type <- function(x, type, arg, caller = rlang::caller_env()) {
  check_fun <- get(paste("is", type, sep = "."))
  if (check_fun(x)) {
    return()
  }
  cli::cli_abort(c(
    "Incorrect type for {.arg arg}.",
    "i" = "{.arg arg} must be {.class {type}}.",
    "x" = "You provided a {.cls class(x)} object."
  ), call = caller)
}

check_len <- function(x, len, arg, caller = rlang::caller_env()) {
  if (length(x) %in% len) {
    return()
  } else {
    len_inform <- glue::glue_collapse(len, sep = ", ", last = " or ")
    cli::cli_abort(c(
      "Incorrect length for {.arg {arg}}.",
      "i" = "{.arg {arg}} must be a length {len_inform} vector.",
      "x" = "You provided a length {length(x)} vector."
    ), call = caller)
  }
}

make_thresholds <- function(thresh, comparator) {
  if (length(comparator) == 1) {
    if (comparator %in% c("between", "outside")) {
      if (!is.list(thresh)) {
        thresh <- vec_to_pairlist(thresh)
      }
    }
   thresh <- list(thresh)
   names(thresh) <- comparator
   return(thresh)
  }
  if (!is.list(thresh)) {
    thresh <- lapply(seq_along(comparator), function(x) thresh)
  }
  if (length(thresh) != length(comparator)) {
    cli::cli_abort(c(
      "Length mismatch between {.arg thresh} and {.arg comparator}",
      "i" = paste(
        "For more than one {.arg comparator}, {.arg thresh} must be a list of",
        "the same length as {.arg comparator} or an atomic vector."
      ),
      "x" = paste(
        "You suppiled {.arg comparator} with length {length(comparator)}",
        "and {.arg thresh} with length {length(thresh)}."
      )
    ))
  }
  names(thresh) <- comparator
  thresh[names(thresh) %in% c("between", "outside")] <- lapply(
    thresh[names(thresh) %in% c("between", "outside")],
    vec_to_pairlist
  )
  thresh
}

vec_to_pairlist <- function(x) {
  lapply(
    seq_along(x)[1:length(x) - 1],
    function(i) x[c(i, i + 1)]
  )
}

list_to_source <- function(l, indent = 2) {
  l <- l[vapply(l, function(x) !is.null(x), logical(1))]
  if (is.null(names(l))) {
    elements <- lapply(l, list_element_to_source, indent = indent)
  } else {
    elements <- mapply(
      list_element_to_source, l, names(l), USE.NAMES = FALSE, SIMPLIFY = FALSE,
      MoreArgs = list(indent = indent)
    )
  }

  paste0(
    "list(\n",
    paste(unlist(elements), collapse = ",\n"),
    "\n", paste(rep(" ", indent), collapse = ""), ")"
  )
}

list_element_to_source <- function(x, name = NULL, indent = 0) {
  indent <- indent + 2
  prefix <- paste(rep(" ", indent), collapse = "")
  if (!is.null(name)) {
    if (grepl("\\s", name)) name <- paste("`", "`", sep = name)
    prefix <- paste0(prefix, name, " =")
  }
  if (is.numeric(x))   {
    out <- num_to_source(x)
  } else if (is.character(x)) {
    out <- char_to_source(x)
  } else if (is.logical(x)) {
    out <- lgl_to_source(x)
  } else if (is.function(x)) {
    out <- fun_to_source(x)
  } else if (is.list(x)) {
    out <- list_to_source(x, indent)
  }
  paste(prefix, out)
}

num_to_source <- function(num) {
  num <- as.character(num)
  num[is.na(num)] <- "NA_real_"
  if (length(num) == 1) {
    return(num)
  }
  paste0(
    "c(",
    paste(num, collapse = ", "),
    ")"
  )
}

char_to_source <- function(char) {
  char[is.na(char)] <- "NA_character_"
  if (length(char) == 1) {
    out <- paste('"', '"', sep = char)
  } else {
    out <- paste0(
      'c("',
      paste(char, collapse = '", "'),
      '")'
    )
  }
  gsub('"NA_character_"', 'NA_character_', out)
}

lgl_to_source <- function(lgl) {
  lgl <- as.character(lgl)
  lgl[is.na(lgl)] <- "NA"
  if (length(lgl) == 1) {
    return(lgl)
  }
  paste0(
    "c(",
    paste(lgl, collapse = ", "),
    ")"
  )
}

fun_to_source <- function(x, indent) {
  x <- utils::capture.output(print(x))
  x <- grep("<bytecode:|<environment:", x, invert = TRUE, value = TRUE)
  paste(x, collapse = paste0("\n", paste(rep(" ", indent), collapse = "")))
}

list_to_txt <- function(l) {
  l <- mapply(
    function(key, value) c(list(param = key), value),
    names(l), l,
    USE.NAMES = FALSE, SIMPLIFY = FALSE
  )

  l <- lapply(
    l,
    function(x) lapply(x, function(y) {
      if(is.list(y)) {
        el_to_string(y)
      } else {
        vec_to_string(y)
      }
    })
  )

  l <- lapply(l, fix_scaling)

  key_length <- max(nchar(Reduce(union, lapply(l, names))))

  Reduce(
    function(x, y) c(x, "", y),
    lapply(
      l,
      function(x) unlist(
        mapply(
          function(key, value) {
            paste(harpIO:::pad_string(key, key_length), value, sep = " = ")
          },
          names(x), x,
          USE.NAMES = FALSE, SIMPLIFY = FALSE
        )
      )
    )
  )
}

vec_to_string <- function(vec) {
  if (is.null(vec)) {
    return("NULL")
  }
  paste(vec, collapse = ", ")
}

el_to_string <- function(el) {
  paste(lapply(el, vec_to_string), collapse = "; ")
}

fix_scaling <- function(x) {
  x[grepl("scaling", names(x))] <- lapply(
    x[grepl("scaling", names(x))], function(y) gsub(";", ",", y)
  )
  x
}

pad_string <- function(x, string_length, text = " ") {
  if (nchar(text) != 1) {
    stop("`text` must be a 1 character string")
  }
  if (nchar(x) > string_length) {
    stop("`x` must have <= ", string_length, " characters")
  }
  out <- paste0(rep(as.character(text), string_length), collapse = "")
  substr(out, 1, nchar(x)) <- x
  out
}
