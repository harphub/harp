#' Run a point verification task
#'
#' `run_point_verif()` runs a point verification task for one or more
#' parameters. It reads forecasts and observations from files and computes
#' verification scores. Deterministic or ensemble scores are computed depending
#' on the data.
#'
#' Currently it is only possible to read point data from harp formatted SQLite
#' files. These can be created using \code{\link[harpIO]{read_forecast}()} and
#' \code{\link[harpIO]{read_obs}()} for forecast and observations data
#' respectively.
#'
#' Many of the verification options are set at the parameter level. This means
#' that parameters must be passed to `run_point_verif()` in a specific format.
#' Parameters with this format can be created using
#' \code{\link{make_verif_param}()}. For options that are common across all
#' parameters, such as verification groups, the `defaults` argument should be
#' used passing data created with \code{\link{make_verif_defaults}}.
#'
#' @inheritParams harpIO::read_point_forecast
#'
#' @param params The parameters for which to do the verification. These need to
#'   be set using \code{\link{make_verif_params}()}.
#' @param station_groups A data frame defining groups each station belongs to.
#'   Should have column names "SID" for station ID and "station_group" for the
#'   group name.
#' @param main_fcst_model The `fcst_model` to treat as the main model. If not
#'   NULL, the stations will first be selected from this model and only these
#'   stations will be read from other `fcst_model`s. This will speed up the
#'   reading of data. In general the model with the smallest domain should be
#'   chosen as `main_fcst_model`. Will be ignored if `stations` is not NULL.
#' @param ens_mean_as_det Logical. For ensembles, should the ensemble mean be
#'   verified as a deterministic forecast.
#' @param dttm_rounding The multiple to which to round valid date-times. This
#'   should be a number followed "s", "m", "h", or "d" for seconds, minutes,
#'   hours and days. This is useful for aggregating data over time periods
#'   when stratifying verification by valid_dttm.
#' @param dttm_rounding_dirn The direction in which to round date-times. Can be
#'   "nearest", "up" or "down". For "nearest", the rounding is centred on
#'   `dttm_rounding`. Where there is an even number of date-times to be
#'   aggregated, the averaging window favours date-times before the rounding
#'   time. For example, if rounding date-times to "6h", the aggregation for 06
#'   UTC will take in 03, 04, 05, 06, 07 and 08 UTC, and the aggregation for 12
#'   UTC will take in 09, 10, 11, 12, 13 and 14 UTC etc. For "up" the rounding
#'   is for a window for all times up to and including `dttm_rounding` and for
#'   "down" the rounding is for a window starting at `dttm_rounding`.
#' @param dttm_rounding_offset The offset to be applied to `dttm_rounding`. This
#'   should be a number followed "s", "m", "h", or "d" for seconds, minutes,
#'   hours and days. This is used to centre the rounding. For example, if
#'   `dttm_rounding` = "1d", the rounding will be centred on 00 UTC. If you want
#'   it to be centred on 12 UTC, you would supply an offset of "12h". Note that
#'   the offset is applied backwards in time, so if you want to centre on 06
#'   UTC, for example, the offset should be "18h".
#' @param fcst_path The path for the forecast files.
#' @param fcst_template The template for the forecast files.
#' @param fcst_format The format of the forecast files. Currently this is
#'   limited to "fctable", but may be extended in the future to allow for other
#'   file formats.
#' @param obs_path The path for the observations files.
#' @param obs_template The template for the observations files.
#' @param obs_format The format of the observations files. Currently this is
#'   limited to "obstable", but may be extended in the future to allow for other
#'   file formats.
#' @param out_path The path to which to write output verification files.
#' @param out_template The template for output verification files.
#' @param out_format The format for output verification files. This is currently
#'   limited to the native R format "rds", but will be extended in the future to
#'   include SQLite and JSON.
#' @param defaults The defaults for the verification. Set using
#'   \code{\link{make_verif_defaults}()}. These defaults currently only cover
#'   verification groups and the number of standard deviations to use in the
#'   observation error checking.
#' @param return_data Logical. Whether to return the verification results to the
#'   global environment. Automatically set to TRUE if `out_path` is `NULL`.
#'
#' @return If `return_data = TRUE`, a list of data frames with the verification
#'   scores. Otherwise there is only the side effect of writing the verification
#'   scores to files.
#' @export
#'
run_point_verif <- function(
  dttm,
  fcst_model,
  params,
  lead_time            = seq(0, 48, 3),
  members              = NULL,
  lags                 = "0s",
  stations             = NULL,
  station_groups       = NULL,
  main_fcst_model      = NULL,
  ens_mean_as_det      = FALSE,
  dttm_rounding        = NULL,
  dttm_rounding_dirn   = c("nearest", "up", "down"),
  dttm_rounding_offset = 0,
  fcst_path            = getwd(),
  fcst_template        = "fctable",
  fcst_format          = "fctable",
  obs_path             = getwd(),
  obs_template         = "obstable",
  obs_format           = "obstable",
  out_path             = NULL,
  out_template         = "point_verif",
  out_format           = "rds",   # make extendable to include json
  defaults             = make_verif_defaults(),
  return_data          = TRUE
) {

  if (!inherits(params, "harp_verif_param")) {
    cli::cli_abort(c(
      "{.arg params} must have a class of {.cls harp_verif_param}.",
      "x" = "You supplied an object with class {.cls {class(params)}}.",
      "i" = "You should use {.fn make_verif_param} to define {.arg params}."
    ))
  }

  if (is.null(out_path)) {
    return_data <- TRUE
  }

  dttm_rounding_dirn <- match.arg(dttm_rounding_dirn)


  ## ALWAYS use map... if return_data = FALSE return "SUCCESS" if no errors,
  # otherwise the error will be returned.

  out <- purrr::imap(
    params,
    function(x, y) try(do_point_verif(
      x,
      y,
      fcst_model,
      dttm,
      lead_time,
      members,
      lags,
      stations,
      station_groups,
      main_fcst_model,
      ens_mean_as_det,
      dttm_rounding,
      dttm_rounding_dirn,
      dttm_rounding_offset,
      fcst_path,
      fcst_template,
      fcst_format,
      obs_path,
      obs_template,
      obs_format,
      defaults,
      out_path,
      out_template,
      out_format,
      return_data
    ))
  )

  ## Clean the data. Inform about errors.

  cli::cli_inform(c(
    "",
    "===================================================================",
    "  Verification complete",
    "===================================================================",
    ""
  ))

  purrr::iwalk(
    out,
    function(x, y) {
      if (inherits(x, "try-error")) {
        param_label   <- paste0(cli::col_br_red(y, ":"))
        param_message <- sapply(
          strsplit(x, "\n")[[1]], cli::col_br_red, USE.NAMES = FALSE
        )
        names(param_message) <- c("x", rep(" ", length(param_message) - 1))
      } else {
        param_label   <- paste0(cli::col_br_green(y, ":"))
        param_message <- c("v" = cli::col_br_green("Verification SUCCESSFUL!"))
      }
      cli::cli_inform(c(
        param_label,
        param_message,
        "",
        "{cli::col_br_blue('---')}",
        ""
      ))
    }
  )

  if (!is.null(out_path)) {
    cli::cli_inform(c(
      "i" = "Verification files saved under {cli::col_cyan(out_path)}",
      ""
    ))
  }

  if (return_data) {
    out <- out[!vapply(out, inherits, logical(1), "try-error")]
    if (length(out) == 1) {
      out <- out[[1]]
    }
    return(out)
  }
}



# function to do a point verification
# The arguments require specific data - could it be best to give them a class
# to ensure they are of the correct format produced by specific functions,
# or is it enough to have a function that checks????

# This internal function does the work. The user facing function either walks
# or maps over this one depending on whether data should be returned to the
# calling environment.

do_point_verif <- function(
  param_list,
  param_name,
  fc_models,
  dttm,
  ld_times,
  members,
  lags,
  stations,
  station_groups,
  main_fcst_model,
  ens_mean_as_det,
  dttm_rounding,
  dttm_rounding_dirn,
  dttm_rounding_offset,
  fc_data_dir,
  fc_file_template,
  fc_file_format,
  obs_data_dir,
  obs_file_template,
  obs_file_format,
  dflts,
  vrf_data_dir,
  vrf_file_template,
  vrf_file_format,
  return_data
) {

  cli::cli_inform(c(
    "",
    "-------------------------------------------------------------------------",
    "Doing verification for {cli::col_br_magenta(param_name)}",
    "-------------------------------------------------------------------------",
    ""
  ))

  # Check if the forecast parameter has a specific name
  fc_prm <- param_list$fc_param
  if (is.null(fc_prm)) {
    fc_prm <- param_name

  }

  # Check for different values of vertical coordinate and correct to
  # something that read_forecast wants

  vc <- as_vertical_coord(param_list$vertical_coordinate)

  # Select SIDs from the main fcst model.

  if (is.null(stations) && !is.null(main_fcst_model)) {
    fc_files <- harpIO::generate_filenames(
      file_path  = fc_data_dir,
      file_date  = dttm,
      parameter  = fc_prm,
      fcst_model = main_fcst_model
    )

    fc_files <- fc_files[file.exists(fc_files)]
    if (length(fc_files) > 0) {
      cli::cli_inform(c(
        "",
        "i" = paste(
          "Getting stations for {.arg main_fcst_model}:",
          "\"{main_fcst_model}\""
        ),
        ""
      ))
      stations <- Reduce(union, lapply(fc_files, sids_from_file))
    }
  }

  ## Get the common cases, read the observations and then loop over the
  ## forecast models, reading only the required data - this should save
  ## on memory usage without adding too much time expense - it may even
  ## be faster in the long run as only the required data are read in.

  cli::cli_inform(c(
    "",
    "i" = "Getting common cases across all forecast models",
    ""
  ))

  fcst_cases <- harpIO::read_point_forecast(
    dttm                = dttm,
    fcst_model          = fc_models,
    parameter           = fc_prm,
    lead_time           = ld_times,
    members             = members,
    lags                = lags,
    stations            = stations,
    file_path           = fc_data_dir,
    file_template       = fc_file_template,
    vertical_coordinate = vc,
    meta_only           = TRUE
  )

  if (is.na(vc)) {
    fcst_cases <- harpCore::common_cases(
      fcst_cases, rows_only = TRUE, "valid_dttm"
    )
  } else {
    vertical_col <- as_vertical_col(vc)
    fcst_cases <- harpCore::common_cases(
      fcst_cases, rows_only = TRUE, "valid_dttm", {{vertical_col}}
    )
  }

  no_data_test(
    fcst_cases,
    paste(
      "No common cases found between forecast models",
      glue::glue_collapse(fc_models, sep = ", ", last = " and ")
    )
  )

  # Read the observations
  obs_prm <- param_list$obs_param
  if (is.null(obs_prm)) {
    obs_prm <- param_name
  }

  cli::cli_inform(c(
    "",
    "i" = "Reading observation data {cli::symbol$arrow_right}",
    ""
  ))

  obs <- harpIO::read_point_obs(
    dttm                = harpCore::unique_valid_dttm(fcst_cases),
    parameter           = obs_prm,
    stations            = harpCore::unique_stations(fcst_cases),
    obs_path            = obs_data_dir,
    min_allowed         = param_list$obs_min,
    max_allowed         = param_list$obs_max,
    vertical_coordinate = vc
  )

  no_data_test(obs, "None of the requested observations data found in file(s).")

  # Join the observations to the cases
  common_cols <- intersect(colnames(fcst_cases), colnames(obs))
  obs <- harpCore::deharp(dplyr::inner_join(fcst_cases, obs, by = common_cols))

  rm(fcst_cases)

  no_data_test(obs, "No forecast - observations pairs found after joining.")

  # Scale the observations
  if (!is.null(param_list$obs_scaling)) {
    obs <- do.call(
      harpCore::scale_param,
      c(
        list(x = obs),
        param_list$obs_scaling,
        list(col = {{obs_prm}})
      )
    )
  }

  # Loop over forecast models, read in the data and do the verification

  #result <- list()

  #for (fc_model in fc_models) {

  # cli::cli_inform(c(
  #   "",
  #   "---",
  #   "i" = "Doing verification for {fc_model}",
  #   "---",
  #   ""
  # ))

  cli::cli_inform(c(
    "i" = "Reading forecast data for {cli::symbol$arrow_right}",
    ""
  ))
  # Read the forecasts

  fcst <- harpIO::read_point_forecast(
    dttm                = harpCore::unique_fcst_dttm(obs),
    fcst_model          = fc_models,
    parameter           = fc_prm,
    lead_time           = ld_times,
    members             = members,
    lags                = lags,
    stations            = harpCore::unique_stations(obs),
    file_path           = fc_data_dir,
    file_template       = fc_file_template,
    vertical_coordinate = vc
  )

  no_data_test(fcst, "None of the requested forecast data found in file(s).")

  # Need to check if all elements of fcst are the same type (i.e. all det or
  # all ens)

  cls <- get_fcst_class(fcst)

  if (is.element("not_fcst", cls)) {
    bad_idx <- which(cls == "not_fcst")
    err <- glue::glue(
      "\"",
      glue::glue_collapse(
        fc_models[bad_idx], sep = "\", \"", last = "\" and \""
      ),
      "\""
    )
    cli::cli_abort(c(
      "Not all data are harp forecast data.",
      "x" = "Non harp forecast data for {.arg fcst_model} = {err}"
    ))
  }


  fcst <- fix_fcst_classes(fcst, ens_mean_as_det)
  cls  <- get_fcst_class(fcst)

  cls <- unique(cls)

  # Scale the forecast
  if (!is.null(param_list$fc_scaling)) {
    fcst <- do.call(
      harpCore::scale_param,
      c(list(x = fcst), param_list$fc_scaling)
    )
  }


  # Join observations to forecast
  cli::cli_inform(c(
    "",
    "i" = "Joining forecast and observations data {cli::symbol$arrow_right}",
    ""
  ))
  fcst <- harpCore::join_to_fcst(fcst, obs)


  # Check for observation errors, comparing forecasts with observations
  error_sd <- param_list$obs_error_sd
  if (is.null(error_sd)) {
    error_sd <- dflts$obs_error_sd
  }

  cli::cli_inform(c(
    "",
    "i" = "Checking observations for errors {cli::symbol$arrow_right}",
    ""
  ))

  fcst <- harpPoint::check_obs_against_fcst(
    fcst,
    {{obs_prm}},
    num_sd_allowed = error_sd
  )

  no_data_test(fcst, "All observations failed check against forecasts.")

  # Send removed cases to a log file?

  # Rename the observations column (this is useful in providing a final name for
  # the parameter that will be used in the verification file name and thus the
  # shiny app)
  fcst <- dplyr::rename(fcst, {{param_name}} := {{obs_prm}})

  # Jitter the forecast to take account of observation errors
  if (cls == "ens" && is.function(param_list$fc_jitter_func)) {
    fun_args <- names(formals(param_list$fc_jitter_func))
    fun_args <- fun_args[!fun_args == "..."]
    if (length(fun_args) > 1) {
      obs_col <- param_name
    } else {
      obs_col <- NULL
    }
    fcst <- harpPoint::jitter_fcst(
      fcst, param_list$fc_jitter_func, {{obs_col}}
    )
  }

  grps <- param_list$verif_groups
  if (is.null(grps)) {
    grps <- dflts$verif_groups
  }

  if (
    is.list(grps) &&
      !is.null(names(grps)) &&
      sort(names(grps)) == c("groups", "time_groups")
  ) {
    grps <- do.call(harpCore::make_verif_groups, grps)
  }

  # Make modifications to data depending on groups
  grp_names <- unique(unlist(grps))

  if (any(grepl("valid_hour|valid_day|valid_month|valid_year", grp_names))) {
    fcst <- harpCore::expand_date(fcst, "valid_dttm")
  }



  if (is.element("station_group", grp_names)) {
    if (is.null(station_groups)) {
      cli::cli_inform(c(
        "",
        "!" = paste(
          "{.arg station_group} found in groups, but no {.arg station_group}",
          "data frame supplied"
        ),
        "i" = "Using default {.var station_groups}.",
        ""
      ))
      station_groups <- harpCore::station_groups
    }
    fcst <- harpCore::join_station_groups(fcst, station_groups)
  }

  if (!is.na(vc)) {
    if (is.list(grps)) {
      grps <- lapply(grps, function(x) c(x, vertical_col))
    } else {
      grps <- c(grps, vertical_col)
    }
  }

  # Round valid_dttm
  if (is.element("valid_dttm", grp_names) && !is.null(dttm_rounding)) {
    fcst <- dplyr::mutate(
      fcst, valid_dttm = harpCore::round_dttm(
        .data[["valid_dttm"]], dttm_rounding,
        dttm_rounding_dirn, dttm_rounding_offset
      )
    )
  }


  # Select the correct verification function
  verif_func <- switch(
    cls,
    "det" = harpPoint::det_verify,
    "ens" = harpPoint::ens_verify
  )

  # Get parameter and include_low, include_high in the right form
  comps <- check_comparator(
    param_list[
      c("verif_comparator", "verif_comp_inc_low", "verif_comp_inc_high")
    ]
  )

  # Thresholds should be a list to allow for multiple comparators
  thresholds <- make_thresholds(param_list$verif_thresholds, comps$comparator)

  idx <- seq_along(comps$comparator)

  # cli::cli_inform(c(
  #   "",
  #   "i" = "{cli::col_green('::Computing verification for fcst_model ', {fc_model}, '::')}",
  #   ""
  # ))

  verif <- lapply(
    idx,
    function(i) verif_func(
      fcst,
      {{param_name}},
      comparator     = comps$comparator[i],
      include_low    = comps$include_low[i],
      include_high   = comps$include_high[i],
      thresholds     = thresholds[[i]],
      groupings      = grps,
      summary        = i == 1,
      circle         = param_list$verif_circle,
      verify_members = param_list$verif_members
    )
  )

  verif <- mapply(
    add_thresh_type, verif, comps$comparator, SIMPLIFY = FALSE
  )

  #result[[fc_model]] <- harpPoint::bind_point_verif(verif)

  #}

  verif <- harpPoint::bind_point_verif(verif)

  attributes(verif) <- add_point_locs(attributes(verif), fcst)


  if (is.null(vrf_data_dir)) {
    return_data <- TRUE
  } else {
    # Save
    harpIO::save_point_verif(verif, vrf_data_dir)
  }

  if (return_data) {
    return(verif)
  }
  paste(param_name, ": SUCCESS!")
}


################################################################################
################################################################################
############################## Helper functions ################################
################################################################################
################################################################################

# Check what the classes of the data frames in a harp_list are
is_det <- function(cls) {
  any(grepl("harp_det_", cls))
}

is_ens <- function(cls) {
  any(grepl("harp_ens_", cls))
}

check_df_class <- function(x) {
  cls <- class(x)
  if (is_det(cls)) {
    return("det")
  }
  if (is_ens(cls)) {
    return("ens")
  }
  "not_fcst"
}

get_fcst_class <- function(x) {

  if (!harpCore::is_harp_list(x)) {
    return(check_df_class(x))
  }

  vapply(x, check_df_class, character(1L))

}

fix_fcst_classes <- function(x, ens_mean_as_det) {

  # Check for single member ensembles and convert to deterministic
  mbr_regex <- "_mbr[[:digit:]]{3}"

  if (harpCore::is_harp_df(x)) {
    if (length(grep(mbr_regex, colnames(x))) == 1) {
      x <- harpCore::as_det(x)
      cli::cli_inform(c(
        "Single member ensemble read in. Converting to deterministic."
      ))
    }
    return(x)
  }

  ens_idx <- which(get_fcst_class(x) == "ens")
  if (length(ens_idx) < 1) {
    return(x)
  }
  num_mbrs <- vapply(
    x[ens_idx],
    function(y) length(grep(mbr_regex, colnames(y))),
    integer(1L)
  )
  if (all(num_mbrs == 1)) {
    x[ens_idx] <- harpCore::as_det(x[ens_idx])
    cli::cli_inform(c(
      "i" = paste(
        "{length(ens_idx)} {.arg fcst_model{?s}} {?is a/are}",
        "single member ensemble{?s}."
      ),
      "i" = paste(
        "{.arg fcst_model} = {quoted_strings(names(x)[ens_idx])}",
        "converted to deterministic."
      )
    ))
    return(x)
  } else if (ens_mean_as_det) {
    x[ens_idx] <- harpCore::as_harp_list(
      lapply(
        dplyr::rename(
          harpCore::ens_stats(x[ens_idx], sd = FALSE), fcst_det = "ens_mean"
        ),
        harpCore::as_harp_df
      )
    )
    return(x)
  }

  # How to deal with multi member ensembles and deterministic - need an ensemble
  # method for single member ensembles - currently just tells you it can't do it
  # For now will just have to say it's not possible to combine deterministic
  # and ensemble in the same verification.
  ens_models <- quoted_strings(names(x)[ens_idx])
  det_models <- quoted_strings(names(x)[!seq_along(x) %in% ens_idx])
  cli::cli_abort(c(
    "x" = paste(
      "Cannot combine deterministic and ensemble forecasts",
      "in a single verification."
    ),
    "i" = "{ens_models} are ensembles and {det_models} are deterministic."
  ))
}

quoted_strings <- function(x) {
  paste0("'", glue::glue_collapse(x, sep = "', '", last = "' and '"), "'")
}

as_vertical_coord <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }

  x <- switch(
    x,
    "pressure" = ,
    "p"        = "pressure",
    "model"    = ,
    "hybrid"   = ,
    "m"        = "model",
    "height"   = ,
    "z"        = ,
    "h"        = "height",
    NA_character_
  )

  if (is.na(x)) {
    allowed_vc <- paste0(
      "\"",
      glue::glue_collapse(
        c("pressure", "p", "model", "hybrid", "ml",  "height", "z", "h"),
        sep = "\", ", last = ", or \""
      ),
      "\""
    )
    cli::cli_warn(c(
      "Unknown {.arg vertical_coordinate}.",
      "i" = "{.arg vertical_coordinate} must be one of {allowed_vc}.",
      "x" = "You supplied {.arg vertical_coordinate} = {x}."
    ))
  }

  x
}

as_vertical_col <- function(x) {
  switch(
    x,
    "pressure" = "p",
    "model"    = "ml",
    "height"   = "z"
  )
}

check_comparator <- function(l) {
  names(l) <- c("comparator", "include_low", "include_high")
  if (is.null(l$comparator)) {
    l$comparator   <- "ge"
    l$include_low  <- TRUE
    l$include_high <- TRUE
  }
  lengths <- vapply(l, length, numeric(1))
  if (length(lengths) == 1) {
    return(l)
  }
  singletons <- which(lengths == 1)
  if (length(singletons) > 0) {
    l[singletons] <- lapply(
      singletons,
      function(x) {
        l[[x]] <- rep(l[[x]], max(lengths))
      }
    )
  }
  lengths <- unique(vapply(l, length, numeric(1)))
  if (length(lengths) == 1) {
    return(l)
  }
  cli::cli_abort(c(
    paste(
      "{.arg comparator}, {.arg include_low} and {.arg include_high}",
      "not of equal length."
    ),
    "x" = paste(
      "{.arg comparator}, {.arg include_low} and {.arg include_high}",
      "have lengths of {glue::glue_collapse(lengths, sep = ', ', last = ' and ')}"
    ),
    "i" = paste(
      "{.arg comparator}, {.arg include_low} and {.arg include_high}",
      "must all have the same lengths or a length of 1."
    )
  ), call = rlang::caller_env())
}

no_data_test <- function(x, msg, ...) {
  if (inherits(x, "harp_list")) {
    no_data <- all(vapply(x, function(x) nrow(x) < 1, logical(1)))
  } else {
    no_data <- nrow(x) < 1
  }

  if (no_data) {
    cli::cli_abort(msg, call = rlang::caller_env())
  }
}

add_thresh_type <- function(x, comp) {
  thresh_type <- switch(
    comp,
    "lt"      = ,
    "le"      = ,
    "ge"      = ,
    "gt"      = "thresholds",
    "eq"      = ,
    "between" = ,
    "outside" = "classes"
  )
  thresh_scores <- grep("threshold", names(x))
  if (length(thresh_scores) < 1) {
    return(x)
  }
  x[thresh_scores] <- lapply(x[thresh_scores], dplyr::mutate, Type = thresh_type)
  x
}

add_point_locs <- function(verif_attrs, data) {
  verif_attrs[["point_locations"]] <- dplyr::summarise(
    harpCore::common_cases(
      dplyr::select(
        data,
        dplyr::any_of(
          c("SID", "lat", "latitude", "lon", "long", "longitude",
            "elev", "elevation", "station_group", "model_elevation")
        )
      ),
      "SID", "lat", "latitude", "lon", "long", "longitude",
      "elev", "elevation", "station_group", "model_elevation",
      "-fcst_dttm", "-lead_time", "-p", "-z", "-m", "-h",
      rows_only = TRUE
    ),
    dplyr::across(dplyr::everything(), function(x) mean(x)),
    .by = dplyr::any_of(c("SID", "station_group"))
  )
  verif_attrs
}

sids_from_file <- function(file_name) {
  cli::cli_inform(paste(cli::col_br_yellow("Querying:"), "{file_name}"))
  db   <- DBI::dbConnect(RSQLite::SQLite(), file_name)
  fc   <- dplyr::tbl(db, "FC")
  sids <- dplyr::pull(dplyr::distinct(dplyr::select(fc, "SID")), "SID")
  DBI::dbDisconnect(db)
  sids
}
