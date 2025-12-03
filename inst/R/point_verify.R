script_version <- "1.0"

### Script opts

library(docopt)

doc <- "
  harp point verification

  Usage:
    harp_point_verify.R <fcst_model> [options]
    harp_point_verify.R (-h | --help)
    harp_point_verify.R (-v | --version)

  Options:
    -h --help                                             Show this screen.
    -v --version                                          Show version.
    -s START_DTTM, --start_dttm=START_DTTM                Start date-time string. Will be set to 00 UTC 3 days ago if missing.
    -e END_DTTM, --end_dttm=END_DTTM                      Will be set to START_DTTM if missing.
    -b BY, --by=BY                                        Time between forecasts. Should be a number followed by either s, m, h or d for seconds, minutes, hours and days respectively. Set to 1d if missing.
    -l LEAD_TIME, --lead_time=LEAD_TIME                   Lead time sequence. Should be 3 comma separated values of start,end,by. Set to 0,48,3 if missing.
    -c CONFIG_FLIE, --config_file=CONFIG_FILE             Configuration file.
    -o OUT_SUBDIR, --out_subdir=OUT_SUBDIR                Sub directory for outputs under the point_verif directory in the project directory.
    -m MAIN_FCST_MODEL, --main_fcst_model=MAIN_FCST_MODEL The fcst_model to treat as the main fcst_model
"

args <- docopt(
  doc, version = paste("harp Point Verification", script_version, "\n")
)
print(args)

###################

### Handle options

handle_config_file <- function(config_file) {
  config <- harp::read_config(config_file)

  if (!is.null(names(config$param_defs)) && names(config$param_defs) == "file") {
    config$param_defs <- harp::read_verif_params(config$param_defs)
  }

  if (config$defaults == "param_defs") {
    config$defaults <- config$param_defs$defaults
  }

  if (
    !is.null(names(config$param_defs)) && is.element("params", names(config$param_defs))
  ) {
    config$param_defs <- config$param_defs$params
  }

  if (!is.null(names(config$stations)) && names(config$stations) == "file") {
    config$stations <- unique(read.csv(config$stations)$SID)
  }

  if (!is.null(names(config$station_groups)) &&
      names(config$station_groups) == "file"
  ) {
    config$station_groups <- read.csv(config$station_groups)
  }

  if (is.null(config$lead_time_unit)) {
    config$lead_time_unit <- "h"
  }

  if (length(intersect(config$lead_time_unit, c("s", "m", "h", "d"))) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg lead_time_unit} in config file.",
      "x" = "You supplied {.arg lead_time_unit} = {config$lead_time_unit}.",
      "i" = "{.arg lead_time_unit} must be one of {.or {c('s', 'm', 'h', 'd')}}."
    ))
  }
  config$lead_time <- paste0(config$lead_time, config$lead_time_unit)
  config$lead_time_unit <- NULL
  config
}


fcst_model <- gsub("^\\s*|\\s*$", "", strsplit(args$fcst_model, ",")[[1]])

# Use 00 3 days ago if no start_dttm
if (is.null(args$start_dttm)) {
  start_dttm <- harpCore::as_str_dttm(round(Sys.time() - as.difftime(3, unit = "days"), "days"))
} else {
  start_dttm <- args$start_dttm
}

# If no end_dttm, use start_dttm
if (is.null(args$end_dttm)) {
  end_dttm <- start_dttm
} else {
  end_dttm <- args$end_dttm
}

# If no by, set to 1 day
if (is.null(args$by)) {
  by <- "1d"
} else {
  by <- args$by
}

config <- handle_config_file(args$config_file)

lead_args <- TRUE
lead_time <- args$lead_time
# If lead time not specified in args, get from config
if (is.null(lead_time)) {
  lead_time <- config$lead_time
  lead_args <- FALSE
}

config <- config[!names(config) == "lead_time"]

# If still no lead_time set to seq(0, 48, 3)
if (is.null(lead_time)) {
  lead_args <- TRUE
  lead_time <- "0,48,3"
}

if (lead_args) {

  lt <- strsplit(lead_time, ",")[[1]]

  if (length(lt) > 3) {
    lt_unit <- lt[4]
  } else {
    lt_unit <- "h"
  }


  if (length(lt) < 3) {
    cli::cli_abort(c(
      "Invalid lead time specification",
      "i" = "Lead time must be 3 numeric comma separated values",
      "x" = "You supplied {args$lead_time}"
    ))
  }

  lt <- suppressWarnings(as.numeric(lt[1:3]))
  if (any(is.na(lt))) {
    cli::cli_abort(c(
      "Non numeric values found in lead time"
    ))
  }

  lt_func <- switch(
    lt_unit,
    "s" = harpCore::seq_secs,
    "m" = harpCore::seq_mins,
    "h" = harpCore::seq_hours,
    "d" = harpCore::seq_days,
    NA
  )

  if (!is.function(lt_func)) {
    cli::cli_abort(c(
      "Invalid lead time unit specification",
      "i" = "Must be one of {.or {c('s', 'm', 'h', 'd')}}."
    ))
  }

  lead_time <- lt_func(lt[1], lt[2], lt[3])
}


### Do the verification

if (is.null(config$lags)) {
  config$lags <- "0s"
}

if (!is.null(args$out_subdir)) {
  config$out_path <- file.path(config$out_path, args$out_subdir)
}

do.call(
  harp::run_point_verif,
  c(
    list(dttm        = harpCore::seq_dttm(start_dttm, end_dttm, by)),
    list(fcst_model  = fcst_model),
    list(lead_time   = lead_time),
    config,
    list(main_fcst_model = args$main_fcst_model),
    list(return_data     = FALSE)
  )
)

