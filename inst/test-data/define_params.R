library(harp)

params <- c(
  make_verif_param(
    "10m Wind Speed",
    fcst_param          = "S10m", 
    obs_param           = "S10m", 
    obs_min             = 0, 
    obs_max             = 100, 
    obs_error_sd        = 6, 
    verif_thresholds    = list(c(1.5, 3.3, 5.5, 8, 10.8, 13.9, 24.5), c(0, 1.5, 3.3, 5.5, 8, 10.8, 13.9, 24.5, Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE
  ),
  make_verif_param(
    "10m Wind Direction",
    fcst_param          = "D10m", 
    obs_param           = "D10m", 
    obs_min             = 0, 
    obs_max             = 360, 
    verif_thresholds    = list(seq(22.5, 337.5, 45), c(22.5, 337.5)), 
    verif_comparator    = c("between", "outside"),
    verif_comp_inc_low  = c(TRUE, FALSE),
    verif_comp_inc_high = c(FALSE, TRUE),
    verif_circle        = 360
  ),
  make_verif_param(
    "2m Temperature",
    fcst_param          = "T2m",
    fcst_scaling        = make_scaling(-273.15, "degC"), 
    obs_param           = "T2m",
    obs_scaling         = make_scaling(-273.15, "degC"),
    obs_min             = 223, 
    obs_max             = 333, 
    obs_error_sd        = 6, 
    verif_thresholds    = list(c(-30, seq(-20, 30, 5)), c(-Inf, -30, seq(-20, 30, 5), Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE
  ),
  make_verif_param(
    "2m Dewpoint",
    fcst_param       = "Td2m",
    fcst_scaling     = make_scaling(-273.15, "degC"), 
    obs_param        = "Td2m",
    obs_scaling      = make_scaling(-273.15, "degC"),
    obs_min          = 223, 
    obs_max          = 333, 
    obs_error_sd     = 6
  ),
  make_verif_param(
    "2m Relative Humidity",
    fcst_param          = "RH2m",
    obs_param           = "RH2m",
    obs_min             = 0, 
    obs_max             = 101, 
    obs_error_sd        = 6, 
    verif_thresholds    = list(c(30, 50, seq(65, 95, 10), 100), c(0, 30, 50, seq(65, 95, 10), 100, Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE
  ),
  make_verif_param(
    "2m Specific Humidity",
    fcst_param       = "Q2m",
    fcst_scaling     = make_scaling(1000, "g/kg", TRUE), 
    obs_param        = "Q2m",
    obs_scaling      = make_scaling(1000, "g/kg", TRUE),
    obs_min          = 0, 
    obs_max          = 50 / 1000, 
    obs_error_sd     = 6
  ),
  make_verif_param(
    "Total Cloud Cover",
    fcst_param          = "CCtot",
    obs_param           = "CCtot",
    obs_min             = 0, 
    obs_max             = 8, 
    verif_thresholds    = list(seq(1, 8), seq(0, 8)),
    verif_comparator    = c("ge", "eq"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE 
  ),
  make_verif_param(
    "Mean Sea Level Pressure",
    fcst_param          = "Pmsl",
    obs_param           = "Pmsl",
    obs_min             = 900, 
    obs_max             = 1100, 
    verif_thresholds    = list(seq(980, 1010, 5), c(-Inf, seq(980, 1010, 5), Inf)),
    verif_comparator    = c("le", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE 
  ),
  make_verif_param(
    "6h Precipitation",
    fcst_param          = "AccPcp6h",
    obs_param           = "AccPcp6h",
    obs_min             = 0, 
    obs_max             = 750, 
    verif_thresholds    = list(c(0.1, seq_double(0.5, 7)), c(0, 0.1, seq_double(0.5, 7), Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE 
  ),
  make_verif_param(
    "12h Precipitation",
    fcst_param          = "AccPcp12h",
    obs_param           = "AccPcp12h",
    obs_min             = 0, 
    obs_max             = 1000, 
    verif_thresholds    = list(c(0.1, seq_double(0.5, 8)), c(0, 0.1, seq_double(0.5, 8), Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE 
  ),
  make_verif_param(
    "24h Precipitation",
    fcst_param          = "AccPcp24h",
    obs_param           = "AccPcp24h",
    obs_min             = 0, 
    obs_max             = 1000, 
    verif_thresholds    = list(c(0.1, seq_double(0.5, 9)), c(0, 0.1, seq_double(0.5, 9), Inf)),
    verif_comparator    = c("ge", "between"), 
    verif_comp_inc_low  = TRUE,
    verif_comp_inc_high = FALSE 
  ),
  make_verif_param(
    "Upper Air Temperature",
    fcst_param          = "T",
    fcst_scaling        = make_scaling(-273.15, "degC"), 
    obs_param           = "T",
    obs_scaling         = make_scaling(-273.15, "degC"),
    obs_min             = 173, 
    obs_max             = 333, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Dewpoint",
    fcst_param          = "Td",
    fcst_scaling        = make_scaling(-273.15, "degC"), 
    obs_param           = "Td",
    obs_scaling         = make_scaling(-273.15, "degC"),
    obs_min             = 173, 
    obs_max             = 333, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Wind Speed",
    fcst_param          = "S",
    obs_param           = "S",
    obs_min             = 0, 
    obs_max             = 150, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Wind Direction",
    fcst_param          = "D",
    obs_param           = "D",
    obs_min             = 0, 
    obs_max             = 360,
    verif_circle        = 360, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Geopotential Height",
    fcst_param          = "Z",
    obs_param           = "Z",
    obs_min             = 0, 
    obs_max             = 50000, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Relative Humidity",
    fcst_param          = "RH",
    obs_param           = "RH",
    obs_min             = 0, 
    obs_max             = 100, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  ),
  make_verif_param(
    "Upper Air Specific Humidity",
    fcst_param          = "Q",
    fcst_scaling        = make_scaling(1000, "g/kg", TRUE), 
    obs_param           = "Q",
    obs_scaling         = make_scaling(1000, "g/kg", TRUE),
    obs_min             = 0, 
    obs_max             = 50 / 1000, 
    vertical_coordinate = "pressure",
    verif_groups        = c(list(NULL), make_verif_groups(c("lead_time", "valid_hour", "valid_dttm"), "fcst_cycle"))
  )
)

defaults <- make_verif_defaults(
  make_verif_groups(
    c("lead_time", "valid_dttm", "valid_hour"), 
    c("fcst_cycle", "station_group")
  )
)