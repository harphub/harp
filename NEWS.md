# harp v0.2.0

* Major updates in all harp packages.

* Introduction of new package _{harpCore}_ which provides classes and 
functionalities that are common to all harp packages, including methods for 
geographic transformations via `geo_<.>` functions and fast neighbourhood 
smoothing of gridded fields via the `nbhd_smooth()` function.

* A lot of internal changes to make things run more smoothly.

## harpIO

### Breaking changes 

* Arguments `start_date`, `end_date`, and `by` to read functions are deprecated 
and replaced by `dttm`, to be used together with `seq_dttm()` to generate a 
sequence of date-time strings. Note that you can still use the old arguments, 
but will be periodically warned to change.

* New column names in outputs to read functions

|Old name|New name|
|:-------|:-------|
|fcdate|fcst_dttm|
|validdate|valid_dttm|
|leadtime|lead_time|

* `bind_fcst()` is deprecated. `bind()` should be used instead. 

* `get_filenames()` is deprecated. `generate_filenames()` should be used instead.

* `msub()` is deprecated. `psub()` should be used instead.

* `read_det_interpolate()` and `read_eps_interpolate()` are defunct. 
`read_forecast(..., transformation = "interpolate")` should be used instead. 

* `read_obs_convert()` is defunct. `read_obs(..., transformation = "...")` 
should be used instead.

### Selected new features

* Parameters are defined via the internal data list `harp_params`. This list 
includes parameter name substitutions for different file formats and the 
possibility to apply a function to specific parameters at read time. 

* The parameter list can be added to or modified with `add_param_def()` and 
`modify_param_def()` respectively. 

* All data read in by harp read functions attain a class - the print method 
for each class tells you what type of dataset it is. One exception is for 
`read_obs()` and `read_point_obs()`, which both return basic tibbles (data 
frames).

* `use_grib_stepRange()` is added as a helper to select grib messages via
`grib_opts(param_find = list(param = use_grib_stepRange(...)))`. This is useful 
for determining between accumulated and instantaneous variables with the same
grib shortName. `{lead_time}` can be used to take the current lead time.  

## harpPoint

### Breaking changes

* Default `groupings` argument changed from `"leadtime"` to `"lead_time"` for 
consistency with changes in _{harpIO}_. Scripts that use verification functions
with `"leadtime"`, `"validdate"` and `"fcdate"` as values to `groupings` should 
be changed to `"lead_time"`, `"valid_dttm"` and `"fcst_dttm"` respectively. 

* Verification outputs now use the column name `fcst_model` instead of `mname` 
so that there is consistency throughout harp. 

* Attributes for verification outputs have changed to include all forecast dates,
all stations and all groupings used in the verification. Any scripts that make 
use of these attributes should be updated to reflect the new attributes.

* `scale_point_forecast()` and `scale_point_obs()` are deprecated. 
`scale_param()` should be used instead. 

* `gather_members()` and `spread_members()` are deprecated. `pivot_members()`
should be used instead. 

* `first_validdate()` and `last_validdate()` are deprecated. 
`unique_valid_dttm()` should be used instead.

* `pull_stations()` is deprecated. `unique_stations()` should be used instead.

* `bootstrap_score()`, `pooled_bootstrap_score()` and `bind_bootstrap_score()` 
are defunct. `bootstrap_verify()` and `bind_point_verif()` should be used 
instead. 


### Selected new features

* Verification functions have gained new progress bars and are generally less 
verbose in what they are doing, restricting messages to only progress with 
computing scores for different verification groups. 

* New verification score __hexbin__. This gives a data frame of what is 
essentially a heat map of forecast - observation value pairs. 

* New class, attributes and print method for verification function outputs. 

* `jitter_fcst()` now accepts vectorized functions so should be a lot faster. 

## harpVis

### Breaking changes

* `plot_scatter()` is soft deprecated. Since the data are computed in 
verification functions, scatter (or more accurately hexbin) plots can now be 
made with `plot_point_verif(..., score = hexbin)`

* `plot_station_eps()` is defunct. `plot_station_ts()` should be used instead. 

### Selected new features

* Plot functions are backwards compatible so that verification data produced by 
old versions of _{harpPoint}_ with `leadtime`, `mname`, `fcdate`, `validdate` 
included in the column names will still work.

* `geom_georaster()`, `geom_geocontour()` and `geom_geocontour_filled()` are new 
functions for plotting gridded data using 
[ggplot](https://ggplot2.tidyverse.org/index.html). An upscaling option is added 
to enable faster plotting of rasters and calculation / smoothing of contours. 

* Map data for plots projected to the domain of a gridded data field can be 
retrieved using `get_map()`.

* The shiny app for point verification can now select from different time axes 
and will recognise and plot vertical profile verifications. 

* The shiny app for point verification has gained two new options: 
`full_dir_navigation` controls whether a modal is opened for selecting data 
directories (the old behaviour), or simply populating a dropdown selector. 
`theme` allows you to control the overall appearance of the app and you can 
choose between "dark", "light" and "white".



# harp v0.0.9

* This is the version that is basically unchanged since late 2021 / early 2022. 

* It was officially tagged v0.0.9 in November 2023
