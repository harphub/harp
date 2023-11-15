
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harp <a href=#><img src='man/figures/harp_logo_dark.svg' align="right" height="131.5" style="margin-left:30px" /></a>

{*harp*} is a meta-package that attaches functionality from the
[{*harpIO*}](https://harphub.github.io/harpIO),
[{*harpPoint*}](https://harphub.github.io/harpPoint),
[*{harpVis*}](https://harphub.github.io/harpVis) and
[{*harpSpatial*}](https://harphub.github.io/harpSpatial) packages to
your session.

## Installation

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("harphub/harp")
```

### System libraries

*harp* packages use the
[{*meteogrid*}](https://github.com/harphub/meteogrid) package for
interpolation and the geogrid class for storing gridded data. This
package makes use of the [PROJ](https://proj4.org) library for handling
projections. If you do not have PROJ installed you can install with:

``` bash
sudo apt-get install libproj-dev
```

If you do not have sudo rights, try installing a user local version, or
speak to your system administrator.

If you have the PROJ libraries installed in a standard location
(e.g. /usr/local) {*meteogrid*} will install without problems. However,
if the PROJ libraries are in a non standard location, you need to tell
the install function where they are:

``` r
remotes::install_github(
  "harphub/harp",
  configure.args = c(
    meteogrid = "--with-proj=/path/to/proj"
  )
)
```

Alternatively you can set environment variables

``` bash
export PROJ4_DIR=/path/to/proj
```

If you include these environment variables in your .bashrc file, or
equivalent, you won’t need to worry about it when you wish to install an
update to meteogrid.

Or you can set compiler and linker options in the file
\$HOME/.R/Makevars

``` bash
CPPFLAGS=-I/path/to/proj/include
LDFLAGS=-L/path/to/proj/lib -Wl,-rpath,/path/to/proj/lib
```

In this case you only have to set them once and not worry about it when
you wish to install an update to {*meteogrid*}.

When setting environment variables or creating a Makevars file, R must
be restarted for the changes to take effect before running
`remotes::install_github("harphub/harp")`.

## Learning harp

Currently the best learning resource for getting started with harp is
the [2022 training course
website](https://harphub.github.io/harp-training-2022/) that includes a
number of worked examples. There are plans to make an online book
available in 2024 that will go into harp’s functionalities in more
detail.
