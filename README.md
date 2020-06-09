
<!-- README.md is generated from README.Rmd. Please edit that file -->

<style>
  body{
    text-align: justify;
  }
</style>

# harp <a href=#><img src='man/figures/harp_logo_dark.svg' align="right" height="131.5" style="margin-left:30px" /></a>

**harp** is a meta-package that attaches functionality from the
[harpIO](https://andrew-met.github.io/harpIO),
[harpPoint](https://andrew-met.github.io/harpPoint) and
[harpSpatial](https://andrew-met.github.io/harpSpatial) packages to your
session.

## Installation

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("andrew-MET/harp")
```

### System libraries

The [harpIO](https://andrew-met.github.io/harpIO) and
[harpSpatial](https://andrew-met.github.io/harpSpatial) packages use the
[meteogrid](https://github.com/adeckmyn/meteogrid) package for
interpolation and the geogrid class for storing gridded data. This
package makes use of the [PROJ4](https://proj4.org) library for handling
projections. If you do not have PROJ4 installed you can install with:

``` bash
sudo apt-get install libproj-dev
```

If you do not have sudo rights, speak to your system administrator.

If you have the PROJ libraries installed in a standard location
(e.g. /usr/local) meteogrid will install without problems. However, if
the PROJ libraries are in a non standard location, you need to tell the
install function where they are:

``` r
remotes::install_github(
  "andrew-MET/harpIO",
  configure.args = c(
    meteogrid = "--with-proj=/path/to/proj"
  )
)
```

Alternatively you can set environment variables

``` bash
export PROJ=/path/to/proj
```

If you include these environment variables in your .bashrc file, or
equivalent, you won’t need to worry about it when you wish to install an
update to meteogrid.

Or you can set compiler and linker options in the file $HOME/.R/Makevars

``` bash
CPPFLAGS=-I/path/to/proj/include
LDFLAGS=-L/path/to/proj/lib -Wl,-rpath,/path/to/proj/lib
```

In this case you only have to set them once and not worry about it when
you wish to install an update to meteogrid.

When setting environment variables or creating a Makevars file, R must
be restarted for the changes to take effect before running
`remotes::install_github("andrew_MET/harp")`.

## Workflows

The purpose of this website is to demonstrate common workflows, such as
point and spatial verification, score card generation, and data analysis
using the **harp** packages.
