
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

### Installing on ecgate

Before installing on ecgate you need to load the proj4 and R modules
before starting R and executing the above:

``` bash
module load proj4
module load R
```

ecgate has had an issue with the {*RSQLite*} package that provides
functionality for reading from and writing to SQLite files. If you
experience problems with RSQLite during installation, this can be
alleviated by installing an older version of {*RSQLite*} before
attempting to install {*harp*} using `remotes::install_version`:

``` r
remotes::install_version("RSQLite", "2.2.5")
```

You may be asked if you want to update versions of certain packages. In
general it is best to answer with the number that corresponds to All.
However, when {*harpIO*} begins to install, you need to input the
numbers (separated by spaces) for all packages excluding RSQLite.

### System libraries

The [{*harpIO*}](https://harphub.github.io/harpIO) and
[{*harpSpatial*}](https://harphub.github.io/harpSpatial) packages use
the [{*meteogrid*}](https://github.com/harphub/meteogrid) package for
interpolation and the geogrid class for storing gridded data. This
package makes use of the [PROJ4](https://proj4.org) library for handling
projections. If you do not have PROJ4 installed you can install with:

``` bash
sudo apt-get install libproj-dev
```

If you do not have sudo rights, try installing a user local version, or
speak to your system administrator.

If you have the PROJ4 libraries installed in a standard location
(e.g. /usr/local) {*meteogrid*} will install without problems. However,
if the PROJ4 libraries are in a non standard location, you need to tell
the install function where they are:

``` r
remotes::install_github(
  "harphub/harpIO",
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

Or you can set compiler and linker options in the file $HOME/.R/Makevars

``` bash
CPPFLAGS=-I/path/to/proj/include
LDFLAGS=-L/path/to/proj/lib -Wl,-rpath,/path/to/proj/lib
```

In this case you only have to set them once and not worry about it when
you wish to install an update to {*meteogrid*}.

When setting environment variables or creating a Makevars file, R must
be restarted for the changes to take effect before running
`remotes::install_github("harphub/harp")`.

## Workflows

The purpose of this website is to demonstrate common workflows, such as
point and spatial verification, score card generation, and data analysis
using the {*harp*} packages.

## harp book

An online [book](https://harphub.github.io/harp_tutorial) that collects
together tutorials on how to use harp is currently being worked on. Feel
free to make suggestions, or to contribute by filing issues or pull
requests [here](https://github.com/harphub/harp_tutorial)
