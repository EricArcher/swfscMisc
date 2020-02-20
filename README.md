[![CRAN version](http://www.r-pkg.org/badges/version/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)  
[![Travis-CI Build Status](https://travis-ci.org/EricArcher/swfscMisc.svg?branch=master)](https://travis-ci.org/EricArcher/swfscMisc)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/swfscMisc?branch=master&svg=true)](https://ci.appveyor.com/project/EricArcher/swfscMisc)

# swfscMisc

## Description

*swfscMisc* is a collection of utility functions used at the Southwest Fisheries 
Science Center in La Jolla, CA. The package contains functions for geodesic 
calculations, commonly used  mapping functions, plotting special symbols, interacting 
with DAS sighting data generated by the line-transect survey data entry program WinCruz, and miscellaneous analytical and conversion functions.

## Installation

To install the stable version from CRAN:

```r
install.packages('swfscMisc')
```

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('ericarcher/swfscMisc')
```

## Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/swfscMisc/issues>
* send a pull request: <https://github.com/ericarcher/swfscMisc/>
* e-mail: <eric.archer@noaa.gov>

## Current functions

* Geodesic functions:
    * bearing
    * datum
    * distance
    * circle.polygon
    * convert.angle
    * convert.distance

* DAS file functions:
    * das.map
    * das.merge.acoustic
    * das.read
    * das.spp.freq

* Mapping functions:
    * lat.lon.axes
    * sample.map
  
* Plotting functions:
    * braces
    * color.name
    * lab.wid
    * row.col.page.fit
    * scatterdens
    * scatterhist
    * sex.symbols
  
* Miscellaneous functions:
    * affin.prop
    * box.area
    * central.quantile
    * copy.tri
    * crossing.point
    * diversity
    * fisher.method.p
    * geometric.mean
    * harmonic.mean
    * isBetween
    * na.count
    * normalize
    * odds conversion (odds, invOdds, logOdds, invLogOdds)
    * one.arg
    * plotAssignments
    * pVal
    * ceiling, floor, trunc, round (.data.frame)
    * uniform.test
    * setupClusters
    * which.nearest
    * zero.pad

## version 1.3 (devel)

* fixed plotAssignments to use facets and removed grids and space around panels
* added setupClusters

## version 1.2 (on CRAN)

* Added rounding functions for data.frames.
* Removed NEWS.md - changelog moved to README.md
* Added `plotAssignments` function.

## version 1.1

* Added `autoUnits` function
* Changed default arguments for `lat.range` and `lon.range` to `NULL` in `sample.map`. If not specified, the ranges will be set to the ranges of the `lat` and `lon`.

## version 1.0.9

* Added `transparent` function
* Changed distance and destination functions to accept partial matches for method 
of calculation, type of surface, and units

## version 1.0.8

* Fixed `das.read` to handle errors in position and suppress warnings about `NA`s
during numerical conversions.
* Fixed `das.map` to remove records with no position

## version 1.0.7

* Changed `isBetween` to accept a vector of numbers

## version 1.0.6

* Added NEWS.md
* Added `diversity` function (moved from strataG package)
* Added `isBetween` function to test if a number is between two numbers