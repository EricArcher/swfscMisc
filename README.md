[![Travis-CI Build Status](https://travis-ci.org/EricArcher/swfscMisc.svg?branch=master)](https://travis-ci.org/EricArcher/swfscMisc)

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

## Available functions:

You can see the most recent changes to the package in the NEWS.md file: https://github.com/EricArcher/swfscMisc/blob/master/swfscMisc/NEWS.md

* Geodesic related functions:
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
    * invLogOdds
    * invOdds
    * isBetween
    * logOdds
    * na.count
    * normalize
    * odds
    * one.arg
    * pVal
    * uniform.test
    * which.nearest
    * zero.pad
  
