# ENMwrap
Ecological niche modeling tool that provides wrappers for existing functions from other ENM packages, along with some other useful and miscellaneous functions. Currently under development.

## Installation
```r
install.packages('devtools')

devtools::install_github('yucheols/ENMwrap')
```

## Software and package dependencies
This package relies heavily on existing ENM and spatial data processing packages. Please check the dependencies below and cite them appropriately.

*The package will be updated soon to remove raster and rgdal dependencies* 
- R (version 4.2.2 or higher; most recently tested with R version 4.4.2)
- ENMeval (version 2.0.5)
- dismo (version 1.3.14)
- SDMtune (version 1.3.1)
- ecospat (version 4.0.0)
- ENMTools (version 1.1.2)
- blockCV (version 3.0.0)
- sf (version 1.0.14)
- terra (version 1.7.65)
- raster (version 3.6.14)
- rgdal (1.6.4)
- MASS (version 7.3.58.2)
- dplyr (version 1.1.0)
- ggplot2 (version 3.5.1)
- rasterVis (version 0.51.5)
