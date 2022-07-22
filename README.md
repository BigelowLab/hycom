hycom
================

# hycom

Provides for access from R to [HYCOM](https://www.hycom.org/) online
datasets.

### [Citation](https://www.hycom.org/)

> Bleck, R., (2001). An oceanic general circulation model framed in
> hybrid isopycnic-Cartesian coordinates. Ocean Modeling, 4: 55-88.

### Requirements

From CRAN…

-   [R v4+](https://www.r-project.org/)
-   [rlang](https://CRAN.R-project.org/package=rlang)
-   [dplyr](https://CRAN.R-project.org/package=dplyr)
-   [sf](https://CRAN.R-project.org/package=sf)
-   [stars](https://CRAN.R-project.org/package=stars)

From github…

-   [xyzt](https://github.com/BigelowLab/xyzt)

### Installation

    remotes::install_github("BigelowLab/hycom")

### Getting started

``` r
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(hycom)
  library(xyzt)
  library(stars)
})
```

#### Working with points.

See the [xyzt](https://github.com/BigelowLab/xyzt) package for more
details on the example Southern US Atlantic Bight data.

``` r
# read in example SAB points
x <- xyzt::read_sab() |>
  dplyr::mutate(lon = xyzt::to_360(lon),  # HYCOM lons are [0,360]
                depth = abs(depth)) |>    # HYCOM depths are positive downward
  xyzt::as_POINT(dims = 'xyzt')

# generate a hycom url for a dataset
url <- hycom_url(filename = "ts3z")

# open the resource
X <- ncdf4::nc_open(url)

if (FALSE){
  g <- sf::st_geometry(x)
  y <- X
  res = hycom_res(X)
  verbose = TRUE
  varname = "water_temp"
}
# extract the data 
covars <- hycom::extract(x, X, varname = c("salinity", "water_temp"))


# bind to the input
(y <- dplyr::bind_cols(x, covars))
```

    ## Simple feature collection with 5 features and 4 fields
    ## Geometry type: POINT
    ## Dimension:     XYZM
    ## Bounding box:  xmin: 279.815 ymin: 28.508 xmax: 287.752 ymax: 34.714
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 5 × 5
    ##   id    name                                     geometry salinity water_temp
    ## * <chr> <chr>                                 <POINT [°]>    <dbl>      <dbl>
    ## 1 41009 Canveral       ZM (279.815 28.508 1.5 1655489106)     36.2       28.8
    ## 2 41010 Canaveral East ZM (281.515 28.878 1.5 1656525906)     36.4       28.1
    ## 3 41002 South Hatteras ZM (285.064 31.759 1.5 1654452306)     36.2       27.3
    ## 4 41001 East Hatteras    ZM (287.752 34.714 2 1651341906)     36.5       21.2
    ## 5 41004 EDISTO         ZM (280.901 32.502 1.5 1657562706)     35.3       29.0

#### Working with bounding boxes (from points or polygons).

Learn more about working with
[stars](https://CRAN.R-project.org/package=stars) objects in the
[vignettes](https://r-spatial.github.io/stars/).

    # read in example SAB points
    x <- xyzt::read_sab() |>
      dplyr::mutate(lon = xyzt::to_360(lon),
                    depth = abs(depth)) |>    
      xyzt::as_BBOX(dim = 'xyzt')

    (covars <- hycom::extract(x, X, varnames = hycom::hycom_vars(X)))

Now let’s see what it looks like.

    x <- xyzt::read_sab() |>
      dplyr::select(-time, -depth) |>
      dplyr::mutate(lon = xyzt::to_360(lon)) |>
      xyzt::as_POINT()
    par(mfrow = c(1,2))
    plot(covars, attr = 'sst', axes = TRUE, reset = FALSE)
    plot(sf::st_geometry(x), add = TRUE, col = "orange", pch = 19, cex = 2)

``` r
# cleanup
ncdf4::nc_close(X)
```
