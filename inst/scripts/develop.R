suppressPackageStartupMessages({
  library(ncdf4)
  library(dplyr)
  library(sf)
  library(stars)
  library(xyzt)
})
source("R/ncdf.R")
source("R/extract.R")



x_orig <- x <- xyzt::read_sab() |>
  dplyr::mutate(depth = abs(depth),
    lon = xyzt::to_360(lon)) |>
  xyzt::as_POINT(dim = 'xyzt')


g <- sf::st_geometry(x)


open_connection <- function(uri = 'https://tds.hycom.org/thredds/dodsC/GLBy0.08/expt_93.0/ts3z'){
  nc_open(uri)
}


var <- "water_temp"

X = open_connection(uri)

hycom_vardims(X)
# $tau
# [1] "time"
# 
# $water_temp
# [1] "lon"   "lat"   "depth" "time" 
# 
# $water_temp_bottom
# [1] "lon"  "lat"  "time"
# 
# $salinity
# [1] "lon"   "lat"   "depth" "time" 
# 
# $salinity_bottom
# [1] "lon"  "lat"  "time"

hycom_varndims(X)
# tau        water_temp water_temp_bottom          salinity   salinity_bottom 
# 1                 4                 3                 4                 3

pts_covar <- extract(x, X, varname = 'water_temp')



nc_close(X)
