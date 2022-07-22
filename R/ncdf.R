#' Retrieve the HYCOM time origin 
#' 
#' @export 
#' @param x ncdf4 object
#' @param tz character, the time zone to use
#' @return POSIXct time
hycom_t0 <- function(x, tz = 'UTC'){
  #units: hours since 2000-01-01 00:00:00
  as.POSIXct(x$dim$time$units, format = "hours since %Y-%m-%d %H:%M:%S", tz = tz)
}

#' Retrieve the HYCOM time vector 
#' 

#' @export 
#' @param x ncdf4 object
#' @return POSIXct time
hycom_time <- function(x){
  x$dim$time$vals * 3600 + hycom_t0(x)
}


#' Retrieve HYCOM spatial resolution
#' 
#' @seealso [HYCOM](https://www.hycom.org/dataserver/gofs-3pt1/analysis)
#' 
#' @export
#' @param x ncdf4 object
#' @return two element [x, y] resolution
hycom_res <- function(x){
  # TODO
  # this is fixed for recent (as of?) outputs, but earlier had low res poleward
  # and high res equator-ward  So we need to inquire within the NCDF object
  c(0.08, 0.04)
}

#' Retrieve HYCOM spatial resolution as a per element look up table
#' 
#' @seealso [HYCOM](https://www.hycom.org/dataserver/gofs-3pt1/analysis)
#' 
#' @export
#' @param x ncdf4 object
#' @return two element [x, y] list of resolution vectors
hycom_res_lut <- function(x){
  dlon <- round(diff(x$dim$lon$vals),2)
  dlon <- c(dlon, dlon[length(dlon)])
  dlat <- round(diff(x$dim$lat$vals),2)
  dlat <- c(dlat, dlat[length(dlat)])
  list(x = dlon, y = dlat)
}

#' Retrieve the names of variables
#' 
#' @export
#' @param x ncdf4 object
#' @param drop character, the variable to drop, set to 'none' to drop none.
#' @return character vector of variable names
hycom_vars <- function(x, drop = c("salinity_bottom", "water_temp_bottom", 
                                   'tau',
                                   'water_u_bottom', 'water_v_bottom')){
  nm <- names(x$var)
  nm[!(nm %in% drop)]
}

#' Retrieve a list of the dimensions for each variable
#' 
#' @export
#' @param x ncdf4 object
#' @return named list, one element per variable, identifying the dimensions
hycom_vardims <- function(x){
  sapply(names(x$var), 
         function(vname){
           sapply(X$var[[vname]]$dim, function(x) {x$name})
         }, simplify = FALSE)
}

#' Retrieve a list of the number of dimensions for each variable
#' 
#' @export
#' @param x ncdf4 object
#' @return named vector, one element per variable, identifying the number of dimensions
hycom_varndims <- function(x){
  sapply(names(x$var), 
         function(vname){
           X$var[[vname]]$ndims
         })
}


#' Retrieve hycom navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines point locations
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @param time numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the time dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @param lev numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the level dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @return data frame 
#' \itemize{
#'   \item{g the requested lonlats}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
hycom_nc_nav_point <- function(X, g,
                               res = hycom_res(X),
                               time = c(1, 1),
                               lev = c(1, -1),
                               varname = hycom_vars(X)){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])


  # accepts one row tibble (key is empty since we are rowwise)
  # X the ncdf4 objecy
  # returns tibble with start and count appended [x, y, level, time]
  locate_xyzt <- function(tbl, key, X = NULL){
    ix <- sapply(tbl[['X']],
                 function(x){
                   which.min(abs(X$dim$lon$vals - x))[1]
                 })
    iy <- sapply(tbl[['Y']],
                 function(y){
                   which.min(abs(X$dim$lat$vals - y))[1]
                 })
    iz <- sapply(tbl[['Z']],
                 function(z){
                   which.min(abs(X$dim$depth$vals - z))[1]
                 })
    it <- sapply(tbl[['M']],
                 function(z, t0 = NULL){
                   which.min(abs(hycom_time(X) - (z + t0)))[1]
                 }, t0 = xyzt::POSIX_epoch())
    
    start <-  unname(c(ix, iy, iz, it))
    count <- rep(1, length(start))
    tbl |>
      dplyr::mutate(start = list(start), count = list(count))
  }
  
  # accepts one row tibble (key is empty since we are rowwise)
  # X the ncdf4 objecy
  # returns tibble with start and count appended [x, y, time]
  locate_xyt <- function(tbl, key, X = NULL){
    ix <- sapply(tbl[[1]],
                 function(x){
                   which.min(abs(X$dim$lon$vals - x))[1]
                 })
    iy <- sapply(tbl[[2]],
                 function(y){
                   which.min(abs(X$dim$lat$vals - y))[1]
                 })
    iz <- sapply(tbl[[3]],
                 function(z, t0 = NULL){
                   which.min(abs(hycom_time(X) - (z + t0)))[1]
                 }, t0 = xyzt::POSIX_epoch())
    
    start <-  unname(c(ix,iy,iz))
    count <- rep(1, length(start))
    tbl |>
      dplyr::mutate(start = list(start), count = list(count))
  }
  
  
  d <- xyzt::get_geometry_dimension(g)
  nd <- nchar(d)
  if (nd == 3){
    FUN <- locate_xyt
  } else if(nd == 4){
    FUN <- locate_xyzt
  } else {
    stop("coordinates must be xyt or xyzt")
  }
  
  xy <- sf::st_coordinates(g) |>
    dplyr::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::group_map(FUN, X = X) |>
    dplyr::bind_rows() |>
    dplyr::mutate(varname = paste(varname, collapse = ",")) |>
    tidyr::separate_rows(.data$varname, sep = ",")
  
  xy
}


#' Retrieve hycom navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines a bounding box and possibly coded with time (XYM/XYZ)
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @param time numeric two elements time indexing \code{[start, length]}. Ignored if
#'   the geometry is coded with a 3rd dimension
#'   \code{start} is a 1-based index into the time dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @param lev numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the level dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @return list with
#' \itemize{
#'   \item{bb the requested bounding box}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
hycom_nc_nav_bb <- function(X, g,
                            res = hycom_res(X),
                            time = c(35000, 1),
                            lev = c(1, 1),
                            varname =  hycom_vars(X)[1]){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  half <- res/2
  
  bb <- sf::st_bbox(g) |> as.numeric()
  bb <- bb[c(1,3,2,4)]
  
  bb2 <- bb + c(-half[1], half[1], -half[2], half[2])
  ix <- sapply(bb2[1:2],
               function(xbb) which.min(abs(X$dim$lon$vals - xbb)))
  we <- X$dim$lon$vals[ix]
  iy <- sapply(bb2[3:4],
               function(ybb) which.min(abs(X$dim$lat$vals-ybb)))
  sn <- X$dim$lat$vals[iy]
  
  # if the polyon contains time then we override any user supplied values 
  # for time
  
  d <- get_geometry_dimension(g)
  nd <- nchar(d)
  if (nd < 3){
    stop("coordinates must be xyt or xyzt")
  } else if (nd == 3){
    xyz <- sf::st_coordinates(g)
    btimes <- hycom_time(X)
    time <- c(findInterval(xyz[1,3] + xyzt::POSIX_epoch(), btimes),1)
  } else {
    xyz <- sf::st_coordinates(g)
    btimes <- hycom_time(X)
    time <- c(findInterval(xyz[1,4] + xyzt::POSIX_epoch(), btimes),1)
    lev <- c(which.min(abs(X$dim$depth$vals - xyz[1,3])),1)
  }
  
  list(bb = bb,
       res = res,
       start = c(ix[1], iy[1], lev[1], time[1]),
       count = c(ix[2] - ix[1] + 1, iy[2] - iy[1] + 1, lev[2], time[2]),
       ext = c(we + (half[1] * c(-1,1)), sn + (half[2] * c(-1,1)) ),
       crs = 4326,
       varname = varname)
}
