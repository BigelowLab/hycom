#' Convert [0,360] longitudes to [-180, 180]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being withing [0, 360] range
#' @return numeric vector
to_180 <- function(x) { ((x + 180) %% 360) - 180 }

#' Convert [-180,180] longitudes to [0, 360]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being within [0,3 60] range
#' @return numeric vector
to_360 <- function(x) {x %% 360}


#' Get geometry dimension code
#' 
#' @export
#' @param x sf or sfc object
#' @param recursive logical, if TRUE drill down to get the type for each
#'   feature.
#' @return character vector such as "XY" or "XYZ"
get_geometry_dimension <- function(x, recursive = FALSE){
  if (recursive[1]){
    x <- sf::st_geometry(x)
    d <- sapply(x,
                function(x) {
                  sort(unique(sapply(x, function(x) class(x)[1])))
                })
  } else {
    x <- sf::st_geometry(x)
    d <- sort(unique(sapply(x, function(x) class(x)[1])))
  }
  d
}

#' Get geometry type code
#' 
#' @export
#' @param x sf or sfc object
#' @param recursive logical, if TRUE drill down to get the type for each
#'   feature.
#' @return character vector such as "POINT" or "POLYGON"
get_geometry_type <- function(x, recursive = FALSE){
  if (recursive[1]){
    klass <- sapply(sf::st_geometry(x), class)
  } else {
    klass <- sf::st_geometry(x) |>
      class()
  }
  sub("sfc_", "", klass[1])
}

#' extract generic
#'
#' @export
#' @param x \code{sf} object
#' @param y \code{ncdf4} object
#' @param ... Arguments passed to or from other methods
#' @return data frame of covariates (point, or raster)
extract <- function(x, ...) {
  UseMethod("extract")
}

#' @export
#' @param x \code{sf} object
#' @param y \code{ncdf4} object
#' @describeIn extract Extract data from a NCDF4 object
extract.default <- function(x, y = NULL, ...){
  stop("class not known:", paste(class(x), collapse = ", "))
}

#' @export
#' @param x \code{bbox} object that defines a bounding box
#' @param y \code{ncdf4} object 
#' @param varname character one or more variable names
#' @param flip char one of "y", "x" or "none" to flip the raster
#' @return stars object (one variable per covariate)
#' @describeIn extract Extract from a NCDF4 object using any sf object
extract.bbox <- function(x, y = NULL, varname = hycom_vars(y)[1], flip = "y", 
                         time = hycom_most_recent_time(y),
                         depth = 0, ...){
  
  ntime = length(time)
  ndepth = length(depth)
  if (ntime > 1) {
    if (ndepth > 1) stop("if time has more than one value than depth can have only one")
  }
  
  #if (length(time) > 1) warning("only the first requested time is used")
  #if (length(depth) > 1) warning("only the first requested depth is used")
    
  x = sf::st_as_sfc(x)
  
  DEPTH = y$dim$depth$vals
  idepth <- findInterval(depth, DEPTH)
  TIME = hycom_time(y)
  itime <- findInterval(time, TIME)
  
  # for each variable
  #   for each time or depth
  # bind attributes
  rr = lapply(varname,
             function(v, id = 1, it = 1){
               if (length(id) > 1){
                 r = lapply(idepth,
                            function(dep, ...){
                              extract(x, y = y, 
                                   varname = v, 
                                   flip = flip, 
                                   time = c(it,1), 
                                   lev = c(dep,1),
                                   ...)
                            }, ...) 
                 r = do.call(c, append(r, list(along = list(depth = DEPTH[idepth]))))
                   
               } else if(length(it) > 1){
                 r = lapply(it,
                            function(tim, ...){
                              extract(x, y = y, 
                                      varname = v, 
                                      flip = flip, 
                                      time = c(tim,1), 
                                      lev = c(id,1),
                                      ...)
                            }, ...) 
                 r = do.call(c, append(r, list(along = list(time= TIME[itime]))))
               } else {
                 r = extract(x, y = y, 
                          varname = v, 
                          flip = flip, 
                          time = c(it,1), 
                          lev = c(id,1),
                          ...)
               }
              r
        }, id = idepth, it = itime, ...)
    Reduce(c, rr)      
}

#' @export
#' @param x \code{sf} object with POSIX time encoded in the coordinates
#' @param y \code{ncdf4} object
#' @param varname character, one or more variable names
#' @param verbose logical, output helpful messages?
#' @describeIn extract Extract from a NCDF4 object using any sf object
extract.sf <- function(x, y = NULL, 
                       varname = hycom_vars(y),
                       verbose = FALSE, 
                       ...){
  
  typ <- get_geometry_type(x)
  if (verbose[1]) {
    cat("extract.sf type: ", typ, "\n" )
    cat("  varname:", paste(varname, collapse = ", "), "\n")
  }
  switch(typ,
         "POINT" = {
           g <- sf::st_geometry(x)
           d <- get_geometry_dimension(g)
           # this isn't right... we need to test for each variable name the number
           # of required dims... This means a user could get water_temp[x,y,z,t] and
           # bottom_water_temp[x,y,t] in a single call.  Hmmm.
           if ((nchar(d) < 3)) stop("time must be included in XYZT or XYZM coordinates")
           r <- extract(g, y = y, varname = varname, verbose = verbose, ...)
         },
         "BBOX" = {
           g <- sf::st_as_sfc(x) |> sf::st_geometry(x)
           ss <- lapply(varname,
                        function(varnm,g = NULL, y = NULL, ...) {
                          extract(g, y = y, varname = varnm, verbose = verbose, ...)
                        }, g = g, y = y, ...)
           r <- Reduce(c, ss)
         },
         "POLYGON" = {
           
           g <- sf::st_geometry(x)
           ss <- lapply(varname,
                        function(varnm,g = NULL, y = NULL, ...) {
                          extract(g, y = y, varname = varnm, verbose = verbose, ...)
                        }, g = g, y = y, ...)
           r <- Reduce(c, ss)
         }
  )
  r
}

#' @export
#' @param x \code{sfc} object
#' @param y \code{ncdf4} object
#' @param varname character, one or more variable names
#' @param verbose logical, output helpful messages?
#' @return tibble of extracted values (one variable per covariate)
#' @describeIn extract Extract data from a NCDF4 object using sf POINT object
extract.sfc_POINT <- function(x, y = NULL, 
                              varname = hycom_vars(y),
                              verbose = FALSE, 
                              ...){
  if (verbose[1]) {
    cat("extract.sfc_POINT\n" )
    cat("  varname:", paste(varname, collapse = ", "), "\n")
  }
  
  # Extract points for a given variable
  # 
  # @param tbl table of navigation info, see \code{\link{hycom_nc_nav_point}}
  # @param key table of variable name
  # @param X \code{ncdf4} object
  # @return table of variable values
  .extract_point <- function(tbl, key, X = NULL){
    
    varname <- key$varname[1]
    x <- tbl$data[[1]]
    v <- sapply(seq_len(nrow(x)), 
                function(i){
                  ncdf4::ncvar_get(X, varid = varname,
                                   start = x$start[[i]], count = x$count[[i]])
                })
    dplyr::tibble(!!varname := v)
  }
  
  
  
  nav <- hycom_nc_nav_point(y, x, varname = varname, ...)
  xx <- nav |>
    dplyr::nest_by(.data$varname) |>
    dplyr::group_map(.extract_point, X = y) |> 
    dplyr::bind_cols()
}


#' @export
#' @param x \code{sfc} object that defines a bounding box
#' @param y \code{ncdf4} object 
#' @param varname character one or more variable names
#' @param verbose logical, output helpful messages?
#' @param flip character, one of 'x', 'y' or 'none'
#' @return stars object (one variable per covariate)
#' @describeIn extract Extract data from a NCDF4 object using sf POLYGON object
extract.sfc_POLYGON <- function(x, y = NULL, 
                                varname = hycom_vars(y)[1], 
                                verbose = FALSE,
                                flip = "y",
                                ...){
  
  if (verbose[1]) {
    cat("extract.sfc_POLYGON\n" )
    cat("  varname:", paste(varname, collapse = ", "), "\n")
  }
  
  nav <- hycom_nc_nav_bb(y, x, varname = varname, ...)
  m <- ncdf4::ncvar_get(y, varid = varname,
                        start = nav$start, count = nav$count)
  stars::st_as_stars(sf::st_bbox(x), 
                     nx = nav$count[1],
                     ny = nav$count[2],
                     values = m ) |>
    stars::st_flip(flip) |>
    rlang::set_names(varname)
}


