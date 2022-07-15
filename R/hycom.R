#' Craft a HYCOM URL for a given date
#' 
#' @export
#' @param date character, POSIXt or Date the date to retrieve
#' @param root character, the root URL
#' @param product character the product to fetch
#' @param version character the version to fetch
#' @return one or more URLs
hycom_url <- function(date = Sys.Date() - 2,
                      product = "GLBv0.08",
                      version = "expt_93.0",
                      root = file.path("http://tds.hycom.org/thredds/catalog/datasets")){

      #"http://tds.hycom.org/thredds/dodsC/datasets/GLBv0.08/expt_57.7/data/hindcasts/2017/hycom_GLBv0.08_577_2017060112_t009.nc" 
  if (inherits(date, "character")) date <- as.Date(date)                    
  name <- sprintf("hycom_%s_%s_.nc",
                  substr(tolower(product), 1, 4),
                  as.character(as.numeric(substr(version, 6, 9)) * 10),
                  format(date, "%Y%m"))
  file.path(root, product[1], version[1], "data", "forecasts", name)                     
}