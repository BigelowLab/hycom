#' Craft a HYCOM URL for a given date
#' 
#' @seealso the \href{https://tds.hycom.org/thredds/catalog.html}{HYCOM THREDDS catalog}
#' @export
#' @param filename character, the name of the file. Filenames are
#'   coded to reflect the variables served.
#' @param root character, the root URL
#' @param product character the product to fetch
#' @param version character the version to fetch
#' @return one or more URLs
hycom_url <- function(filename = c("ts3z","uv3z", "ssh")[1],
                      product = "GLBy0.08",
                      version = "expt_93.0",
                      root = file.path("http://tds.hycom.org/thredds/dodsC")){

  #https://tds.hycom.org/thredds/dodsC/GLBy0.08/expt_93.0/ts3z
  file.path(root, product[1], version[1], filename)                     
}