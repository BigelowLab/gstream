
suppressPackageStartupMessages({
  library(gstream)
  library(dplyr)
})

#' Given a year, create an "orig" and "ordered" package from "raw" files
#' 
#' @param cfg list of config values
#' @param year char or num  of 4-digit year
#' @return the "ordered" sf object for the specified year
package_year = function(cfg = gstream::read_configuration(), 
                        year = format(Sys.Date(), "%Y")){
  year = as.character(year[1])
  files = list.files(file.path(cfg$usn$rawpath, year),
                     pattern = glob2rx("*.sub"), 
                     full.names = TRUE)  
 
  x = read_wall_data_usn(files) |>
    deduplicate_usn() |>
    sf::write_sf(gstream_path("usn", "orig", paste0(year, ".gpkg")))
  y = order_usn(x) |>
    sf::write_sf(gstream_path("usn", "ordered", paste0(year, ".gpkg")))
  
  y
}

package_year()
 