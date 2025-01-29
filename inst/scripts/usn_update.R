# cur: load the current year of data
# new: load the new items from the current year
# add: cross-tab the dates selecting only new dates to add
# read add, append to cur, save to disk

suppressPackageStartupMessages({
  library(gstream)
  library(dplyr)
})


#' @param cfg list of config values
#' @param year char or num  of 4-digit year
#' @return the updated dataset for the specified year
update_year = function(cfg = gstream::read_configuration(), 
                       year = format(Sys.Date(), "%Y")){
  year = as.numeric(year)
  cur = read_usn(year = year)

  newfiles = list.files(cfg$usn$rawpath, pattern = glob2rx("*.sub"), full.names = TRUE)  
  newdates = gsub(".sub", "", basename(newfiles), fixed = TRUE) |> as.Date()
  enddate = as.Date(sprintf("%0.4i-12-31", as.numeric(year)), format = "%Y-%m-%d")

  if (nrow(cur) == 0){
    # Happy New Year!
    startdate = as.Date(sprintf("%0.4i-01-01", as.numeric(year)), format = "%Y-%m-%d")
  } else {
    # updating an existing year
    startdate = max(cur$date) + 1
  }
  
  newfiles = newfiles[newdates >= startdate & newdates <= enddate]
  newdata = lapply(newfiles, read_wall_data_usn) |>
    dplyr::bind_rows() |>
    order_usn() 
  
  if (nrow(cur) == 0){
    # Happy New Year!
    updated = newdata
  } else {
    # updating an existing year
    updated = dplyr::bind_rows(cur, newdata)
  }
  updated = updated |>
    deduplicate_usn() |>
    sf::write_sf(file.path(cfg$usn$datapath, "ordered", sprintf("%04.i.gpkg", year)))
  
}


