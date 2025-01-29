#' List the raw data files and decode the years
#' for each file
#'   read the tar file
#'   save as orig
#'   order and orient eastward
#'   save as ordered
#'   

stop("this is a one-and-done script to seed the database - see usn_update.R script")
suppressPackageStartupMessages({
  library(gstream)
})


cfg = read_configuration()

rawfiles = list.files(file.path(cfg$usn$datapath, "archived-tar"),
                      pattern = glob2rx("*.tar"),
                      full.names = TRUE)


names(rawfiles) <- years <- substring(basename(rawfiles), 8, 11)

xx = lapply(rawfiles, read_wall_data_usn)


ok = lapply(names(xx),
            function(year){
              sf::write_sf(xx[[year]],
                           file.path(file.path(cfg$usn$datapath, "orig",
                                               sprintf("%s.gpkg", year))))
            })

xx = lapply(xx, order_usn)

ok = lapply(names(xx),
            function(year){
              sf::write_sf(xx[[year]],
                           file.path(file.path(cfg$usn$datapath, "ordered",
                                               sprintf("%s.gpkg", year))))
            })


