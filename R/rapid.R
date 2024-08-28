#' Read the moc_transport data file
#' 
#' @export
#' @param filename the name of the file to read
#' @return a tibble of class "rapid_moc_transports"
read_moc_transports = function(
    filename = gstream_path("rapid-amoc/moc_transports.nc")){
  
  on.exit({
    ncdf4::nc_close(nc)
  })
  
  nc = ncdf4::nc_open(filename)
  date = as.Date("2004-01-01") + as.vector(nc$dim$time$vals)
  
  x = sapply(names(nc$var),
             function(vname) {
               ncdf4::ncvar_get(nc, vname, start = 1, count = length(date)) |>
                 as.vector()
             },
             simplify = FALSE) |>
  dplyr::as_tibble() |>
  dplyr::mutate(date = date, 
                .before = 1)
  
  u = sapply(names(nc$var),
             function(vname) {
                nc$var[[vname]]$units
             },
             simplify = FALSE) |>
    unname()
  
  longnames = readr::read_csv(system.file("rapid-amoc/moc_transports_lut.csv", package = "gstream"),
                              col_types = "cc")

  attr(x, "longnames") <- longnames
  attr(x, "units") <- u
  class(x) <- c("rapid_moc_transports", class(x))
  x
}

#' Plot the moc_transports data set
#' 
#' @export
#' @param x moc_transports data frame (class of "rapid_moc_transports")
#' @param smooth logical, if TRUE add a smoothing line (for timeseries only)
#' @param by chr, one of 'none' (default timeseries), 'month' (ghost plot by month over the years)
#'  or 'year' a box plot by year
#' @return ggplot2 object
plot.rapid_moc_transports = function(x = read_moc_transports(),
                                     smooth = TRUE, 
                                     by = c("none", "month", "year")[1]){
  lutable = attr(x, "longnames")
  lut = lutable$longname
  names(lut) = lutable$name
  # transform from wide to long
  x = tidyr::pivot_longer(x,
                          !dplyr::any_of("date"),
                          names_to = "name",
                          values_to = "value") |>
    dplyr::mutate(name = factor(lut[.data$name], levels = lutable$longname))
  
  gg = ggplot2::ggplot(data = x, 
                  mapping = ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line()
  if (smooth) gg = gg + ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
  
  gg + ggplot2::facet_wrap(~name, scales = "free_y", ncol = 2)
  
}


#' Retrieve the variable names that depend only upon time
#' 
#' @param x ncdf4 object
#' @param dim the name of the dim that contains time
#' @return character vector of vars that deoend upon time only
mocha_time_only_vars = function(x, dim = "time"){
  vars = names(x$var)
  ix = sapply(vars,
              function(vname, test = "time"){
                dnames = sapply(x$var[[vname]]$dim, function(x) x$name)
                identical(test, dnames)
              }, test = dim)
  names(ix)[ix]
}

#' Retrieve the variable time
#' 
#' @param x ncdf4 object
#' @return POSIXct time
mocha_get_time = function(x){
  origin = as.POSIXct(x$dim$time$units, 
                      format = "seconds since %Y-%m-%d %H:%M:%OS", tz = 'UTC')
  as.vector(x$dim$time$vals) + origin
}

#' Get the metadata  for variables
#' 
#' @param x ncdf4 object
#' @param vars chr, one or more variable names
#' @return tibble of metadata for variable
mocha_get_metadata = function(x, vars){
  
  r = sapply(vars,
             function(vname){
               c(name = x$var[[vname]]$name,
                 units = x$var[[vname]]$units,
                 longname = x$var[[vname]]$longname)
              }) |>
    t() |>
    dplyr::as_tibble()
  dplyr::bind_rows(
    dplyr::tibble(name = "time", units = "", longname = "POSIX time UTC"),
    r)
}


#' Read RAPID-MOCHA data
#' 
#' @export
#' @param filename 
#' @param by chr, one of 'time' (default) or 'depth'
#' @return for \code{by = 'time'} then a tibble of data.  The table has an
#' added attribute called 'meta' that contains the column units and long name. 
#' It will have the class "rapid_mocha_time"
read_rapid_mocha = function(filename = gstream_path("rapid-mocha/mocha_mht_data_ERA5_v2020.nc"),
                            by = 'time'){

  on.exit({
    ncdf4::nc_close(nc)
  })
    
  get_time = function(x){
    origin = as.POSIXct(x$dim$time$units, 
                        format = "seconds since %Y-%m-%d %H:%M:%OS", tz = 'UTC')
    time = as.vector(x$dim$time$vals) + origin
  }
  
  
  nc = ncdf4::nc_open(filename)
  
  by = tolower(by[1])
  
  if (by == "depth"){
    
  } else {  
    
    vars = mocha_time_only_vars(nc)
    meta = mocha_get_metadata(nc, vars)
    
    x = sapply(vars,
               function(vname){
                 as.vector(ncdf4::ncvar_get(nc, varid = vname))
               }, simplify = FALSE) |>
      dplyr::as_tibble() |>
      dplyr::mutate(time = get_time(nc), .before = 1)
    
    attr(x, "mocha") <- meta
    class(x) <- c("rapid_mocha_time", class(x))
  }
  x
}

#' Plot RAPID-MOCHA data time series
#' 
#' @export
#' @param x the mocha table
#' @param smooth logical, if TRUE add a smoothing line (for timeseries only)
#' @param by chr, one of 'none' (default timeseries), 'day' (ghost plot by day over the years)
#'  or 'year' a box plot by year
#'#' @return a ggplot2 object
plot.rapid_mocha_time = function(x = read_rapid_mocha(), 
                                 smooth = TRUE, 
                                 by = c("none", "day", "year")){
  
  by = tolower(by[1])
  
  if (by == "day"){
    xx = x |>
      dplyr::select(c("time", dplyr::starts_with("Q"))) |>
      tidyr::pivot_longer(cols = dplyr::starts_with("Q")) |>
      dplyr::mutate(year = as.numeric(format(.data$time, "%Y")),
                    doy = as.numeric(format(.data$time, "%j")))
    yy = dplyr::slice_max(xx, year)
    
    month_format = function(x){ month.abb[x] }
    
    gg = ggplot2::ggplot(data = xx,
                         mapping = ggplot2::aes(x = doy, y = value, group = year)) +
      ggplot2::geom_line(color = "grey", alpha = 0.5) +
      ggplot2::geom_line(data = yy,
                         mapping = ggplot2::aes(x = doy, y = value),
                         color = "black") +
      ggplot2::labs(x = "Month", 
                    y = "Heat transport (W)", 
                    caption = "data source: https://mocha.earth.miami.edu/mocha/data/index.html") +
      ggplot2::facet_wrap(~name, scales = "free_y")
    
    
  } else if (by == "year"){
    
    xx = x |>
      dplyr::select(c("time", dplyr::starts_with("Q"))) |>
      tidyr::pivot_longer(cols = dplyr::starts_with("Q")) |>
      dplyr::mutate(year = as.numeric(format(.data$time, "%Y")))
                    
    mm = range(xx$year)
    gg = ggplot2::ggplot(data = xx, 
                         mapping = ggplot2::aes(x = year, y = value, group = year)) + 
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Year", 
                    y = "Heat transport (W)", 
                    caption = "data source: https://mocha.earth.miami.edu/mocha/data/index.html") +
      ggplot2::scale_x_continuous(breaks = seq(from = mm[1]-1, to = mm[2]+1, by = 5)) +
      ggplot2::facet_wrap(~name, scale = "free_y")
    
  } else {
  
    y = x |>
      dplyr::select(c("time", dplyr::starts_with("Q"))) |>
      tidyr::pivot_longer(cols = dplyr::starts_with("Q"))
    
    gg =  ggplot2::ggplot(data = y,
                    mapping = ggplot2::aes(x = time, y = value)) +
      ggplot2::geom_line()
    
     if (smooth) gg = gg + ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
      
     gg = gg + 
       ggplot2::labs(y = "Heat transport (W)", 
                    caption = "data source: https://mocha.earth.miami.edu/mocha/data/index.html") + 
      ggplot2::facet_wrap(~name, scales = "free_y")
  }
  
  gg
}
