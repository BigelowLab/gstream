% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/usn.R
\name{crop_usn}
\alias{crop_usn}
\title{Crop USN wall data}
\usage{
crop_usn(
  x,
  bb = sf::st_as_sfc(sf::st_bbox(c(xmin = -71, ymin = 32, xmax = -61, ymax = 45), crs =
    4326)),
  resolve_multiline = c("keep", "longest")[2],
  verbose = FALSE,
  use_s2 = FALSE
)
}
\arguments{
\item{x}{sf table of wall data.  Only matched north-south pairs for
each given data are converted. All others are dropped.}

\item{bb}{bounding box (or object from which we can get a bounding box) to crop}

\item{resolve_multiline}{char, when \code{type} is LINESTRING a conflict may arise when
a wall is composed of multiple lines, which yields MULTILINESTRING.
Set this to retain just the longest LINESTRING or to use "keep" retain as MULTILINESTRING.}

\item{use_s2}{logical, if TRUE then turn on use of the s2 engine}
}
\value{
The input cropped.  If a particular line can not be cropped by s2, then
that row of the input is dropped
}
\description{
Crop USN wall data
}
