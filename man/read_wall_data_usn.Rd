% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/usn.R
\name{read_wall_data_usn}
\alias{read_wall_data_usn}
\title{Read the GS wall data in usn format}
\usage{
read_wall_data_usn(filename, verbose = FALSE)
}
\arguments{
\item{filename}{char, the name of file, a vector of files, or the nameof a
single tar file (with one or more *.sub files within)}

\item{verbose}{logical, if TRUE output messages}
}
\value{
tibble or sf MULTIPOINT object
}
\description{
Read the GS wall data in usn format
}
