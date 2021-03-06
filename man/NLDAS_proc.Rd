% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NLDAS_proc.R
\name{NLDAS_proc}
\alias{NLDAS_proc}
\title{Processes downloaded NLDAS light data}
\usage{
NLDAS_proc(read_dir, Site_IDs)
}
\arguments{
\item{read_dir}{The read directory for downloaded files. For example, "C:/myfolder}

\item{Site_IDs}{Site name(s), for example "NC_UEno"}
}
\value{
Returns a time series of incoming light data
}
\description{
This function processes downloaded NLDAS incoming shortwave radiation
data (w m-2) for a given Latitude and Longitude.
}
