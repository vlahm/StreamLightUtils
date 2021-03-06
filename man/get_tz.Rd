% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tz.R
\name{get_tz}
\alias{get_tz}
\title{Determines timezone from Latitude and Longitude}
\usage{
get_tz(Lat, Lon)
}
\arguments{
\item{Lat}{The site Latitude}

\item{Lon}{The site Longitude}
}
\value{
Returns the IANA/Olson time zone identifier for a given Latitude and Longitude
}
\description{
This function determines the IANA/Olson time zone identifier from
latitude and longitude. This is a helper function which is used to assist in
converting downloaded data from UTC to local time.

Timezone information is extracted from the included tz_world dataset. The original
tz_world data was created by Eric Muller and can be found at (http://efele.net/maps/tz/world/)
}
