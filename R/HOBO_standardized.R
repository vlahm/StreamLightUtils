#' Generates a standardized set of light from HOBO loggers
#' @description This function takes observed light from HOBO loggers (in lux)
#' and generates a standardized set of validation data for comparing estimated light.
#'
#' @param read_dir The read directory for downloaded files. For example, "C:/
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param estimate Binary ("yes", "no"). If yes, PPFD (umol m-2 s-1) is estimated
#' from the lux (lumens m-2) data
#'
#' @return Returns a standardized file of light data
#' @export

#===============================================================================
#Standardized HOBO light validation
#Created 7/5/2018
#===============================================================================
HOBO_standardized <- function(read_dir, filename, Lat, Lon, estimate){
  #Read in the file ignoring the additional columns for attached couplers etc...
    myfile <- read.csv(paste(read_dir, "/", filename, sep = ""), skip = 2, header = FALSE,
      stringsAsFactors = FALSE, na.strings = "")

  #Pull simplified column names out and add them
    colnames(myfile) <- gsub(",.*$", "", names(data.table::fread(paste(read_dir, "/",
      filename, sep = ""), nrows = 1, skip = 1, sep = ",")))

  #Select the final columns for variables of interest
    VOI <- myfile[, c("Date Time", "Intensity")]

  #-------------------------------------------------
  #Handling date and time information
  #-------------------------------------------------
    #Adding a POSIX time column in UTC (note, note the same as when using the files downloaded
    #via the portal. Required 12->24hr conversion and %y instead of %Y)
      VOI$UTC_time <- as.POSIXct(VOI[, "Date Time"],
        format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")

    #Getting the name of the local timezone
      tz_name <- get_tz(Lat, Lon)

    #Adding a POSIX time column in local time
      VOI$local_time <- as.POSIXct(format(VOI[, "UTC_time"], format =
        "%Y-%m-%d %H:%M:%S", tz = tz_name), tz = tz_name)

    #Adding in Year, DOY, and local time hour information
      VOI[, "Year"] <- as.numeric(strftime(VOI[, "UTC_time"], format = "%Y", tz = tz_name))
      VOI[, "DOY"] <- as.numeric(strftime(VOI[, "UTC_time"], format = "%j", tz = tz_name))
      VOI[, "Hour"] <- as.numeric(strftime(VOI[, "UTC_time"], format = "%H", tz = tz_name)) +
        as.numeric(strftime(VOI[, "UTC_time"], format = "%M", tz = tz_name)) / 60

      VOI[, "jday"] <- as.matrix(as.numeric(paste(VOI[, "Year"], sprintf("%03d",
        VOI[, "DOY"]), sep = "")))

    if(estimate == "yes"){
      #Converting Lux (lumens m-2) to PPFD (umol m-2 s-1)
        VOI$light_obs <- (((VOI[, "Intensity"] / 683)) / 2.35*10^-5) * 1000000
    } #End if statement

    if(estimate == "no"){
      VOI$light_obs <- VOI[, "Intensity"]
    } #End if statement

    #Ordering the data
      ordered <- VOI[order(VOI[, "local_time"]), ]

  return(ordered[, c("local_time", "jday", "Year", "DOY", "Hour", "light_obs")])
} #End hobo_standardized function

