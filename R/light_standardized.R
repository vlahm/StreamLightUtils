#' Generates a standardized set of light validation data for StreamPULSE sites
#' @description This function takes observed light, in either Lux or PPFD, from
#' StreamPULSE sites and generates a standardized set of validation data for
#' comparing estimated light.
#'
#' @param read_dir The read directory for downloaded files. For example, "C:/
#' @param save_dir The save directory for files to be placed in. For example, "C:/
#' @param Site The site name, for example "FL_ICHE2700"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#'
#' @return Returns a standardized file of light data
#' @export

#===============================================================================
#Standardized StreamPULSE light validation
#Created 11/27/2017
#===============================================================================
light_standardized <- function(read_dir, save_dir, Site, Lat, Lon){
  #Reading in the observed data and dynamically generating character classes
    setwd(read_dir)
    obs <- read.csv(paste(Site, "_sensorData.csv", sep = ""), colClasses = c(rep("character", 3),
      rep("numeric", (ncol(data.table::fread(paste(Site, "_sensorData.csv", sep = ""), skip = 1, nrows = 1,
      sep = ",")) - 3))))

  #-------------------------------------------------
  #Handling date and time information
  #-------------------------------------------------
    #Adding a POSIX time column in UTC
      obs$UTC_time <- as.POSIXct(obs[, "DateTime_UTC"], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    #Getting the name of the local timezone
      tz_name <- get_tz(Lat, Lon)

    #Adding a POSIX time column in local time
      obs$local_time <- as.POSIXct(format(obs[, "UTC_time"], format =
        "%Y-%m-%d %H:%M:%S", tz = tz_name), tz = tz_name)

    #Adding in Year, DOY, and local time hour information
      obs[, "Year"] <- as.numeric(strftime(obs[, "UTC_time"], format = "%Y", tz = tz_name))
      obs[, "DOY"] <- as.numeric(strftime(obs[, "UTC_time"], format = "%j", tz = tz_name))
      obs[, "Hour"] <- as.numeric(strftime(obs[, "UTC_time"], format = "%H", tz = tz_name)) +
        as.numeric(strftime(obs[, "UTC_time"], format = "%M", tz = tz_name)) / 60

      obs[, "jday"] <- as.matrix(as.numeric(paste(obs[, "Year"], sprintf("%03d",
        obs[, "DOY"]), sep = "")))

  #-------------------------------------------------
  #Identifying and processing light data
  #-------------------------------------------------
    #Finding columns with light data
      lux_cols <- grep("^.+lux$", colnames(obs), perl = TRUE, value = TRUE)
      par_cols <- grep("^.+PAR$", colnames(obs), perl = TRUE, value = TRUE)

    #For right now I am choosing between PAR and lux based on which has the most data
    #TEMPORARY
      light_binary <- which.max(c(length(as.vector(as.matrix(obs[, lux_cols]))),
        length(as.vector(as.matrix(obs[, par_cols])))))

      ifelse(light_binary == 1, light_source <- "lux", light_source <- "PAR")

    #Conditional processing if light data is in lux
      if(light_source == "lux"){
        #Generating a single lux column (takes mean if multiple sensors)
          ifelse(length(lux_cols) > 1, obs$lux <- rowMeans(obs[, lux_cols]),
            obs$lux <- obs[, lux_cols])

        #Converting Lux (lumens m-2) to umol m-2 s-1
        #NOT SET ON THIS CONVERsION
          obs$light_obs <- (((obs[, "lux"] / 683)) / 2.35*10^-5) * 1000000

      } #End if statement

    #Conditional processing if light data is PAR
    #REVISIT IN CASE THEY HAVE NOT MEASURED IT IN umol m-2 s-1
      if(light_source == "PAR"){obs$light_obs <- obs[, par_cols]}
      #if(light_source == "PAR"){obs$light_obs <- obs[, par_cols] * 0.235}

  #-------------------------------------------------v
  #Write the final output
  #-------------------------------------------------
    final <- obs[, c("local_time", "jday", "Year", "DOY", "Hour", "light_obs")]
    setwd(save_dir)
    write.csv(final, paste(Site, "_standardized.csv", sep = ""), row.names = FALSE, quote = FALSE)

} #End light_standardized function
