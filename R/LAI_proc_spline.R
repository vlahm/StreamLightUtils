#' Interpolates LAI into daily values
#' @description This function generates a continuous series of LAI from downloaded
#' from the lpdaac AppEEARS download portal through spline interpolation. Processing
#' steps include:
#'
#' \itemize{
#'   \item Savitsky-Golay filtering
#'   \item Fit a double logistic function to extract a continuous series of LAI
#' }
#'
#' @param Site The Site ID
#' @export

#===============================================================================
#Function for processing LAI data a Savitzky-Golary filter and spline
#===============================================================================
LAI_proc_spline <- function(Site){
  #Selecting the site of interest
    SOI <- Site

  #Adding in newjd data
    num_days <- ifelse(unique(as.numeric(SOI[, "Year"]))%%4 != 0, 365, 366)
    sum_days <- cumsum(num_days)
    day_adj <- setNames(data.frame(unique(SOI[, "Year"]), sum_days - num_days), c("Year", "day_adj"))

  #Adding a newjd column
    newjd <- by(SOI, SOI[, "Year"], function(x){x[, "newjd"] <- x[, "DOY"] +
      day_adj[day_adj[, "Year"] %in% x[, "Year"], "day_adj"]
      return(x)})

      ts <- do.call("rbind", newjd)

  #-------------------------------------------------
  #Savitzsky-Golay Filtering
  #-------------------------------------------------
    #Performing the Savitzsky-Golay filter #n = 8 + 3 - 8%%2
    #COME BACK AND CHECK p AND n
      ts$SGF <- signal::sgolayfilt(ts[, "Lai"], p = 2, n = 11, m = 0)
      # ts$SGF <- savGol(ts[, "Lai"])
      # ts <- na.omit(ts)

  #-------------------------------------------------
  #Generating continuous series of LAI
  #-------------------------------------------------
    #Generate a complete set of dates bewteen the first and last record
      full_dates <- setNames(data.frame(seq(from = as.Date(paste(ts[, "Year"], "-",
        ts[, "DOY"], sep = ""), "%Y-%j")[1], to = as.Date(paste(ts[, "Year"], "-",
        ts[, "DOY"], sep = ""), "%Y-%j")[nrow(ts)], by = 1)), "Date")

    #Adding in Year, DOY, and newjd
      full_dates$Year <- as.numeric(format(full_dates[, "Date"], "%Y"))
      full_dates$DOY <- as.numeric(format(full_dates[, "Date"], "%j"))
      full_dates$newjd <- seq(full_dates[1, "DOY"], nrow(full_dates) +
            (full_dates[1, "DOY"] - 1))

    #Fitting a spline through the Savitsky-Golay filtered data
      spline <- smooth.spline(ts[, "newjd"], ts[, "SGF"])

    #Predicting daily values of LAI
      full_dates$LAI <- predict(spline, full_dates[, "newjd"])$y

    #If spline is below or above observed value it is reset
      if(nrow(full_dates[full_dates[, "LAI"] < min(ts[, "SGF"]), ]) > 0){
        full_dates[full_dates[, "LAI"] < min(ts[, "SGF"]), ]$LAI <- min(ts[, "SGF"])
      }

      if(nrow(full_dates[full_dates[, "LAI"] > max(ts[, "SGF"]), ]) > 0){
        full_dates[full_dates[, "LAI"] > max(ts[, "SGF"]), ]$LAI <- max(ts[, "SGF"])
      }

    #Merging the data together and selecting the final output
      merged <- merge(full_dates, ts, by = "newjd", all.x = TRUE)

  #Selecting and reordering the columns for the final output
    final <- merged[, c("Date", "Year.x", "DOY.x", "newjd", "Lai","SGF", "LAI")]
      colnames(final) <- c("Date", "Year", "DOY", "newjd", "Lai", "SGF_MOD_LAI", "LAI_proc")

  return(final)

} #End LAI_proc_spline function
