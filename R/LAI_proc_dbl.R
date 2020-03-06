#' Generate a continuous series of LAI using a Savitzky-Golay filter and double
#' logistic function
#' @description This function generates a continuous series of LAI from downloaded
#' from the lpdaac AppEEARS download portal. Processing steps include:
#'
#' \itemize{
#'   \item Savitsky-Golay filtering
#'   \item Fit a double logistic function to extract a continuous series of LAI
#' }
#'
#' @param Site The Site ID
#' @export

#===============================================================================
#Function for processing LAI data a Savitzky-Golary filter and double logistic function
#===============================================================================
LAI_proc_dbl <- function(Site){
  #Selecting the site of interest
    SOI <- Site

  #-------------------------------------------------
  #Savitzsky-Golay Filtering
  #-------------------------------------------------
    #Performing the Savitzsky-Golay filter #n = 2 + 3 - 2%%2
    #COME BACK AND CHECK p AND n
      SOI$SGF <- signal::sgolayfilt(SOI[, "Lai"], p = 2, n = 11, m = 0)

  #-------------------------------------------------
  #Generating continuous series of LAI
  #-------------------------------------------------
    #Generate a complete set of dates bewteen the first and last record
      full_dates <- setNames(data.frame(seq(from = as.Date(paste(SOI[, "Year"],
        "-", SOI[, "DOY"], sep = ""), "%Y-%j")[1], to = as.Date(paste(SOI[, "Year"],
        "-", SOI[, "DOY"], sep = ""), "%Y-%j")[nrow(SOI)], by = 1)), "Date")

    #Adding in Year, DOY, and newjd
      full_dates$Year <- as.numeric(format(full_dates[, "Date"], "%Y"))
      full_dates$DOY <- as.numeric(format(full_dates[, "Date"], "%j"))
          full_dates$newjd <- seq(full_dates[1, "DOY"], nrow(full_dates) +
            (full_dates[1, "DOY"] - 1))

    #Merging together
      full_merge <- merge(full_dates, SOI, by = c("Year", "DOY"), all.x = TRUE)

  #-------------------------------------------------
  #Fitting a double logistic function to the data
  #-------------------------------------------------
    #Splitting by year
      year_split <- split(full_merge, full_merge[, "Year"])

    #Fitting a double logistic function to each year of data
      for(i in 1:length(year_split)){
        year_split[[i]]$LAI_proc <- as.numeric(FitDoubleLogBeck(year_split[[i]][, "SGF"])$predicted)
      }

    #Unsplitting the data
      year_unsplit <- unsplit(year_split, full_merge[, "Year"])

    #If the function fit is above observed value it is reset
      LAI_min <- min(year_unsplit[, "SGF"], na.rm = TRUE)
      LAI_max <- max(year_unsplit[, "SGF"], na.rm = TRUE)

      if(nrow(year_unsplit[year_unsplit[, "LAI_proc"] < LAI_min, ]) > 0){
        year_unsplit[year_unsplit[, "LAI_proc"] < LAI_min, ]$LAI_proc <- LAI_min
      }

      if(nrow(year_unsplit[year_unsplit[, "LAI_proc"] > LAI_max, ]) > 0){
        year_unsplit[year_unsplit[, "LAI_proc"] > LAI_max, ]$LAI_proc <- LAI_max
      }

  #Selecting and reordering the columns for the final output
    LAI_processed <- year_unsplit[, c("Date", "Year", "DOY", "newjd", "Lai", "SGF", "LAI_proc")]
      colnames(LAI_processed) <- c("Date", "Year", "DOY", "newjd", "Lai", "SGF_MOD_LAI", "LAI_proc")

  return(LAI_processed)

} #End LAI_proc_dbl function
