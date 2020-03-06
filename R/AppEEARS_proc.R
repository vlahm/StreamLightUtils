#' Filters and gap-fills downloaded MODIS AppEEARS data
#' @description This function filters and gap-fills downloaded MODIS LAI data from
#' the lpdaac AppEEARS download portal. Processing steps include:
#'
#' \itemize{
#'   \item Savitsky-Golay filtering
#'   \item Interpolation using either a spline or double logistic function
#' }
#'
#' @param Site The site name
#' @param proc_type either "spline" (for a spline) or "dbl_log" (for a double logistic function)
#'
#' @export

#===============================================================================
#Function for filtering and gap-filling downloaded MODIS data
#Created 8/13/2018
#===============================================================================
AppEEARS_proc <- function(Site, proc_type){
  #If proc_type == spline, use the spine function
    if(proc_type == "spline"){processed <- LAI_proc_spline(Site)}

  #If pro_type == dbl_log use the double logistic function
    if(proc_type == "dbl_log"){processed <- LAI_proc_dbl(Site)}

  #Calculating some fitting statistics for the Savitzky-Golay filter
    r2 <- round(summary(lm(processed[, "Lai"] ~ processed[, "SGF_MOD_LAI"]))$adj.r.squared, 2)
    RMSE <- round(sqrt(mean((processed[, "Lai"] - processed[, "SGF_MOD_LAI"])^2 , na.rm = TRUE )), 2)
    MAE <- round(mean(abs(processed[, "Lai"] - processed[, "SGF_MOD_LAI"]), na.rm = TRUE), 2)

  #-------------------------------------------------
  #Plotting the results
  #-------------------------------------------------
    #Observed MODIS data
      ylabel <- expression(paste("LAI ", "(m"^{2}, "m"^{-2},")")) #Set y label

      plot(processed[, "Date"], processed[, "Lai"], pch = 20, col = "grey60",
        ylim = c(0, 7), xlab = "Time (Days)", ylab = ylabel, main = paste("Site= ",
        unique(Site[, "ID"])," r2 = ", r2, " ;RMSE = ", RMSE, " ;MAE = ", MAE, sep = ""))

    #SGF filtered MODIS data
      points(processed[, "Date"], processed[, "SGF_MOD_LAI"], pch = 20)

    #Continuous daily LAI (from spline fit to SGF filtered data)
      lines(processed[, "Date"], processed[, "LAI_proc"], col = "darkorange")

  return(processed)

} #End AppEEARS_proc function

