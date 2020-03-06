#' Downloads NLDAS light data
#' @description This function downloads NLDAS incoming shortwave radiation data
#' (w m-2). When using the NLDAS_DL function to download data from many sites, sometimes
#' sites will fail to download data (possibly due to volume of requests). This function
#' adds in an additional check to continually attempt to download data for any missing
#' locations until no more locations can be successfully downloaded. For example, sites
#' outside of the U.S. do not have NLDAS data and therefore will never download.
#'
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param site_locs A table with Site_ID, Lat, and Lon, and startDate
#' @param statDate An optional parameter. By default, if nothing is provided the function
#' assumes that site_locs has a column that contains startDate. Alternatively, a single
#' startDate can be provided as an argument for the download (YYYY-MM-DD).
#'
#' @return Returns a time series of incoming shortwave solar radiation from the start
#' date to the most recent available data
#'
#' @export

#===============================================================================
#Function for bulk downloading NLDAS data via data rods that will try several
#times to download sites if they fail to initially download
#Created 7/5/2019
#===============================================================================
NLDAS_DL_bulk <- function(save_dir, site_locs, startDate){
  #Download NLDAS data for all sites
    if(hasArg(startDate) == TRUE){
      mapply(NLDAS_DL, save_dir = DL_NLDAS_dir, site_locs[, "Site_ID"], site_locs[, "Lat"],
        site_locs[, "Lon"], stardDate)
    }

    if(hasArg(startDate) == FALSE){
      mapply(NLDAS_DL, save_dir = DL_NLDAS_dir, site_locs[, "Site_ID"], site_locs[, "Lat"],
        site_locs[, "Lon"], site_locs[, "startDate"])
    }

  #Get sites with downloaded data
    dl_sites <- stringr::str_sub(list.files(save_dir)[grep("*_NLDAS.asc",
      list.files(save_dir))], 1, -11)

  #Check sites with missing data
    missing_sites <- site_locs[site_locs[, "Site_ID"] %in% setdiff(site_locs[, "Site_ID"],
      dl_sites), ]

  #Set the initial number of missing sites
    missing_number <- nrow(missing_sites)

  #If there are missing sites, retry downloading
    if(missing_number > 0){
      for(i in 1:missing_number){
        #Retry downloading NLDAS data
          mapply(NLDAS_DL, save_dir = save_dir, missing_sites[, "Site_ID"],
            missing_sites[, "Lat"], missing_sites[, "Lon"], startDate)

        #Check the number of missing sites
          dl <- stringr::str_sub(list.files(save_dir)[grep("*_NLDAS.asc",
            list.files(save_dir))], 1, -11)

        #Find remaining missing sites
          ms <- site_locs[site_locs[, "Site_ID"] %in% setdiff(site_locs[, "Site_ID"],
            dl), ]

        #Find # of remaining missing sites
          mn <- nrow(ms)

        #Check to see if no more missing sites have been downloaded
          if(mn == missing_number) break()

        #Update the number of missing sites
          if(mn != missing_number){message("Attempting to download missing sites")}
          if(mn != missing_number){missing_number <- mn}

      } #End for loop

    } #End if statement

} #End NLDAS_DL_bulk function


