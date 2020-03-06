#' Unpacks downloaded MODIS LAI data
#' @description This function unpacks downloaded MODIS LAI data from the lpdaac
#' AppEEARS download portal into a list with each item in the list representing
#' a single site. Additionally, data QC is performed
#'
#' @param zip_file The name of the zip file. For example, "myzip.zip"
#' @param zip_dir The directory the zip file is located in
#' @param request_sites A string of Site_IDs
#'
#' @export

#===============================================================================
#Function for unpacking the LAI data downloaded from AppEEARS
#Created 8/13/2018
#===============================================================================
AppEEARS_unpack <- function(zip_file, zip_dir, request_sites){
  #Get the name of the request based on the .zip file
    request <- sub('\\..*', '', zip_file)

  #Unzip the AppEEARS file to the same path as the zip file
    unzip(paste(zip_dir, "/", zip_file, sep = ""), exdir = paste(zip_dir, "/",
      request, sep = ""))

  #Identifying the file containing the results using regular expression and pattern matching
    grx_exp <- glob2rx(paste("*-results.csv", sep = ""))
    folder_files <- list.files(paste(zip_dir, "/", request, sep = ""))
    results_fn <- folder_files[grep(grx_exp,folder_files)][1]

  #Identify the MODIS product used and info to paste in to read the file
    res_split <- strsplit(results_fn, "-")[[1]]
    prod_loc <- grep(glob2rx(paste("MCD*", sep = "")), res_split)

  #Reading in the results based on the MODIS product used
    raw <- data.frame(data.table::fread(paste0(zip_dir, "/", request, "/", results_fn)))

  #-------------------------------------------------
  #Performing QC, adding date, and selecting the final columns
  #-------------------------------------------------
    #Get a string needed to select column headings based on version
      heading <- paste(strsplit(results_fn, "-")[[1]][prod_loc:(prod_loc + 1)], collapse = "_")

    #Selecting scenes that meet the QC requirements
      scene_sel <- raw[raw[, paste(heading, "_FparLai_QC_MODLAND", sep = "")] !="0b1" &
        raw[, paste(heading, "_FparLai_QC_CloudState", sep = "")] != "0b01" &
        raw[, paste(heading, "_FparLai_QC_SCF_QC", sep = "")] != "0b010" &
        raw[, paste(heading, "_FparLai_QC_SCF_QC", sep = "")] != "0b011" &
        raw[, paste(heading, "_FparLai_QC_SCF_QC", sep = "")] != "0b100",]

    #Taking only the columns I am interested in
      reduced <- scene_sel[, c("ID", "Date", paste(heading, "_Lai_500m", sep = ""),
        paste(heading, "_LaiStdDev_500m", sep = ""))]
        colnames(reduced)[3:4] <- c("Lai", "Lai_sd")

    #Adding POSIX time column
      reduced$pos_time <- as.POSIXct(reduced[, "Date"], format = "%Y-%m-%d", tz = "UTC")

    #Adding in Year and DOY information
      reduced$Year <- as.numeric(strftime(reduced[, "pos_time"], format = "%Y", tz = "UTC"))
      reduced$DOY <- as.numeric(strftime(reduced[, "pos_time"], format = "%j", tz = "UTC"))

    #Selecting only the final information I need
      VOI <- reduced[, c("ID", "pos_time", "Year", "DOY", "Lai", "Lai_sd")]

  #-------------------------------------------------
  #Exporting the data for each site
  #-------------------------------------------------
    #Splitting the dataset up
      site_split <- split(VOI, VOI[, "ID"])

    #Assigning the proper Site ID
      for(i in 1:length(site_split)){
        #Getting the Site name (the MODIS request removes "_")
          site_name <- request_sites[gsub("[[:punct:]]", "", request_sites) %in%
            unique(site_split[[i]][, "ID"])]

          site_split[[i]][, "ID"] <- site_name

      } #End for loop

    #Data frame of Site_ID's with and without punctuation
      ID_DF <- setNames(data.frame(gsub("[[:punct:]]", "", request_sites), request_sites),
        c("no_punct", "Site_ID"))

    #Merging together the names
      ID_merge <- merge(setNames(data.frame(names(site_split)), "no_punct"), ID_DF,
        by = "no_punct")

      names(site_split) <- ID_merge[, "Site_ID"]

  #Notify the user with a list of sites that did not have data
    missing <- request_sites[!(request_sites %in% names(site_split))]

    if(length(missing) != 0){
      message(paste("The following sites did not have LAI data in this request:",
      paste(missing, sep="", collapse=", ")))
    } #End if statement

  return(site_split)

} #End AppEEARS_proc function
