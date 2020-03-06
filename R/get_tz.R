#' Determines timezone from Latitude and Longitude
#' @description This function determines the IANA/Olson time zone identifier from
#' latitude and longitude. This is a helper function which is used to assist in
#' converting downloaded data from UTC to local time.
#'
#' Timezone information is extracted from the included tz_world dataset. The original
#' tz_world data was created by Eric Muller and can be found at (http://efele.net/maps/tz/world/)
#'
#'
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#'
#' @return Returns the IANA/Olson time zone identifier for a given Latitude and Longitude
#' @export

#===============================================================================
#Function for retrieving timezone information based on latitude and longitude
#===============================================================================
get_tz <- function(Lat, Lon){
  #Import the tz_world dataset if it is not already loaded
    if(!exists("tz_world")){data("tz_world", package = "StreamLight")}

  #Create a spatial points data frame from the site location
    site_location <- data.frame(Lat, Lon)
    xy <- site_location[, c("Lon", "Lat")]

    xy_spdf <- sp::SpatialPointsDataFrame(coords = xy, data = site_location,
      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
  #Return the timezone name
    tz_name <- paste(over(xy_spdf, tz_world))
    
  #Error catch, sometimes points very near boundaries of large water bodies return
  #NA due to imprecise georeferencing either on the part of the map or point
  #Note, this is rudimentary and should be made more robust
    if(tz_name == "NA"){
      #Get TZ world coordinates
        tz_xy <- setNames(data.frame(data.frame(data.frame(coordinates(tz_world)), tz_world[[1]],
          stringsAsFactors = FALSE)), c("Lon", "Lat", "Timezone")) 
        
      #Subset within two degrees  
        tz_subset <- tz_xy[
          tz_xy[, "Lat"] <= xy[, "Lat"] + 2 & 
          tz_xy[, "Lat"] >= xy[, "Lat"] -2 &
          tz_xy[, "Lon"] <= xy[, "Lon"] + 2 &
          tz_xy[, "Lon"] >= xy[, "Lon"] - 2, ]     
        
      #Find the nearest timezone 
        distances <- as.matrix(dist(rbind(xy, tz_subset[, c("Lon", "Lat")])))
        row_num <- names(which.min(distances[distances[,"1"] > 0, "1"]))
        tz_name <- tz_subset[row_num, "Timezone"]
        
    } #End if statement        

  #Return the timezone name
    return(tz_name)

} #End get_tz function
