#' Get LiDAR derived tree heights based on Latitude and Longitude
#' @description Extract canopy height from the LiDAR derived map of
#' tree heights produced by Simard et al. 2011 (asdf)
#'
#' @param Site The site ID
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#'
#' @return Site ID, Latitude, Longitude, and tree height
#' @export

#===============================================================================
#Extracting canopy height from Simard et al. (2011) for our site
#Map downloaded from http://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=10023
  #Simard, M., N. Pinto, J. B. Fisher, and A. Baccini (2011), Mapping forest canopy
  #height globally with spaceborne lidar, J. Geophys. Res., 116, G04021,
  #doi:10.1029/2011JG001708.
#===============================================================================
extract_height <- function(Site_ID, Lat, Lon){
  #Import the Simard et al. (2011) dataset if it is not already loaded
    if(!exists("simard2011")){data("simard2011", package = "StreamLightUtils")}

  #Defining the min and max values (by default these are not associated with the raster)
    map_ranges <- raster::setMinMax(simard2011)

  #Getting Lat and Lon data for my sites and making spatial
    site_location <- data.frame(Site_ID, Lat, Lon)
    xy <- site_location[, c("Lon", "Lat")]

    xy_spdf <- sp::SpatialPointsDataFrame(coords = xy, data = site_location,
        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  #Extracting the canopy height at our site
    TH <- raster::extract(map_ranges, xy_spdf)

  bound <- setNames(data.frame(Site_ID, Lat, Lon, TH), c("Site_ID", "Lat", "Lon", "TH"))

  return(bound)
} #End extract_height function

