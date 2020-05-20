
#extract_mean_from_raster

  #extract_mean_from_raster is a function to extract the average value of a raster over each polygon in a shapefile. It is 
  #essentially a wrapper for the extract function in the raster package. You can choose to have the function return a 
  #dataframe with one row for each polygon, and the corresponding raster mean, or a tibble table that is ready for 
  #plotting in ggplot. 
  
  #ARGUMENTS
  #raster: is a raster file stored as a "Formal class RasterLayer" - load using the raster function. Check projections. 
  #polygon: a polygon shapefile stored as a "SpatialPolygonsDataFrame" - load using readOGR. Check projections. 
  #polygon_id: the name of the column containing a unique ID for each polygon - e.g. "BlockNo"
  #output: options for outputs: 
  #"df" means return a dataframe with one row per polygon. 
  #"for.map" returns a tibble table that is ready to be plot using ggplot. Use aestherics: x = long, y = lat, group = group

extract_mean_from_raster<-function(raster, polygon, polygon_id, output){
  r.vals <- raster::extract(raster, polygon) #extract values of raster
  r.mean <- unlist(lapply(r.vals, FUN=mean)) #take mean for each polygon
  
  polygon@data <- data.frame(polygon@data, mean=r.mean) #merge mean back in to polygon
  
  out<-broom::tidy(polygon, region = paste(polygon_id)) #reformat to support mapping
  
  map.data<-left_join(out, data.frame(polygon@data), by = c("id" = paste(polygon_id))) #merge back with polygon
  
  if(output == "df"){  polygon@data } #output dataframe
  if(output == "for.map") { map.data } #output ggplot ready tibble
  
}