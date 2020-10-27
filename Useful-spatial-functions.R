require(raster)
require(tidyverse)
require(devtools)
require(sf)
require(magrittr)
require(ncdf4)
require(lubridate)
require(doParallel)
require(parallel)
require(foreach)
require(tidync)



devtools::source_url("https://github.com/aodn/imos-user-code-library/blob/master/R/commons/NetCDF/ncParse.R?raw=TRUE")

#get_sst

#get_sst is a function to extract the sst given a series of Dates and points (Latitudes and Longitude). The function
#returns a single column of sst corresponding to the input Dates and points.
#sst is extracted from the AODN website and corresponds to a 6 day moving average. This was used to minimise holes
#and for better reach into coastal areas. SST resolution is 0.02deg. The extraction function searches for data within 
#100 and then 10000 and then 100000m from the point. Failing that it returns a warning
#more details on source data at: https://catalogue-imos.aodn.org.au/geonetwork/srv/api/records/023ae12a-8c0c-4abc-997a-7884f9fec9cd

#ARGUMENTS
#Dates: is a data vector (created using as.Date) with format YYYY-MM-DD. 
#Latitude: is the latitude of the point files in GDA94
#Longitude is the longitude of the pont files in GDA94

get_sst<- function(Dates, Long, Lat){
  
  Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
    sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>% #only extract for points on this day
    st_transform(3112) %>%
    mutate(ID = row_number())
  
  #ggplot() +  geom_sf(data = Australia_map_Lambert) + geom_sf(data = Points) 
  
  Points %<>% mutate(land = as.integer(sf::st_intersects(geometry, Australia_map_Lambert))) #returns NA for water
  
  Points_extraction <- Points %>% filter(is.na(land))
  Date_code <- as.character(Points_extraction$Dates)
  
  Date_code %<>% str_replace_all(., "-", "") %>% unique() #Extra Date codes for each day desired
  
  for(i in 1:length(Date_code)) { #loop through days download and extract data
    print(i)
    file_URL<- paste0("http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-6d/dn/", 
                      substr(Date_code[i], 1,4), "/", 
                      Date_code[i], 
                      "212000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-6d_dn.nc")
    
    dataset <- ncParse(file_URL, variables = "sea_surface_temperature")
    
    ## Extract data from variables and dimensions
    lat <- dataset$dimensions$lat$data
    lon <- dataset$dimensions$lon$data
    temp <- dataset$variables$sea_surface_temperature$data
    
    ## Create a raster of longitude, latitude, and temperature data
    dat1 <- list( )
    dat1$x <- c( lon)
    dat1$y <- c( lat)
    dat1$z <- t( temp)
    raster <- raster( dat1$z, xmn = range( dat1[[1]])[1], xmx = range( dat1[[1]])[2], ymn = range( dat1[[2]])[1], ymx = range( dat1[[2]])[2])
    
    crs(raster)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')#in WGS84
    
    #Get points ready
    temp <- Points_extraction %>% filter(Dates == ymd(Date_code[i])) %>% st_transform(4326) #only extract for points on this day
    
    temp$sst.100<-raster::extract(raster, temp, buffer = 100, fun=mean)
    temp$sst.10000<-raster::extract(raster, temp, buffer = 10000, fun=mean)
    temp$sst.100000<-raster::extract(raster, temp, buffer = 100000, fun=mean)
    temp$sst <-ifelse(is.na(temp$sst.100), ifelse(is.na(temp$sst.10000), temp$sst.100000, temp$sst.10000), temp$sst.100)
    
    if(i==1){points_extracted<-temp}else{ points_extracted<-rbind(temp,points_extracted)}
  }
  points_extracted %<>% st_drop_geometry()
  if( any(is.na(points_extracted$sst))) warning('raster extract returned NA consider increasing bufffer size')
  Points %<>% left_join(.,points_extracted[, c("ID", "sst")], by = c("ID" = "ID"))
  Points$sst
}


get_sst_OneMonthAverage<- function(Dates, Long, Lat){
  #get_sst
  
  #get_sst is a function to extract the sst given a series of Dates and points (Latitudes and Longitude). The function
  #returns a single column of sst corresponding to the input Dates and points.
  #sst is extracted from the AODN website and corresponds to a monthly average. This was used to minimise holes
  #and for better reach into coastal areas. SST resolution is 0.02deg. The extraction function searches for data within 
  #100 and then 10000 and then 100000m from the point. Failing that it returns a warning
  #more details on source data at: https://catalogue-imos.aodn.org.au/geonetwork/srv/api/records/023ae12a-8c0c-4abc-997a-7884f9fec9cd
  
  #ARGUMENTS
  #Dates: is a data vector (created using as.Date) with format YYYY-MM-DD
  #Latitude: is the latitude of the point files in GDA94
  #Longitude is the longitude of the pont files in GDA94
    
    Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
      sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>% #only extract for points on this day
      st_transform(4326) %>%
      mutate(ID = row_number(), Month = substr(Dates, 1, 7))
    
    Month_list <- unique(as.character(Points$Month))
    Month_code <- Month_list %>% str_replace_all(., "-", "") #Extra Date codes for each day desired
    
    URL_front_part <- "http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L3S-1m/ngt/"
    
    for(i in 1:length(Month_code)) { #loop through days download and extract data
      print(i)
      if(substr(Month_code[i], 5,6) %in% c("01", "03", "05", "07", "08", "10", "12")) {
        file_URL<- paste0(URL_front_part, substr(Month_code[i], 1,4), "/", substr(Month_code[i], 1,6), "31",
                          "152000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
      
      if(substr(Month_code[i], 5,6) %in% c("04", "06", "09", "11")) {
        file_URL<- paste0(URL_front_part, substr(Month_code[i], 1,4), "/", substr(Month_code[i], 1,6), "30",
                          "032000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
      
      if(substr(Month_code[i], 5,6) %in% c("02") & substr(Month_code[i], 1,4) %in% seq(1980,2100, 4)) { #leap year
        file_URL<- paste0(URL_front_part, substr(Month_code[i], 1,4), "/", substr(Month_code[i], 1,6), "29",
                          "152000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
      
      if(substr(Month_code[i], 5,6) %in% c("02") & !substr(Month_code[i], 1,4) %in% seq(1980,2100, 4)) {
        file_URL<- paste0(URL_front_part, substr(Month_code[i], 1,4), "/", substr(Month_code[i], 1,6), "28",
                          "032000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
      
      download.file(file_URL, "temp.nc", mode = 'wb') #to be saved in working directory
      
      #get relevant data points
      Points_month<-Points %>% filter(Month == Month_list[i])
      
      #get extraction bbox (add a bit of a buffer)
      lonrange<-c(st_bbox(Points_month)$xmin-0.5, st_bbox(Points_month)$xmax+0.5)
      latrange<-c(st_bbox(Points_month)$ymin-0.5, st_bbox(Points_month)$ymax+0.5)
      
      #read in the data
      nc<-tidync("temp.nc") #open months data
      
      nc_slice <- nc %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                                      lat = lat > latrange[1] & lat <= latrange[2])
      
      
      nc_slice_data <- nc_slice %>% hyper_array(select_var = c("sea_surface_temperature"))
      
      #get sst matrix
      sst_matrix<- apply(nc_slice_data$sea_surface_temperature, c(1,2), mean)
      
      #create rasters
      trans <- attr(nc_slice_data, "transforms")
      
      lon <- trans$lon %>% dplyr::filter(selected)
      lat <- trans$lat %>% dplyr::filter(selected)
      
      dat1 <- list( )
      dat1$x <- lon$lon
      dat1$y <- lat$lat
      dat1$z <- t(sst_matrix)
      
      sst_raster=raster( dat1$z, xmn = range( dat1[[1]])[1], xmx = range( dat1[[1]])[2], ymn = range( dat1[[2]])[1], ymx = range( dat1[[2]])[2])
      
      crs(sst_raster)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')#in WGS84
      
      #Get points ready
      
      Points_month$sst.100<-raster::extract(sst_raster, Points_month, buffer = 100, fun=mean)
      Points_month$sst.10000<-raster::extract(sst_raster, Points_month, buffer = 10000, fun=mean)
      Points_month$sst.100000<-raster::extract(sst_raster, Points_month, buffer = 100000, fun=mean)
      Points_month$sst <-ifelse(is.na(Points_month$sst.100), ifelse(is.na(Points_month$sst.10000), Points_month$sst.100000, 
                                                                    Points_month$sst.10000), Points_month$sst.100)
      
      if(i==1){points_extracted<-Points_month}else{ points_extracted<-rbind(Points_month,points_extracted)}
    }
    
    points_extracted %<>% st_drop_geometry() %>% arrange(ID)
    if( any(is.na(points_extracted$sst))) warning('raster extract returned NA consider increasing bufffer size')
    
    points_extracted$sst
  }



get_hs_ws<- function(Dates, Long, Lat){
  #get_hs_ws
  
  #get_hs_ws is a function to extracts monthly mean significant wave heights and wind speeds given a series of Dates and points 
  #(Latitudes and Longitude). 
  #The function returns a a list of 2 lists (one for hs and one for ws) corresponding to the input Dates and points.
  #hs and ws is extracted from the CSIRO https://data.csiro.au/collections/#collection/CIcsiro:39819. 
  #The extraction function searches for data within 
  #100 and then 10000 and then 100000m from the point. Failing that it returns a warning
  
  #ARGUMENTS
  #Dates: is a data vector (created using as.Date) with format YYYY-MM-DD
  #Latitude: is the latitude of the point files in GDA94
  #Longitude is the longitude of the pont files in GDA94
  
  Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
    sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>%
    st_transform(4326) %>% 
    mutate(ID = row_number(), Month = substr(Dates, 1, 7))
  
  #Date_code <- as.character(Points_extraction$Dates) #time steps to get data for
  
  Month<-unique(Points$Month)
  Month_code <- Month %>% str_replace_all(., "-", "") #Get the months to loop through
  
  #register for parallel computing
  avail_cores<-detectCores()
  registerDoParallel(cores=2) #using 80% of the cores available
  
  points_extracted <- foreach(i = 1:length(Month_code), .combine=rbind) %dopar% { #looping through time steps in month
    
    print(i)
    
    #prepare file url
    file_URL<- paste0("http://data-cbr.csiro.au/thredds/dodsC/catch_all/CMAR_CAWCR-Wave_archive/CAWCR_Wave_Hindcast_aggregate/gridded/ww3.aus_4m.",
                      Month_code[i], ".nc")
    
    #get relevant data points
    Points_month<-Points %>% filter(Month == Month[i])
    
    #get extraction bbox (add a bit of a buffer)
    lonrange<-c(st_bbox(Points_month)$xmin-0.5, st_bbox(Points_month)$xmax+0.5)
    latrange<-c(st_bbox(Points_month)$ymin-0.5, st_bbox(Points_month)$ymax+0.5)
    
    #read in the data
    while(TRUE){
      nc<-try(tidync(file_URL)) #open months data
      
      nc_slice <- try(nc %>% hyper_filter(longitude = longitude > lonrange[1] & longitude <= lonrange[2], 
                                          latitude = latitude > latrange[1] & latitude <= latrange[2]))
      
      if(Month_code[i] >= 201306) {
        nc_slice_data <- try(nc_slice %>% hyper_array(select_var = c("uwnd", "vwnd", "hs")))}else{
          nc_slice_data <- try(nc_slice %>% hyper_array(select_var = c("U10", "V10", "hs")))}
      
      if(!is(nc, 'try-error') & !is(nc_slice, 'try-error') & !is(nc_slice_data, 'try-error') & length(nc_slice_data)==3) {print("break")}
      if(!is(nc, 'try-error') & !is(nc_slice, 'try-error') & !is(nc_slice_data, 'try-error')& length(nc_slice_data)==3) break}
    
    
    #get wind and wave matrix
    if(Month_code[i] >= 201306) {
      nc_slice_data$wspeed <- sqrt(nc_slice_data$uwnd^2 + nc_slice_data$vwnd^2)}else{ #converting from vectors
        nc_slice_data$wspeed <- sqrt(nc_slice_data$U10^2 + nc_slice_data$V10^2)} #converting from vectors
    wind_matrix<- apply(nc_slice_data$wspeed, c(1,2), mean) #taking average over month
    
    wave_matrix<- apply(nc_slice_data$hs, c(1,2), mean)
    
    #create rasters
    trans <- attr(nc_slice_data, "transforms")
    
    lon <- trans$longitude %>% dplyr::filter(selected)
    lat <- trans$latitude %>% dplyr::filter(selected)
    
    dat1 <- list( )
    dat1$x <- lon$longitude
    dat1$y <- lat$latitude
    dat1$z <- wave_matrix
    
    wave_raster=raster(dat1)
    
    dat1$z <- wind_matrix
    wind_raster=raster(dat1)
    
    Points_month$wave.100<-raster::extract(wave_raster, Points_month, buffer = 100, fun=mean)
    Points_month$wave.10000<-raster::extract(wave_raster, Points_month, buffer = 10000, fun=mean)
    Points_month$wave.100000<-raster::extract(wave_raster, Points_month, buffer = 100000, fun=mean)
    Points_month$wave <-ifelse(is.na(Points_month$wave.100), ifelse(is.na(Points_month$wave.10000), Points_month$wave.100000, 
                                                                    Points_month$wave.10000), Points_month$wave.100)
    
    Points_month$wind.100<-raster::extract(wind_raster, Points_month, buffer = 1, fun=mean)
    Points_month$wind.10000<-raster::extract(wind_raster, Points_month, buffer = 10000, fun=mean)
    Points_month$wind.100000<-raster::extract(wind_raster, Points_month, buffer = 100000, fun=mean)
    Points_month$wind <-ifelse(is.na(Points_month$wind.100), ifelse(is.na(Points_month$wind.10000), Points_month$wind.100000, 
                                                                    Points_month$wind.10000), Points_month$wind.100)
    
    Points_month
    
  }
  
  points_extracted %<>% st_drop_geometry() %>% arrange(ID)
  if( any(is.na(points_extracted$wave | points_extracted$wind))) warning('raster extract returned NA consider increasing bufffer size')
  
  out<-list(points_extracted$wave, points_extracted$wind); names(out)<-c("wave", "wind")
  out
}

#extract_mean_from_raster

#extract_mean_from_raster is a function to extract the average value of a raster over each polygon in a shapefile. It is 
#essentially a wrapper for the extract function in the raster package. The function needs to be provided with an sf 
#shapefile, and will return an sf shapefile

#ARGUMENTS
#raster: is a raster file stored as a "Formal class RasterLayer" - load using the raster function. Check projections. 
#polygon: a polygon shapefile stored as a sf - load using st_read - need to ensure there are no null geometries


extract_mean_from_raster<-function(raster, polygon){
  r.vals <- raster::extract(raster, polygon) #extract values of raster
  r.mean <- unlist(lapply(r.vals, FUN=mean)) #take mean for each polygon
  
  polygon$mean <- r.mean #merge mean back in to polygon
  polygon
}