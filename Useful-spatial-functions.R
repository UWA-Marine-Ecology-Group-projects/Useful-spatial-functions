require(raster)
require(tidyverse)
require(devtools)
require(sf)
require(magrittr)
require(ncdf4)
require(lubridate)

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
    
    crs(raster)<- CRS('+init=EPSG:4326')#in WGS84
    
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
get_sst_OneMonthAverage<- function(Dates, Long, Lat){
  
  Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
    sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>% #only extract for points on this day
    st_transform(3112) %>%
    mutate(ID = row_number())
  
  #ggplot() +  geom_sf(data = Australia_map_Lambert) + geom_sf(data = Points) 
  
  Points %<>% mutate(land = as.integer(sf::st_intersects(geometry, Australia_map_Lambert))) #returns NA for water
  
  Points_extraction <- Points %>% filter(is.na(land))
  Date_code <- as.character(Points_extraction$Dates)
  
  Date_code %<>% str_replace_all(., "-", "") %>% unique() #Extra Date codes for each day desired
  
  URL_front_part <- "http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-1m/ngt/"
  for(i in 1:length(Date_code)) { #loop through days download and extract data
    if(substr(Date_code[i], 5,6) %in% c("01", "03", "05", "07", "08", "10", "12")) {
      file_URL<- paste0(URL_front_part, substr(Date_code[i], 1,4), "/", substr(Date_code[i], 1,6), "31",
                        "152000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
    
    if(substr(Date_code[i], 5,6) %in% c("04", "06", "09", "11")) {
      file_URL<- paste0(URL_front_part, substr(Date_code[i], 1,4), "/", substr(Date_code[i], 1,6), "30",
                        "032000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
    
    if(substr(Date_code[i], 5,6) %in% c("02") & substr(Date_code[i], 1,4) %in% seq(1980,2100, 4)) { #leap year
      file_URL<- paste0(URL_front_part, substr(Date_code[i], 1,4), "/", substr(Date_code[i], 1,6), "29",
                        "152000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
    
    if(substr(Date_code[i], 5,6) %in% c("02") & !substr(Date_code[i], 1,4) %in% seq(1980,2100, 4)) {
      file_URL<- paste0(URL_front_part, substr(Date_code[i], 1,4), "/", substr(Date_code[i], 1,6), "28",
                        "032000-ABOM-L3S_GHRSST-SSTskin-AVHRR_D-1m_night.nc")}
    
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
    
    crs(raster)<- CRS('+init=EPSG:4326')#in WGS84
    
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

#get_hs_ws

#get_hs_ws is a function to extracts monthly mean significant wave heights and wind speeds given a series of Dates and points 
#(Latitudes and Longitude). The function also needs a map (polygon) with which to screen land and non-land points projected to 
#Australian lambert. 
#The function returns a a list of 2 lists (one for hs and one for ws) corresponding to the input Dates and points.
#hs and ws is extracted from the CSIRO https://data.csiro.au/collections/#collection/CIcsiro:39819. 
#The extraction function searches for data within 
#100 and then 10000 and then 100000m from the point. Failing that it returns a warning

#ARGUMENTS
#Dates: is a data vector (created using as.Date) with format YYYY-MM-DD
#Latitude: is the latitude of the point files in GDA94
#Longitude is the longitude of the pont files in GDA94
#map is a polygon map with which to screen land and non-land points in Australian lambert: 3112 is Australian Lambert (Main projected csr)

get_hs_ws<-function(Dates, Long, Lat, map){
  
  Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
    sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>% #only extract for points on this day
    st_transform(3112) %>%
    mutate(ID = row_number())
  
  Points %<>% mutate(land = as.integer(sf::st_intersects(geometry, map))) #returns NA for water
  
  Points_extraction <- Points %>% filter(is.na(land))
  Date_code <- as.character(Points_extraction$Dates)
  
  Date_code %<>% str_replace_all(., "-", "") %>% unique() #Extra Date codes for each day desired
  Month_code <- Date_code %>%  substr(., 1, 6) %>% unique() #Extra Date codes for each day desired
  
  for(i in 1:length(Month_code)) { #loop through months to get right data
    
    file_URL<- paste0("http://data-cbr.csiro.au/thredds/dodsC/catch_all/CMAR_CAWCR-Wave_archive/CAWCR_Wave_Hindcast_aggregate/gridded/ww3.aus_4m.",
                      Month_code[i], ".nc")
    
    nc<-nc_open(file_URL) #open months data
    
    hs <- nc$var[[c("hs")]] #getthe variable you want
    uwnd <- nc$var[[c("uwnd")]] #getthe variable you want
    vwnd <- nc$var[[c("vwnd")]] #getthe variable you want
    varsize <- hs$varsize #save dimensions
    ndims <- hs$ndims
    nt <- varsize[ndims] #save length of time dimension Note: its always the last one
    
    for( j in 1:nt ) { #looping through time steps in month.
      # Initialize start and count to read one timestep of the variable.
      start <- rep(1,ndims) ; start[ndims] <- j
      count <- varsize ; count[ndims] <- 1
      hs_data <- ncvar_get( nc, hs, start=start, count=count )
      uwnd_data <- ncvar_get( nc, uwnd, start=start, count=count )
      vwnd_data <- ncvar_get( nc, vwnd, start=start, count=count )
      
      # Now read in the value of the timelike dimension
      timeval <- ncvar_get( nc, hs$dim[[ndims]]$name, start=j, count=1 )
      nc_lat <- ncvar_get( nc, hs$dim[[2]]$name)
      nc_lon <- ncvar_get( nc, hs$dim[[1]]$name)
      
      dimnames(hs_data) <- list(lon=nc_lon, lat=nc_lat) ; dimnames(uwnd_data) <- list(lon=nc_lon, lat=nc_lat) 
      dimnames(vwnd_data) <- list(lon=nc_lon, lat=nc_lat)  #adding labels to matrix
      data_out <- na.omit(melt(hs_data)) 
      data_out$uwnd<-na.omit(melt(uwnd_data))$value
      data_out$vwnd<-na.omit(melt(vwnd_data))$value
      data_out$wspeed<-sqrt(data_out$value^2 + data_out$vwnd^2)
      
      data_out %<>% select(lon, lat, value, wspeed) %>% rename(hs = value)
      
      data_out$time <- timeval
      
      if(j==1) { data5<-data_out} else{ data5<-rbind(data5, data_out)}
    }
    data5 %<>% group_by(lon, lat) %>% summarise(hs = mean(hs), wspeed = mean(wspeed), #take average for the month
                                                date = as.Date(as.POSIXct(min(time)*86400, origin="1990-01-01")))
    
    ## Create a raster of longitude, latitude, and temperature data
    dat1 <- list( )
    dat1$x <- c( data5$lon)
    dat1$y <- c( data5$lat)
    dat1$hs <- t( data5$hs)
    dat1$wspeed <- t( data5$wspeed)
    raster_hs <- raster( dat1$hs, xmn = range( dat1[[1]])[1], xmx = range( dat1[[1]])[2], ymn = range( dat1[[2]])[1], ymx = range( dat1[[2]])[2])
    raster_wspeed <- raster( dat1$wspeed, xmn = range( dat1[[1]])[1], xmx = range( dat1[[1]])[2], ymn = range( dat1[[2]])[1], ymx = range( dat1[[2]])[2])
    
    crs(raster_hs)<- CRS('+init=EPSG:4326')#in WGS84
    crs(raster_wspeed)<- CRS('+init=EPSG:4326')
    
    #Get points ready
    temp <- Points_extraction %>% mutate(Dates2 = str_replace(substr(Dates, 1, 7), "-", "")) %>% 
      filter(Dates2 == Month_code[i]) %>% st_transform(4326) #NEED TO MODIFY TO MATCH DATE TO THAT MONTHS DATA
    
    temp$hs.100<-raster::extract(raster_hs, temp, buffer = 100, fun=mean)
    temp$hs.10000<-raster::extract(raster_hs, temp, buffer = 10000, fun=mean)
    temp$hs.100000<-raster::extract(raster_hs, temp, buffer = 100000, fun=mean)
    temp$hs <-ifelse(is.na(temp$hs.100), ifelse(is.na(temp$hs.10000), temp$hs.100000, temp$hs.10000), temp$hs.100)
    
    temp$ws.100<-raster::extract(raster_wspeed, temp, buffer = 100, fun=mean)
    temp$ws.10000<-raster::extract(raster_wspeed, temp, buffer = 10000, fun=mean)
    temp$ws.100000<-raster::extract(raster_wspeed, temp, buffer = 100000, fun=mean)
    temp$ws <-ifelse(is.na(temp$ws.100), ifelse(is.na(temp$ws.10000), temp$ws.100000, temp$ws.10000), temp$ws.100)
    
    if(i==1){points_extracted<-temp}else{ points_extracted<-rbind(temp,points_extracted)}
  }
  points_extracted %<>% st_drop_geometry()
  if( any(is.na(points_extracted$hs | points_extracted$ws))) warning('raster extract returned NA consider increasing bufffer size')
  Points %<>% left_join(.,points_extracted[, c("ID", "hs", "ws")], by = c("ID" = "ID"))
  out<-list(Points$hs, Points$ws); names(out)<-c("hs", "ws")
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