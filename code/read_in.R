##### Directories
root <- getwd()

##### Mozambique spatial data

# Get data from GADM
setwd('data/spatial')
moz3 <- getData('GADM', country = 'MOZ', level = 3) # localidade
moz2 <- getData('GADM', country = 'MOZ', level = 2) # district
moz1 <- getData('GADM', country = 'MOZ', level = 1) # province
moz0 <- getData('GADM', country = 'MOZ', level = 0) # country
setwd(root)

# Fortify data into data.frame format
moz3f <- fortify(moz3, region = 'NAME_3')
moz2f <- fortify(moz3, region = 'NAME_2')
moz1f <- fortify(moz3, region = 'NAME_1')
moz0f <- fortify(moz3, region = 'NAME_0')
##### Merge data on previous notification rates (2013) 
##### to the spatial data

##### Geocode at a more granular level
setwd('data/spatial/')
if('locations_geocoded.RData' %in% dir()){
  load('locations_geocoded.RData')
} else {
  locations <- paste0(tb$health_center, ', ', tb$district, ', Mozambique')
  locations_geocoded <- geocode(locations, source = 'google')
#   locations_geocoded[is.na(locations_geocoded$lon)] <- 
#     geocode(locations[is.na(locations_geocoded$lon)], source = 'google')
  save('locations_geocoded', file = 'locations_geocoded.RData')
}
setwd(root)


