# This script calculates cluster specific sample sizes for the 
# catastrophic costs in TB project.  It borrows heavily from Aina's.

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

# Define function to read in the data from the xls files
read_spreadsheet <- function(file_name  = 'Not.C.Delgado 2013.xls',
                             location = 'Niassa',
                             sheet = 'anual'){
  # Read spreadsheet
  temp <- read_excel(file_name, skip = 3, sheet = sheet)
  # Define which row aggregate data begins (we're not interested in that)
  start_bad <- which(temp$DISTRITOS == 'Total')
  # Subset to keep only raw data
  temp <- temp[1:(start_bad - 1),]
  # Define which columns we're keeping
  keepers <- c('DISTRITOS', 'POPULAÇÃO', 'TOTAL Casos Novos')
  # Select only our columns of interest
  temp <- temp[,keepers]
  # Add column with the location
  temp$location <- location
  # Rename columns
  names(temp) <- c('health_center', 
                   'pop',
                   'casos', 
                   'district')
  # Spit back
  return(temp)
}

# Read in the excel spreadsheets
setwd('data')
files <- dir()
files <- files[grepl('.xls', files)]
files <- files[files != 'Not.Nacional 2013.xls']
locations <- c('Cabo Delgado',
               'Maputo City',
               'Gaza',
               'Inhambane',
               'Manica',
               # 'Nacional',
               'Nampula',
               'Nassa',
               'Maputo',
               'Sofala',
               'Tete',
               'Zambezia')
results_list <- list()
for (i in 1:length(files)){
  cat(paste0('FILE NUMBER ', i, '------------\n'))
  results_list[[i]] <- read_spreadsheet(file_name = files[i],
                                        location = locations[i],
                                        sheet = 'anual')
}
setwd(root)

# Bind together all the data from the different locations
tb <- do.call(rbind, results_list)

# Correct data types and add a rate
tb$pop <- as.numeric(tb$pop)
tb$casos <- as.numeric(tb$casos)
tb$rate <- tb$casos / tb$pop

##### Associate tb data with the spatial data

# Confirm that our names match in the two datasets
all(unique(sort(tb$district)) == unique(sort(moz1f$id)))

# Group together the tb data by district
tb_district <- tb %>%
  group_by(district) %>%
  summarise(pop = sum(pop, na.rm = TRUE),
            casos = sum(casos, na.rm = TRUE)) %>%
  mutate(rate = casos / pop)

# Bring district-level data into spatial map
moz1f <- 
  moz1f %>%
  mutate(district = id) %>%
  left_join(tb_district, by = 'district')

##### Geocode at a more granular level
setwd('data/spatial/')
if('locations_geocoded.RData' %in% dir()){
  load('locations_geocoded.RData')
} else {
  locations <- paste0(tb$health_center, ', ', tb$district, ', Mozambique')
  locations_geocoded <- geocode(locations, source = 'google')
#   locations_geocoded[is.na(locations_geocoded$lon)] <- 
#     geocode(locations[is.na(locations_geocoded$lon)], source = 'google')
  save.image('locations_geocoded.RData')
}
setwd(root)


