# This script calculates cluster specific sample sizes for the 
# catastrophic costs in TB project.  It borrows heavily from Aina's.

##### Libraries and the like
library(raster)
library(dplyr)
library(ggplot2)
library(ggmap)
library(readxl)

##### Directories
root <- getwd()

##### Mozambique spatial data

# Get data from GADM
moz3 <- getData('GADM', country = 'MOZ', level = 3) # localidade
moz2 <- getData('GADM', country = 'MOZ', level = 2) # district
moz1 <- getData('GADM', country = 'MOZ', level = 1) # province
moz0 <- getData('GADM', country = 'MOZ', level = 0) # country

# Fortify data into data.frame format
moz3f <- fortify(moz3, region = 'NAME_3')
moz2f <- fortify(moz3, region = 'NAME_2')
moz1f <- fortify(moz3, region = 'NAME_1')
moz0f <- fortify(moz3, region = 'NAME_0')

# Map example
# ggplot(data = moz0f,
#        aes(x = long, y = lat)) +
#   coord_map(projection = 'cylindrical') +
#   # Country
#   geom_polygon(aes(group = piece), fill = 'white', color = 'black') +
#   # District
#   geom_polygon(data = moz1f, 
#                aes(x = long, y = lat, group = group),
#                fill = NA, color = 'black', alpha = 0.8, size = 1.05) +
#   # District
#   geom_polygon(data = moz2f, 
#                aes(x = long, y = lat, group = group),
#                fill = NA, color = adjustcolor('black', alpha.f = 0.5)) +
#   # Localidade
#   geom_polygon(data = moz3f, 
#                aes(x = long, y = lat, group = group),
#                fill = NA, color = adjustcolor('red', alpha.f = 0.2)) +
#   # geom_polygon(aes(group = id), fill = 'grey', color = 'darkgrey') +
#   xlab('Longitude') + ylab('Latitude')

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
  names(temp) <- c('distrito', 
                   'pop',
                   'casos', 
                   'provincia')
  # Spit back
  return(temp)
}

# Read in the excel spreadsheets
setwd('data')
files <- dir()
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

# Bind together all the data from the different locations
tb <- do.call(rbind, results_list)

# Correct data types
tb$pop <- as.numeric(tb$pop)
tb$casos <- as.numeric(tb$casos)

# Add a rate
tb$rate <- tb$casos / tb$pop

##### Associate tb data with the spatial data
