# Written by Felicity Charles
# 02/10/2025
# Caveat emptor


## This script gathers together environmental data needed for Brush-tailed rock wallaby connectivity analyses

# R version 4.5.1

# Load required packages ----
library(terra) # terra_1.8-70
library(dplyr) # dplyr_1.1.4 
library(httr) # httr_1.4.7
library(raster) # raster 3.6-32
library(sf) # s3 1.0-21

# Install gdalUtilities for working with large datasets
#library(devtools)
#install_github("JoshOBrien/gdalUtilities")
library(gdalUtilities) # gdalUtilities_1.2.5

# 1. Determine region of interest
BTRW_pres <- read.csv('./00_Data/BTRW_data/1_BTRW_Records_All_Combined_Hi_Prec.csv', header = T)
head(BTRW_pres)

# Convert to spatial dataframe
BTRW_sf <- st_as_sf(BTRW_pres, crs = 'EPSG:4326', coords = c('Longitude', 'Latitude'))
BTRW <- vect(BTRW_sf) # Convert to spatVector for better visualisation
plet(BTRW)
e <- vect(ext(BTRW), crs = 'EPSG:4326') # Get coverage of presence points
plet(e) # Visualise coverage of presence points
# Need to extend this boundary box to include coastal areas


# Load Australian outline
Aus <- download.file("https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/STE_2021_AUST_SHP_GDA2020.zip", destfile = './00_Data/Australia_shapefile.zip', mode = "wb", cacheOK = F)
unzip(zipfile = './00_Data/Australia_shapefile.zip', exdir = './00_Data/Australia_shapefile')
Aus <- vect('./00_Data/Australia_shapefile/STE11aAust.shp') %>% 
  project("EPSG:4326")

QLD <- Aus[Aus$STATE_NAME == "Queensland"]
QLD 

# Load habitat suitability for BTRW and look at the extent
hsm <- vect('./00_Data/BTRW_data/DES HSM_Petrogale penicillata/DES HSM_Petrogale penicillata.shp') %>% 
  project('EPSG:4326')
hsm
plet(vect(ext(hsm)))

# Create an extended extent area of interest for BTRW
extended_e <- vect(ext(149.8, 153.65, -29.4, -25.7), crs = 'EPSG:4326') %>%  # Making this slightly larger than presence points and habitat suitability mapping for BTRW as we can reduce the spatial extent later if required. 
  project('EPSG:3577') # Change to CRS which is measured in metres
plet(extended_e)
extended_e
# Wondering whether we need a larger buffer at the bottom? but this is over the NSW border

writeVector(extended_e, './00_Data/BTRW_data/BTRW_pres_ext.gpkg', overwrite = T)

aoi <- vect('./00_Data/BTRW_data/BTRW_pres_ext.gpkg')

# Presence point data ranges from 1990 to 2025





# 2. Read in, crop and aggregate environmental data ----
# 2.1 Rainfall ----
# NOTE: For the SILO data when download fails and an error is returned, it is advised to adjust the start year and re-run the download code until all years have been downloaded

# 1.1.1 Load daily rainfall data
load_SILO_rain <- function(year, dest_dir = './00_Data/Environmental_data/SILO_Rainfall'){
  if(!dir.exists(dest_dir)) dir.create(dest_dir)
  
  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/daily_rain"
  file_name <- paste0(year, ".daily_rain.nc")
  url <- file.path(base_url, file_name)
  local_file <- file.path(dest_dir, file_name)
  
  # Download file
  download.file(url, local_file, mode = 'wb') # Method = 'wb' preserves the byte content of the original file
  
  # Load raster
  r <- rast(local_file)
  r[r < 0] <- NA # SILO uses values like -32768 as values for missing data which means these values are not recognised as NAs during reprojection. 
  r <- project(r, 'EPSG:3577') %>% 
    crop(e)
  assign(paste0("rain_", year), r, envir = .GlobalEnv) # Add rasters to the environment as they are processed
}

# Loop over 1990 to 2024
options(timeout = 400)
e <- aoi 
years <- 1990:2024 # Only data up until December 2024 is available
invisible(lapply(years, load_SILO_rain))
rain_1990; plot(rain_1990)


# 1.1.2 Calculate rainfall average
rain_list <- list(rain_1990, rain_1991, rain_1992, rain_1993, rain_1994, rain_1995, rain_1996, rain_1997, rain_1998, rain_1999, rain_2000, rain_2001, rain_2002, rain_2003, rain_2004, rain_2005, rain_2006, rain_2007, rain_2008, rain_2009, rain_2010, rain_2011, rain_2012, rain_2013, rain_2014, rain_2015, rain_2016, rain_2017, rain_2018, rain_2019, rain_2020, rain_2021, rain_2022, rain_2023, rain_2024)

years <- 1990:2024

for(i in seq_along(rain_list)){
  
  rain <- rain_list[[i]]
  year <- years[i]
  
  year_avg <- mean(rain, na.rm = T)
  assign(paste0('avg_rain', year), year_avg)
  writeRaster(year_avg, paste0('./00_Data/Environmental_data/Outputs/SILO_Rainfall/avg_rain_', year, ".tif"), overwrite = T)
}
avg_rain1990; avg_rain2000 # Check
plot(avg_rain1990)

# Calculate long-term average
rain_average <- mean(avg_rain1990, avg_rain1991, avg_rain1992, avg_rain1993, avg_rain1994, avg_rain1995, avg_rain1996, avg_rain1997, avg_rain1998, avg_rain1999, avg_rain2000, avg_rain2001, avg_rain2002, avg_rain2003, avg_rain2004, avg_rain2005, avg_rain2006, avg_rain2007, avg_rain2008, avg_rain2009, avg_rain2010, avg_rain2011, avg_rain2012, avg_rain2013, avg_rain2014, avg_rain2015, avg_rain2016, avg_rain2017, avg_rain2018, avg_rain2019, avg_rain2020, avg_rain2021, avg_rain2022, avg_rain2023, avg_rain2024)
plot(rain_average); rain_average

writeRaster(rain_average, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi.tif', overwrite = T)

rain_averager <- resample(rain_average, rtemp, method = 'bilinear')
rain_averager
writeRaster(rain_averager, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj.tif', overwrite = T)




# Calculate average for two time periods, drought period and flooding period

rain90_10 <- mean(avg_rain1990, avg_rain1991, avg_rain1992, avg_rain1993, avg_rain1994, avg_rain1995, avg_rain1996, avg_rain1997, avg_rain1998, avg_rain1999, avg_rain2000, avg_rain2001, avg_rain2002, avg_rain2003, avg_rain2004, avg_rain2005, avg_rain2006, avg_rain2007, avg_rain2008, avg_rain2009, avg_rain2010)
rain90_10
writeRaster(rain90_10, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010.tif', overwrite = T)

rain90_10r <- resample(rain90_10, rtemp, method = 'bilinear')
rain90_10r
writeRaster(rain90_10r, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj.tif', overwrite = T)

rain11_24 <- mean(avg_rain2011, avg_rain2012, avg_rain2013, avg_rain2014, avg_rain2015, avg_rain2016, avg_rain2017, avg_rain2018, avg_rain2019, avg_rain2020, avg_rain2021, avg_rain2022, avg_rain2023, avg_rain2024)
rain11_24
writeRaster(rain11_24, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024.tif', overwrite = T)

rain11_24r <- resample(rain11_24, rtemp, method = 'bilinear')
rain11_24r
writeRaster(rain11_24r, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj.tif', overwrite = T)





# 2.2 Temperature ----
# 2.2.1 Min temperature
load_SILO_mintemp <- function(year, dest_dir = './00_Data/Environmental_data/SILO_Min_temp'){
  if(!dir.exists(dest_dir)) dir.create(dest_dir)
  
  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/min_temp"
  file_name <- paste0(year, ".min_temp.nc")
  url <- file.path(base_url, file_name)
  local_file <- file.path(dest_dir, file_name)
  
  # Download file
  download.file(url, local_file, mode = 'wb') # Method = 'wb' preserves the byte content of the original file
  
  # Load raster
  r <- rast(local_file)
  r[r < 0] <- NA # SILO uses values like -32768 as values for missing data which means these values are not recognised as NAs during reprojection. 
  r <- project(r, 'EPSG:3577') %>% 
    crop(e)
  assign(paste0("mintemp_", year), r, envir = .GlobalEnv) # Add rasters to the environment as they are processed
  writeRaster(r, paste0('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/aoi_mintemp_', year, '.tif'), overwrite = T)
}
# Loop over 1990 to 2024
options(timeout = 400)
e <- aoi
years <- 1990:2024 
invisible(lapply(years, load_SILO_mintemp))
mintemp_1990; plot(mintemp_1990) # Values are per day


mintemp_list <- list(mintemp_1990, mintemp_1991, mintemp_1992, mintemp_1993, mintemp_1994, mintemp_1995, mintemp_1996, mintemp_1997, mintemp_1998, mintemp_1999, mintemp_2000, mintemp_2001, mintemp_2002, mintemp_2003, mintemp_2004, mintemp_2005, mintemp_2006, mintemp_2007, mintemp_2008, mintemp_2009, mintemp_2010, mintemp_2011, mintemp_2012, mintemp_2013, mintemp_2014, mintemp_2015, mintemp_2016, mintemp_2017, mintemp_2018, mintemp_2019, mintemp_2020, mintemp_2021, mintemp_2022, mintemp_2023, mintemp_2024)

# Calculate yearly average
years <- 1990:2024
for(i in seq_along(mintemp_list)){
  
  mintemp <- mintemp_list[[i]]
  year <- years[i]
  
  year_avg <- mean(mintemp, na.rm = T)
  assign(paste0('avg_mintemp', year), year_avg)
  writeRaster(year_avg, paste0('./00_Data/Environmental_data/Outputs/SILO_min_Temp/avg_mintemp_', year, ".tif"), overwrite = T)
}
avg_mintemp1990; avg_mintemp2000 # Check

# Calculate 1990 to 2024 average
avg_mintemp <- mean(avg_mintemp1990, avg_mintemp1991, avg_mintemp1993, avg_mintemp1994, avg_mintemp1995, avg_mintemp1996, avg_mintemp1997, avg_mintemp1998, avg_mintemp1999, avg_mintemp2000, avg_mintemp2001, avg_mintemp2002, avg_mintemp2003, avg_mintemp2004, avg_mintemp2005, avg_mintemp2006, avg_mintemp2007, avg_mintemp2008, avg_mintemp2009, avg_mintemp2010, avg_mintemp2011, avg_mintemp2012, avg_mintemp2013, avg_mintemp2014, avg_mintemp2015, avg_mintemp2016, avg_mintemp2017, avg_mintemp2018, avg_mintemp2019, avg_mintemp2020, avg_mintemp2021, avg_mintemp2022, avg_mintemp2023, avg_mintemp2024)
avg_mintemp # Check
writeRaster(avg_mintemp, './00_Data/Environmental_data/Outputs/SILO_min_Temp/Average_min_temp_aoi.tif', overwrite = T)

# Resample
rtemp <- rast(xmin = 1702105, xmax = 2137037, ymin = -3371610, ymax = -2903819, res = 30, crs = 'EPSG:3577') # Use the extent of the area of interest to create the template raster
avg_mintempr <- resample(avg_mintemp, rtemp, method = 'bilinear')
avg_mintempr
writeRaster(avg_mintempr, './00_Data/Environmental_data/Outputs/SILO_min_Temp/Average_min_temp_aoi_reproj.tif', overwrite = T)





# 2.2.2 Max temperature
load_SILO_maxtemp <- function(year, dest_dir = './00_Data/Environmental_data/SILO_max_temp'){
  if(!dir.exists(dest_dir)) dir.create(dest_dir)
  
  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/max_temp"
  file_name <- paste0(year, ".max_temp.nc")
  url <- file.path(base_url, file_name)
  local_file <- file.path(dest_dir, file_name)
  
  # Download file
  download.file(url, local_file, mode = 'wb') # Method = 'wb' preserves the byte content of the original file
  
  # Load raster
  r <- rast(local_file)
  r[r < 0] <- NA # SILO uses values like -32768 as values for missing data which means these values are not recognised as NAs during reprojection. 
  r <- project(r, 'EPSG:3577') %>% 
    crop(e)
  assign(paste0("maxtemp_", year), r, envir = .GlobalEnv) # Add rasters to the environment as they are processed
  writeRaster(r, paste0('./00_Data/Environmental_data/Outputs/SILO_max_Temp/aoi_maxtemp_', year, '.tif'), overwrite = T)
}
# Loop over 1990 to 2024
options(timeout = 400)
e <- aoi
years <- 1990:2024 
invisible(lapply(years, load_SILO_maxtemp))
maxtemp_1990; plot(maxtemp_1990) # Values are per day


maxtemp_list <- list(maxtemp_1990, maxtemp_1991, maxtemp_1992, maxtemp_1993, maxtemp_1994, maxtemp_1995, maxtemp_1996, maxtemp_1997, maxtemp_1998, maxtemp_1999, maxtemp_2000, maxtemp_2001, maxtemp_2002, maxtemp_2003, maxtemp_2004, maxtemp_2005, maxtemp_2006, maxtemp_2007, maxtemp_2008, maxtemp_2009, maxtemp_2010, maxtemp_2011, maxtemp_2012, maxtemp_2013, maxtemp_2014, maxtemp_2015, maxtemp_2016, maxtemp_2017, maxtemp_2018, maxtemp_2019, maxtemp_2020, maxtemp_2021, maxtemp_2022, maxtemp_2023, maxtemp_2024)

# Calculate yearly average
years <- 1990:2024
for(i in seq_along(maxtemp_list)){
  
  maxtemp <- maxtemp_list[[i]]
  year <- years[i]
  
  year_avg <- mean(maxtemp, na.rm = T)
  assign(paste0('avg_maxtemp', year), year_avg)
  writeRaster(year_avg, paste0('./00_Data/Environmental_data/Outputs/SILO_max_Temp/avg_maxtemp_', year, ".tif"), overwrite = T)
}
avg_maxtemp1990; avg_maxtemp2000 # Check

# Calculate 1990 to 2024 average
avg_maxtemp <- mean(avg_maxtemp1990, avg_maxtemp1991, avg_maxtemp1993, avg_maxtemp1994, avg_maxtemp1995, avg_maxtemp1996, avg_maxtemp1997, avg_maxtemp1998, avg_maxtemp1999, avg_maxtemp2000, avg_maxtemp2001, avg_maxtemp2002, avg_maxtemp2003, avg_maxtemp2004, avg_maxtemp2005, avg_maxtemp2006, avg_maxtemp2007, avg_maxtemp2008, avg_maxtemp2009, avg_maxtemp2010, avg_maxtemp2011, avg_maxtemp2012, avg_maxtemp2013, avg_maxtemp2014, avg_maxtemp2015, avg_maxtemp2016, avg_maxtemp2017, avg_maxtemp2018, avg_maxtemp2019, avg_maxtemp2020, avg_maxtemp2021, avg_maxtemp2022, avg_maxtemp2023, avg_maxtemp2024)
avg_maxtemp # Check

writeRaster(avg_maxtemp, './00_Data/Environmental_data/Outputs/SILO_max_Temp/Average_max_temp_aoi.tif', overwrite = T)

avg_maxtempr <- resample(avg_maxtemp, rtemp, method = 'bilinear')
avg_maxtempr
writeRaster(avg_maxtempr, './00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj.tif', overwrite = T)





# 2.3 Vegetation information ----

# 2.3.1 NDVI ----
# Download BOM NDVI data for May 1992 to December 2018 from https://www.bom.gov.au/jsp/awap/ndvi/index.jsp?colour=colour&map=ndviave&year=2018&month=8&period=month&area=nat

e <- aoi 
years <- 1992:2019
months <- c("January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December")
template <- rast(xmin  = 1702104, xmax = 2137037, ymin = -3371609, ymax = -2903818, crs = 'EPSG:3577', res = 30)



# Loop through all year-month combinations for NDVI
terraOptions(memfrac = 0.8, progress = 2)

for (year in years) {
  monthly_rasters <- list()
  
  for (month in months) {
    # Skip months before May 1992
    if (year == 1992 & month %in% c("January", "February", "March", "April")) next 
    
      # Create file path
      file_path <- file.path("./00_Data/Environmental_data/BOM_NDVI", paste0(year, "_", month))
      
      # Check if file exists and process
      if (file.exists(file_path)) {
        # Load raster from first extracted file and crop
        r <- rast(file_path)
        r[r < -100] <- NA # NA values coded as -999 or -9999
        monthly_rasters[[month]] <- crop(project(r, template, method = 'bilinear'), e)
        } 
  }
  # Calculate yearly average from monthly rasters and save to disk
  if (length(monthly_rasters) > 0) {
    yearly_mean <- mean(rast(monthly_rasters), na.rm = TRUE)
    writeRaster(yearly_mean, paste0("./00_Data/Environmental_data/Outputs/BOM_NDVI/NDVI_", year, "_average.tif"), overwrite = TRUE)
  }
  
  tmpFiles(remove = TRUE)
  gc()
}
years <- 1992:2019
annual_rasters <- rast(paste0("./00_Data/Environmental_data/Outputs/BOM_NDVI/NDVI_", years, "_average.tif"))
annual_rasters # Check

# Calculate average NDVI
avg_NDVI <- mean(annual_rasters)
avg_NDVI; plot(avg_NDVI)

writeRaster(avg_NDVI, './00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi.tif')


# 2.3.2 Broad vegetation groups ----
##### Use the NVIS Major vegetation group data 
# NVIS data progresses from dense closed vegetation systems of rainforest and vine thickets to progressively more open low vegetation systems, denser systems = more resistant to movement.
#NOTE: The NVIS data was used when the decision was made to work with data at 100m resolution
list.files('./00_Data/Environmental_data/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb/')
describe('./00_Data/Environmental_data/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb')
NVIS <- rast('./00_Data/Environmental_data/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb', lyrs = "NVIS7_0_AUST_EXT_MVG_ALB")
plot(NVIS); NVIS

NVIS_aoi <- crop(project(NVIS, 'EPSG:3577'), aoi)
plot(NVIS_aoi); NVIS_aoi
# Remove sea and estuaries 
cats(NVIS_aoi)[[1]]
NVIS_aoi <- ifel(NVIS_aoi == "Sea and estuaries", NA, NVIS_aoi)
plot(NVIS_aoi)

# Convert to numeric
NVIS_aoi_numeric <- as.numeric(NVIS_aoi)
plot(NVIS_aoi_numeric)
writeRaster(NVIS_aoi_numeric, './00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi.tif')

# 2.4 Elevation data ----
# Download the DEM data file from https://data.gov.au/data/dataset/9a9284b6-eb45-4a13-97d0-91bf25f1187b/resource/7653b920-5334-4267-8df3-5d55b21f05ec and unzip the file using WinZip, native archive management software is not sufficient.



# Create the QLD polygon for cropping
# Read in while cropping the DEM data to QLD and NSW as our aoi extends into northern NSW
Aus <- vect('./00_Data/Australia_shapefile/STE11aAust.shp') %>% 
  project("EPSG:4326")

QN <- Aus[Aus$STATE_NAME == "Queensland" | Aus$STATE_NAME == "New South Wales"]
QN 

DEMq <- rast('./00_Data/Environmental_data/DEM/srtm-1sec-dem-v1-COG.tif') %>% 
  crop(QN)
NAflag(DEMq) # NAn, should be handled correctly

DEM <- project(DEMq, 'EPSG:3577') # Instead of cropping here, we will crop while using gdalwarp
plet(DEM)
DEM; DEMq

writeRaster(DEM, './00_Data/Environmental_data/Outputs/DEM/QLD_DEM.tif')


gdalwarp(srcfile = './00_Data/Environmental_data/Outputs/DEM/QLD_DEM.tif',
         dstfile = './00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj.tif',
         tr = c(30,30),
         r = 'bilinear',
         te = c(1702105, -3371610, 2137037, -2903819))


DEM <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj.tif') 
DEM
plet(DEM)


# 2.5.1 Calculate slope, aspect and topographic position index (TPI) ----
slope <- terrain(DEM, v = "slope", unit = "degrees")
slope; plot(slope) #Check
writeRaster(slope, './00_Data/Environmental_data/Outputs/DEM/slope_aoi.tif') # Save output


aspect <- terrain(DEM, v = "aspect")
plot(aspect); aspect
writeRaster(aspect, './00_Data/Environmental_data/Outputs/DEM/aspect_aoi.tif')

ruggedness <- terrain(DEM, v = 'TRI')
plot(ruggedness); ruggedness
writeRaster(ruggedness, './00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi.tif', overwrite = T)



# 2.6 Land use ----
land_use <- rast('./00_Data/Environmental_data/NLUM_v7_250_ALUMV8_2020_21_alb_package_20241128/NLUM_v7_250_ALUMV8_2020_21_alb.tif')
land_use$SIMPN
par(mfrow = c(1,2)); plot(land_use$SIMP); plot(land_use$SIMPN) # Landuse is organised from the most natural to most intensively used area, with this reflected correctly in the SIMPN numerical coding
land_usec <- crop(land_use, aoi)
land_usec$SIMPN
# The data was formatted in the same way as the NSW BVG data with categories instead of additional layers meaning that to be able to save the numerical simplified landuse categories we need to activate the layer that we want  layers for each category of the raster.
activeCat(land_usec) <- 'SIMPN'
landuse_c <- catalyze(land_usec)
land <- rast(landuse_c$SIMPN, vals = values(landuse_c$SIMPN))
land
plot(land)
writeRaster(land, './00_Data/Environmental_data/Outputs/Landuse/aoi_landuse.tif', overwrite = T)

rtemp <- rast(xmin = 1702105, xmax = 2137037, ymin = -3371610, ymax = -2903819, res = 30, crs = 'EPSG:3577') # Use the extent of the area of interest to create the template raster
landuse_disagg <- resample(land, rtemp, method = 'mode') # For categorical variabels such as landuse we want the most common cell value
landuse_disagg
plet(landuse_disagg)

writeRaster(landuse_disagg, './00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated.tif', overwrite = T)


# 2.7 Building areas ----
# Previously had built-up areas but this does not give us information on every building that exists and onyl provided information on high density built-up areas which weren't all that useful
building <- vect('./00_Data/Environmental_data/Building_points/Building_points.shp') %>% 
  project('EPSG:3577') %>% 
  crop(aoi)
building; plot(building)


# Convert to raster
temp <- rast(xmin = 1702105, xmax = 2137037, ymin = -3371610, ymax = -2903819, crs = 'EPSG:3577') # Do not specify resolution so features are preserved during rasterisation process
build_rast <- rasterize(building, temp, background = NA)
build_rast; plet(build_rast)
inv_build_rast <- classify(build_rast, cbind(NA, 1), others = 0.0001)
plot(inv_build_rast)
writeRaster(build_rast, './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas.tif')

build_agg <- resample(inv_build_rast, rtemp, method = 'mode')
plet(build_agg); build_agg
writeRaster(build_agg, './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated.tif')



# 2.8 Road infrastructure data ----
road <- vect('./00_Data/Environmental_data/Roads_and_tracks/Queensland_roads_and_tracks.shp') %>% 
  project('EPSG:3577') %>% 
  crop(aoi)
road; plot(road)
plet(road)

roads <- road[!is.na(road$road_type)]
plet(roads)


temp <- rast(xmin = 1702105, xmax = 2137037, ymin = -3371610, ymax = -2903819, crs = 'EPSG:3577') # Do not specify resolution so features are preserved in rasterisation process
road_rast <- rasterize(roads, temp)
inv_road_rast <- classify(road_rast, cbind(NA, 1), others = 0.0001)
inv_road_rast; plet(inv_road_rast)
writeRaster(inv_road_rast, './00_Data/Environmental_data/Outputs/Roads/aoi_roads.tif')
# Will not resample roads to 30, 30 resolution as this will result in most cells being classified as roads. We can address this later with gdalwarp.




# 2.10 BTRW Habitat suitability -----
BTRW_hsm <- vect('./00_Data/BTRW_data/Qspatial/gis/shapefiles/HSM_QLD.shp')
unique(BTRW_hsm$SCI_NAME)
BTRW_hsm <- BTRW_hsm[BTRW_hsm$SCI_NAME == "Petrogal penicillata"]
writeVector(BTRW_hsm, './00_Data/BTRW_data/DES HSM_Petrogale penicillata/DES HSM_Petrogale penicillata.shp')

BTRW_hsm <- project(BTRW_hsm, 'EPSG:3577')

BTRW_hsm 
head(BTRW_hsm)
table(BTRW_hsm$HSM_SUIT, BTRW_hsm$ENV_HB_SUI)
table(BTRW_hsm$HSM_SUIT, BTRW_hsm$ECO_HB_SUI)
length(unique(BTRW_hsm$HSM_SUIT))
BTRW_hsm$HSM_VAL <- factor(BTRW_hsm$HSM_SUIT, levels = c("Very low", "Low", "Medium", "High", "Very high")) # Reorganise categorical levels
BTRW_hsm$HSM_VAL <- as.numeric(BTRW_hsm$HSM_VAL) # Convert to numeric
head(BTRW_hsm); table(BTRW_hsm$HSM_SUIT, BTRW_hsm$HSM_VAL)
BTRW_hsm_val <- BTRW_hsm[, 20]
plot(BTRW_hsm_val)

# Convert to raster
rtemp <- rast(xmin = 1702105, xmax = 2137037, ymin = -3371610, ymax = -2903819, res = 30, crs = 'EPSG:3577') # Use the extent of the area of interest to create the template raster
BTRW_hsm_r <- rasterize(BTRW_hsm, rtemp, field = 'HSM_VAL')
BTRW_hsm_r
# Check
BTRWhsm_r <- rasterize(BTRW_hsm, rtemp, field = 'HSM_SUIT') # Convert the categorical value to raster
par(mfrow = c(1,2)); plot(BTRWhsm_r); plot(BTRW_hsm_r)

NAflag(BTRW_hsm_r)
writeRaster(BTRW_hsm_r, './00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM.asc', NAflag = -9999, overwrite = T)

# Natalya Maitz SDM and HSM
HSM <- rast('./00_Data/BTRW_data/Natalya_Maitz_SDM/BTRW_Maxent_NM/contemporary_binary.tif')
plet(HSM); HSM
HSM <- crop(project(HSM, 'EPSG:3577'), aoi)
plet(HSM)
HSM_res <- resample(HSM, rtemp, method = 'mode')
plet(HSM_res); HSM_res
writeRaster(HSM_res, './00_Data/Environmental_data/Outputs/BTRW_HSM/Natalya_Maitz_HSM.tif')
SDM <- rast('./00_Data/BTRW_data/Natalya_Maitz_SDM/BTRW_Maxent_NM/contemporary_full.tif')
plet(SDM) # Same spatial coverage, different output.
# Natalyas HSM while providing good continuous numerical values for habitat suitability, is more limited than what we need to the current study. 






# 3. Check resolution, crs, and spatial extent of all environmental layers ----
NDVI <- rast('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi.tif')
NDVI
 
NVIS <- rast('./00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi.tif')
NVIS

elevation <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj.tif') 
elevation

aspect <- rast('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi.tif')
aspect

slope <- rast('./00_Data/Environmental_data/Outputs/DEM/slope_aoi.tif')
slope

ruggedness <- rast('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi.tif') 
ruggedness

landuse <- rast('./00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated.tif') 
landuse

maxtemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj.tif')
maxtemp

mintemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj.tif') 
mintemp

rainfall_90_10 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj.tif')
rainfall_90_10

rainfall_11_24 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj.tif')
rainfall_11_24

rainfall <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj.tif')
rainfall

buildings <- rast('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated.tif')
buildings

roads <- rast('./00_Data/Environmental_data/Outputs/Roads/aoi_roads.tif')
roads



BTRW_hsm <- rast('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM.asc')
BTRW_hsm


ext(NDVI); ext(elevation); ext(aspect); ext(slope); ext(ruggedness); ext(landuse); ext(maxtemp); ext(mintemp); ext(rainfall_90_10); ext(rainfall_11_24); ext(rainfall); ext(NVIS); ext(buildings); ext(roads); ext(rail); ext(BTRW_hsm)


# Some extent discrepancies to address, use gdalwarp to reinforce cropping
gdalwarp('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi.tif',
         './00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
NDVI <- rast('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi.tif',
         './00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
NVIS <- rast('./00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_cropped.tif')


gdalwarp('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj.tif',
         './00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820)) 
elevation <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi.tif',
         './00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
aspect <- rast('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/DEM/slope_aoi.tif',
         './00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
slope <- rast('./00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi.tif',
         './00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820)) 
ruggedness <- rast('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated.tif',
                './00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820)) 
landuse <- rast('./00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj.tif',
         './00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
maxtemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj.tif',
         './00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820)) 
mintemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj.tif',
         './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
rainfall_90_10 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj.tif',
         './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
rainfall_11_24 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj.tif',
         './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
rainfall <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated.tif',
         './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj.tif',
         tr = c(30, 30),
         r = 'bilinear')
gdalwarp('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj.tif',
         './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped.tif',
         te = c(1702105, -3371609, 2137044, -2903820))
buildings <- rast('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped.tif')



gdalwarp('./00_Data/Environmental_data/Outputs/Roads/aoi_roads.tif',
         './00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj.tif', 
         tr = c(30,30), 
         r = 'bilinear')
gdalwarp('./00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj.tif',
         './00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj_cropped.tif', 
         te = c(1702105, -3371609, 2137044, -2903820),
         tr = c(30,30))
roads <- rast('./00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj_cropped.tif')




gdalwarp('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM.asc',
         './00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_cropped.asc',
         te = c(1702105, -3371609, 2137044, -2903820))
BTRW_hsm <- rast('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_cropped.asc')



# Check
ext(NDVI); ext(elevation); ext(aspect); ext(slope); ext(ruggedness); ext(landuse); ext(maxtemp); ext(mintemp); ext(rainfall_90_10); ext(rainfall_11_24); ext(rainfall); ext(NVIS); ext(buildings); ext(roads); ext(rail); ext(BTRW_hsm)

res(NDVI); res(elevation); res(aspect); res(slope); res(ruggedness); res(landuse); res(maxtemp); res(mintemp); res(rainfall_90_10); res(rainfall_11_24); res(rainfall); res(NVIS); res(buildings); res(roads); res(rail); res(BTRW_hsm)

# Resolutions need to be 30x30.
# Resample all rasters to match the template
rtemp <- rast(xmin = 1702105, xmax = 2137044, ymin = -3371609, ymax = -2903820, res = 30, crs = 'EPSG:3577') # Use the extent of the area of interest to create the template raster

NDVI <- resample(NDVI, rtemp, method = "bilinear")
writeRaster(NDVI, './00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped.tif', overwrite = T)


elevation <- resample(elevation, rtemp, method = "bilinear")
writeRaster(elevation, './00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped.tif', overwrite = T)

aspect <- resample(aspect, rtemp, method = "bilinear")
writeRaster(aspect, './00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped.tif', overwrite = T)

slope <- resample(slope, rtemp, method = "bilinear")
writeRaster(slope, './00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped.tif', overwrite = T)

ruggedness <- resample(ruggedness, rtemp, method = "bilinear")
writeRaster(ruggedness, './00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped.tif', overwrite = T)

landuse <- resample(landuse, rtemp, method = "mode")
writeRaster(landuse, './00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped.tif', overwrite = T)

maxtemp <- resample(maxtemp, rtemp, method = "bilinear")
writeRaster(maxtemp, './00_Data/Environmental_data/Outputs/SILO_Max_temp/Average_max_temp_aoi_reproj_cropped.tif', overwrite = T)

mintemp <- resample(mintemp, rtemp, method = "bilinear")
writeRaster(mintemp, './00_Data/Environmental_data/Outputs/SILO_Min_temp/Average_min_temp_aoi_reproj_cropped.tif', overwrite = T)

rainfall_90_10 <- resample(rainfall_90_10, rtemp, method = "bilinear")
writeRaster(rainfall_90_10, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped.tif', overwrite = T)

rainfall_11_24 <- resample(rainfall_11_24, rtemp, method ="bilinear")
writeRaster(rainfall_11_24, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped.tif', overwrite = T)

rainfall <- resample(rainfall, rtemp, method = "bilinear")
writeRaster(rainfall, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped.tif', overwrite = T)

buildings <- resample(buildings, rtemp, method = 'mode')
writeRaster(buildings, './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped.tif', overwrite = T)



BTRW_hsm <- resample(BTRW_hsm, rtemp, method = 'mode')
plot(BTRW_hsm); plot(BTRW_hsm_r)
writeRaster(BTRW_hsm, './00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_cropped.asc', NAflag = -9999, overwrite = T)


rtemp <- rast(xmin = 1702105, xmax = 2137044, ymin = -3371609, ymax = -2903820, res = 30, crs = 'EPSG:3577')
NVIS <- resample(NVIS, rtemp, method = 'mode')
writeRaster(NVIS, './00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_reproj_cropped.tif', overwrite = T)

# The DEM derived and BVG data include areas that are not land, lets mask the coastline
Aus <- vect('./00_Data/Australia_shapefile/STE11aAust.shp') %>% 
  project("EPSG:3577") %>% 
  crop(NDVI)

elevation <- mask(elevation, Aus) %>% 
  writeRaster('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped.tif', overwrite = T)

slope <- mask(slope, Aus) %>% 
  writeRaster('./00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped.tif', overwrite = T)

aspect <- mask(aspect, Aus) %>% 
  writeRaster('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped.tif', overwrite = T)

ruggedness <- mask(ruggedness, Aus) %>% 
  writeRaster('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped.tif', overwrite = T)



#################### TEST
# As the data is quite large, we want to create a test area within the core cluster of BTRW presence points

### REDUCE to smaller study extent capturing the main cluster of the presence points
BTRW_coords <- BTRW_pres[, 4:5]
colnames(BTRW_coords) <- c("y", "x")
head(BTRW_coords)
BTRW_cds <- vect(BTRW_coords, geom = c("x", "y"), crs = "EPSG:4326") %>% 
  project('EPSG:3577')

e <- ext(1920000, 2060000, -3250000, -3075000)
plot(BTRW_hsm)
plot(e, add = T)
plot(BTRW_cds, add = T)
btrw <- crop(BTRW_cds, e)

rtemp <- vect(ext(e), crs = "EPSG:3577")
writeVector(rtemp, './00_Data/BTRW_data/BTRW_pres_ext_test.gpkg')
plet(c(rtemp, BTRW_cds))


rtemp <- rast(xmin = 1920000, xmax = 2060000, ymin = -3250000, ymax = -3075000, res = 100, crs = 'EPSG:3577')

NDVI <- rast('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped.tif') %>% 
  crop(e) %>% 
  project(rtemp, method = 'bilinear')
names(NDVI) <- 'NDVI'
NDVI
writeRaster(NDVI, './00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped_test.asc', NAflag = -9999, overwrite = T)
plot(NDVI)

NVIS <- rast('./00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_reproj_cropped.tif') %>% 
  crop(e) %>% 
  project(rtemp, method = 'mode')
names(NVIS) <- 'NVIS_code'
NVIS
plot(NVIS)
writeRaster(NVIS, './00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)

elevation <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(elevation) <- "elevation"
elevation # Elevation has negative values and 0s
plot(elevation)
writeRaster(elevation, './00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)

aspect <- rast('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(aspect) <- 'aspect'
aspect
writeRaster(aspect, './00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped_test.asc', NAflag = -9999, overwrite = T)

slope <- rast('./00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(slope) <- 'slope'
slope # Slope has 0 values, flat land would have value 0 but circuitscape won't run with this
plot(slope)
writeRaster(slope, './00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped_test.asc', NAflag = -9999, overwrite = T)

ruggedness <- rast('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(ruggedness) <- 'TRI'
ruggedness # 0 values
plot(ruggedness)
writeRaster(ruggedness, './00_Data/Environmental_data/Outputs/DEM/rugedness_aoi_cropped_test.asc', NAflag = -9999, overwrite = T)

landuse <- rast('./00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(landuse) <- 'landuse'
landuse
writeRaster(landuse, './00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped_test.asc', NAflag = -9999, overwrite = T)

maxtemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(maxtemp) <- 'Max_temp'
maxtemp
writeRaster(maxtemp, './00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)


mintemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(mintemp) <- 'Min_temp'
mintemp
writeRaster(mintemp, './00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)


rainfall <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(rainfall) <- 'Rain'
rainfall
writeRaster(rainfall, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)


rainfall_90_10 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(rainfall_90_10) <- 'Rain_1990_2010'
rainfall_90_10
writeRaster(rainfall_90_10, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)


rainfall_11_24 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped.tif') %>% 
  crop(e)%>% 
  project(rtemp, method = 'bilinear')
names(rainfall_11_24) <- 'Rain_2011_2024'
rainfall_11_24
writeRaster(rainfall_11_24, './00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)

buildings <- rast('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped.tif') %>% 
  crop(e) %>% 
  project(rtemp, method = 'mode')
names(buildings) <- 'Buildings'
plot(buildings)
writeRaster(buildings, './00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)

roads <- rast('./00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj_cropped.tif') %>% 
  crop(e) %>% 
  project(rtemp, method = 'mode')
names(roads) <- 'Roads'
roads; plet(roads)
writeRaster(roads, './00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj_cropped_test.asc', NAflag = -9999, overwrite = T)


BTRW_hsm <- rast('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_cropped.asc') %>% 
  crop(e) %>% 
  project(rtemp, method = 'mode')
BTRW_hsm; plot(BTRW_hsm)
writeRaster(BTRW_hsm, './00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_test.asc', NAflag = -9999)



####### DEM derived variables contain 0s which Circuitscape interprets this as infinite resistance, so we will replace 0s with a value of 0.0001. Circuitscape also requires that values are positive, so we need to address these
elevation <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped_test.asc') 
elevation
plot(elevation)
# Take a look where the negative values lie
negative_map <- ifel(elevation < 0, 1, 0)
plot(negative_map, main = "Negative Elevation Values",col = c("white", "red"), legend = FALSE)
plet(negative_map)
# There are few negative values, mainly at the edges of landmasses so we will treat these in the same way that we will treat the other elevation derived variables which only have 0s
elevation <- ifel(elevation <= 0, 0.0001, elevation)
elevation
writeRaster(elevation, './00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped_test.asc', overwrite = T, NAflag = -9999)

slope <- rast('./00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped_test.asc')
slope
slope <- ifel(slope <0.0001, 0.0001, slope)
slope
writeRaster(slope, './00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped_test.asc', overwrite = T, NAflag = -9999)

ruggedness <- rast('./00_Data/Environmental_data/Outputs/DEM/rugedness_aoi_cropped_test.asc')
ruggedness
plot(ruggedness)
ruggedness <- ifel(ruggedness$TRI <0.0001, 0.0001, ruggedness$TRI)
ruggedness
writeRaster(ruggedness, './00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped_test.asc', overwrite = T, NAflag = -9999)
