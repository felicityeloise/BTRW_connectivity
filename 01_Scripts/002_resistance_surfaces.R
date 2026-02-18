# Written by Felicity Charles
# 02/10/2025
# Caveat emptor


## This script produces resistance surfaces from environmental data needed for Brush-tailed rock wallaby connectivity analyses

# R version 4.5.1

# Install CIRCUITSCAPE 
# In Terminal run 
#brew install julia
#install.packages("JuliaCall")
library(JuliaCall)
# For HPC to install Julia and Circuitscape run the following
#module load julia/1.10.4
# julia -e 'using Pkg; Pkg.add("Circuitscape")'

# Load required packages ----
library(terra) # terra_1.8-70
library(dplyr) # dplyr_1.1.4 
library(car) # 3.1-3
library(usdm) # 2.1-7
library(gdalUtilities) # 1.2.5
library(ggplot2) # 4.0.0
library(tidyterra) # 0.7.2
library(ggspatial) #1.1.10
library(cowplot) # 1.2.0
library(igraph) # 2.2.1

# 1. Read in the data ----
BTRW_pres <- read.csv('./00_Data/BTRW_data/1_BTRW_Records_All_Combined_Hi_Prec.csv', header = T)
head(BTRW_pres); dim(BTRW_pres)
BTRW_pres$Date_start
unique(substr(BTRW_pres$Date_start, 1, 4)) # A few entries have "1770 at the start
BTRW_pres[substr(BTRW_pres$Date_start, 1, 4) == "1770", ]
BTRW_pres[19,6] <- '17/05/2023'
BTRW_pres[212,6] <- '17/05/2023'
length(BTRW_pres$Date_start)
sum(BTRW_pres$Date_start == "" | is.na(BTRW_pres$Date_start)) # There are no empty Date_start entries
unique(BTRW_pres$Date_start)

BTRW_pres$year <- ifelse(nchar(BTRW_pres$Date_start) == 4, as.numeric(BTRW_pres$Date_start), as.numeric(substr(BTRW_pres$Date_start, (nchar(BTRW_pres$Date_start) - 3), nchar(BTRW_pres$Date_start))))

dim(BTRW_pres); head(BTRW_pres)

Aus <- vect('./00_Data/Australia_shapefile/STE11aAust.shp') %>% 
  project("EPSG:3577")

QN <- Aus[Aus$STATE_NAME == "Queensland" | Aus$STATE_NAME == "New South Wales"]


BTRW_hsm_r <- rast('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_test.asc')

places <- vect('./00_Data/Environmental_data/Place_names/Place_names_gazetteer.shp') %>% 
  project('EPSG:3577') %>% 
  crop(BTRW_hsm_r)
places <- places[places$place_name =="Brisbane"| places$place_name =="Toowoomba"| places$place_name =="Esk" | places$place_name == "Boonah"]
places <- places[!duplicated(places$place_name),]



# Load environmental data for the main area where we have BTRW occurrences
NDVI <- rast('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped_test.asc')
names(NDVI) <- 'NDVI'
NVIS <- rast('./00_Data/Environmental_data/Outputs/BVG/NVIS_Major_veg_aoi_reproj_cropped_test.asc')
names(NVIS) <- 'NVIS_code'
elevation <- rast('./00_Data/Environmental_data/Outputs/DEM/QLD_DEM_reproj_cropped_test.asc')
names(elevation) <- "elevation"
aspect <- rast('./00_Data/Environmental_data/Outputs/DEM/aspect_aoi_cropped_test.asc')
names(aspect) <- 'aspect'
slope <- rast('./00_Data/Environmental_data/Outputs/DEM/slope_aoi_cropped_test.asc')
names(slope) <- 'slope'
ruggedness <- rast('./00_Data/Environmental_data/Outputs/DEM/ruggedness_aoi_cropped_test.asc')
names(ruggedness) <- 'TRI'
landuse <- rast('./00_Data/Environmental_data/Outputs/Landuse/aoi_landuse_disaggregated_cropped_test.asc')
names(landuse) <- 'landuse'
maxtemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Max_Temp/Average_max_temp_aoi_reproj_cropped_test.asc')
names(maxtemp) <- 'Max_temp'
mintemp <- rast('./00_Data/Environmental_data/Outputs/SILO_Min_Temp/Average_min_temp_aoi_reproj_cropped_test.asc')
names(mintemp) <- 'Min_temp'
rainfall <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_reproj_cropped_test.asc')
names(rainfall) <- 'Rain'
rainfall_90_10 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_1990_2010_reproj_cropped_test.asc')
names(rainfall_90_10) <- 'Rain_1990_2010'
rainfall_11_24 <- rast('./00_Data/Environmental_data/Outputs/SILO_Rainfall/Average_rainfall_aoi_2011_2024_reproj_cropped_test.asc')
names(rainfall_11_24) <- 'Rain_2011_2024'
buildings <- rast('./00_Data/Environmental_data/Outputs/Buildings/aoi_building_areas_aggregated_reproj_cropped_test.asc')
names(buildings) <-  'Buildings'
roads <- rast('./00_Data/Environmental_data/Outputs/Roads/aoi_roads_reproj_cropped_test.asc')
names(roads) <- "Roads"
rail <- rast('./00_Data/Environmental_data/Outputs/Railways/aoi_railway_aggregated_reproj_cropped_test.asc')
names(rail) <- "Railways"

# Create presence point only information
# Need to convert data from lon/lat to meter based
e <- ext(1920000, 2060000, -3250000, -3075000)

BTRW_coords <- BTRW_pres[, 4:5]
colnames(BTRW_coords) <- c("y", "x")
head(BTRW_coords)
BTRW_cds <- vect(BTRW_coords, geom = c("x", "y"), crs = "EPSG:4326") %>% 
  project('EPSG:3577') %>% 
  crop(e)
BTRW_cds; plet(BTRW_cds) # Reduced set of points to 506 records, only 547 total presence records provided

BTRW_cds$x <- crds(BTRW_cds)[,1]
BTRW_cds$y <- crds(BTRW_cds)[,2]



# Round coordinates to fewer decimals
BTRW_coords_r <- data.frame(ID = 1:nrow(BTRW_cds), x = round(BTRW_cds$x, 0), y = round(BTRW_cds$y, 0))
head(BTRW_coords_r)

coords_text <- paste(BTRW_coords_r$ID, BTRW_coords_r$x, BTRW_coords_r$y, sep = " ")
writeLines(coords_text, './00_Data/BTRW_data/BTRW_coords_test.txt')


# 2. Check for collinearity ----
# 2.1 Create a raster stack of all environmental data and scale the variables to reduce numeric overflow issues during optimization. We will exclude the buildings, roads, and railways data as these do not have more than one value to be able to check VIF.
environmental_data <- c(NDVI, elevation, aspect, slope, ruggedness, landuse, maxtemp, mintemp, rainfall, NVIS)
# Check the values seem reasonable
environmental_data
plot(environmental_data)


# NOTE: Only rainfall data is limited to 1990 to 2010
env_drought_yrs <- c(NDVI, elevation, aspect, slope, ruggedness, landuse, maxtemp, mintemp, rainfall_90_10, NVIS) 

# NOTE: Only rainfall data is limited to 2011 to 2024
env_flood_yrs <- c(NDVI, elevation, aspect, slope, ruggedness, landuse, maxtemp, mintemp, rainfall_11_24, NVIS) 

# 2.2 Check for multicollinearity using Variance Inflation Factors (VIF)
# Extract environmental data for presence points
presence_env <- extract(environmental_data, BTRW_cds)
presence_df <- na.omit(as.data.frame(presence_env))

# Calculate VIF
presence_df$presence <- 1
vif_model <- glm(presence ~ ., data = presence_df, family = binomial)
car::vif(vif_model)
# We can see that we have some very highly correlated variables such as slope and TRI


# Conduct step wise elimination to exclude  highly correlated variables for VIF
VIF_step <- vifstep(presence_df[, -ncol(presence_df)], th = 5) # Choose threshold, usually 5 or 10
VIF_step
# Suggesting to remove ruggedness but we would prefer this over slope. Also suggests moving Maximum temperature which we have no issues with removing as minimum temperature has been found to drive occurrences of BTRW

vif_model2 <- glm(presence ~ NDVI + elevation + aspect + TRI + landuse + Min_temp + Rain + NVIS_code, data = presence_df, family = binomial)
car::vif(vif_model2)
# Most correlations are low, only high correlation remaining is for elevation but even that is reasonable



# 2.2.1 Check if VIF is different for the more limited rainfall data
pres_env_d <- as.data.frame(extract(env_drought_yrs, BTRW_cds))
pres_env_d$presence <- 1

vif_d_m1 <-  glm(presence ~ NDVI + elevation + aspect + slope + TRI + landuse + Max_temp + Min_temp + Rain_1990_2010 + NVIS_code, data = pres_env_d, family = binomial)
car::vif(vif_d_m1)

vif_d_m2 <-  glm(presence ~ NDVI + elevation + aspect + TRI + landuse + Min_temp + Rain_1990_2010 + NVIS_code, data = pres_env_d, family = binomial)
car::vif(vif_d_m2)
# VIF is similar to when we have the full range of environmental rainfall data


# Remove slope and Max temp from environmental data
head(environmental_data)
environmental_data <- c(NDVI, elevation, aspect, ruggedness, landuse, rainfall, mintemp, NVIS, buildings, roads, rail) 

env_drought_yrs <- c(NDVI, elevation, aspect, ruggedness, landuse, rainfall_90_10, mintemp, NVIS, buildings, roads, rail)
env_flood_yrs <- c(NDVI, elevation, aspect, ruggedness, landuse, rainfall_11_24, mintemp, NVIS, buildings, roads, rail)




# 3. Create .ini configuration file for Circuitscape to obtain resistance values for environmental variables ----
# Update habitat file, output file, and log file for each environmental layer. This was run on the HPC platform with file paths navigating to file destinations on the HPC rather than local computer
habitat_file <- './00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped.asc'
point_file <- './00_Data/BTRW_data/BTRW_coords_test.txt'
output_file <- './03_Results/Resistance_surfaces/NDVI/NDVI_output'
log_file <- './03_Results/Resistance_surfaces/NDVI/NDVI_log'

# Create .ini file content - Refer to https://github.com/Circuitscape/Circuitscape.jl.git test folder to find example .ini files
ini_content <- paste0(
  '[Options for advanced mode]\n',
  'ground_file_is_resistances = True\n',
  'remove_src_or_gnd = keepall\n',
  'ground_file = (Browse for a ground point file)\n',
  'use_unit_currents = False\n',
  'source_file = (Browse for a current source file)\n',
  'use_direct_grounds = False\n\n',
  '[Mask file]\n',
  'mask_file = None\n',
  'use_mask = False\n\n',
  '[Calculation options]\n',
  'solver = cg+amg\n',
  'low_memory_mode = False\n\n',
  'parallelize = True\n',
  'solver = cg+amg\n',
  'print_timings = 1\n',
  'preemptive_memory_release = True\n',
  'print_rusages = 1\n',
  'max_parallel = 32\n\n',
  '[Options for one-to-all and all-to-one modes]\n',
  'use_variable_source_strengths = False\n',
  'variable_source_file = None\n\n',
  '[Output options]\n',
  'set_null_currents_to_nodata = False\n',
  'set_focal_node_currents_to_zero = False\n',
  'set_null_voltages_to_nodata = False\n',
  'compress_grids = True\n',
  'write_cur_maps = True\n',
  'write_volt_maps = False\n',
  'output_file = ', output_file, '\n',
  'write_cum_cur_map_only = False\n',
  'log_transform_maps = False\n',
  'write_max_cur_maps = False\n\n',
  '[Options for reclassification of habitat data]\n',
  'reclass_file = (Browse for file with reclassification data)\n',
  'use_reclass_table = False\n\n',
  '[Options for pairwise and one-to-all and all-to-one modes]\n',
  'included_pairs_file = (Browse for a file with pairs to include or exclude)\n',
  'use_included_pairs = False\n\n',
  'point_file = ', point_file, '\n\n',
  '[Connection scheme for raster habitat data]\n',
  'connect_four_neighbors_only = False\n',
  'connect_using_avg_resistances = True\n\n',
  '[Habitat raster or graph]\n',
  'habitat_map_is_resistances = True\n',
  'habitat_file = ', habitat_file, '\n\n',
  '[Circuitscape mode]\n',
  'data_type = raster\n',
  'scenario = pairwise\n\n',
  '[Logging Options]\n',
  'log_level = INFO\n',
  'log_format = default\n',
  'log_file = ', log_file, '\n',
  'screenprint_log = False\n\n',
  '[Circuitscape mode]\n',
  'data_type = raster\n',
  'scenario = pairwise'
)

# Save .ini file
ini_path <- './03_Results/Resistance_surfaces/circuitscape_config.ini'
writeLines(ini_content, ini_path)


# 4. Run Circuitscape on HPC ----
# All files for circuitscape were uploaded to the HPC with file path information updated
# Set up high performance expert desktop computing session with 32 cores, 1 task and 252 GB RAM
# Following commands run in terminal
# module load julia/1.10.4
# julia -e 'using Pkg; Pkg.add("Circuitscape")' # Only run once
# cd Documents/Resistance_surfaces/
# julia -e 'using Circuitscape; compute("NDVI_config.ini")'
# compute file updated for each environmental layer


# 5. Load cumulative current maps ----
# Cumulative current maps show where animal movement flows across the landscape, higher current values more important for connectivity.
HSM_cur <- rast('./03_Results/Resistance_surfaces/Habitat_suitability/Habitat_suitability_output_cum_curmap.asc') %>% 
  mask(QN)
plot(HSM_cur)
plot(BTRW_cds, add = T)
plot(HSM_cur <100)
plot(BTRW_cds, add = T)

# Re-assign values over 100 as 100 to show 100% connectivity
HSM_cur <- ifel(HSM_cur >100, 100, HSM_cur)
HSM_cur; plet(HSM_cur)


NDVI_cur <- rast('./03_Results/Resistance_surfaces/NDVI/NDVI_output_cum_curmap.asc') %>% 
  mask(QN)
plot(NDVI_cur)
plot(BTRW_cds, add = T)
plot(NDVI_cur <100) # Good visualisation of smaller patches nearby to larger patches which could inform population prioritisation.
plot(BTRW_cds, add = T)
# Where we have really high cumulative current values and more connectivity we have larger clusters of BTRW occurrences so these areas are really well connected.
summary(NDVI_cur)

# Re-assign values over 100 as 100 to show 100% connectivity
NDVI_cur <- ifel(NDVI_cur >100, 100, NDVI_cur)
NDVI_cur; plet(NDVI_cur)


mintemp_cur <- rast('./03_Results/Resistance_surfaces/Min_temp/Min_temp_output_cum_curmap.asc') %>% 
  mask(QN)
plot(mintemp_cur); summary(mintemp_cur$Min_temp_output_cum_curmap)
mintemp_cur <- ifel(mintemp_cur >100, 100, mintemp_cur)
mintemp_cur; plet(mintemp_cur)


aspect_cur <- rast('./03_Results/Resistance_surfaces/Aspect/Aspect_output_cum_curmap.asc') %>% 
  mask(QN)
summary(aspect_cur); plot(aspect_cur)
aspect_cur <- ifel(aspect_cur >100, 100, aspect_cur)
aspect_cur; plet(aspect_cur)


landuse_cur <- rast('./03_Results/Resistance_surfaces/Land/Land_output_cum_curmap.asc') %>% 
  mask(QN)
plot(landuse_cur); summary(landuse_cur)
landuse_cur <- ifel(landuse_cur >100, 100, landuse_cur)
landuse_cur; plet(landuse_cur)

rainfall_cur <- rast('./03_Results/Resistance_surfaces/Rainfall/Rainfall_output_cum_curmap.asc') %>% 
  mask(QN)
plot(rainfall_cur); summary(rainfall_cur)
rainfall_cur <- ifel(rainfall_cur >100, 100, rainfall_cur)
rainfall_cur; plet(rainfall_cur)

building_cur <- rast('./03_Results/Resistance_surfaces/Buildings/Buildings_output_cum_curmap.asc') %>% 
  mask(QN)
plot(building_cur); summary(building_cur)
building_cur <- ifel(building_cur >100, 100, building_cur)
plot(building_cur)

NVIS_cur <- rast('./03_Results/Resistance_surfaces/NVIS/NVIS_output_cum_curmap.asc') %>% 
  mask(QN)
plot(NVIS_cur); summary(NVIS_cur)
NVIS_cur <- ifel(NVIS_cur >100, 100, NVIS_cur)
plot(NVIS_cur)

Rain_drought_cur <- rast('./03_Results/Resistance_surfaces/Rainfall_90_10/Rainfall_90_10_output_cum_curmap.asc') %>% 
  mask(QN)
Rain_drought_cur <- ifel(Rain_drought_cur >100, 100, Rain_drought_cur)
plot(Rain_drought_cur)

Rain_flood_cur <- rast('./03_Results/Resistance_surfaces/Rainfall_11_24/Rainfall_11_24_output_cum_curmap.asc') %>% 
  mask(QN)
Rain_flood_cur <- ifel(Rain_flood_cur >100, 100, Rain_flood_cur)
plot(Rain_flood_cur)

Rugged_cur <- rast('./03_Results/Resistance_surfaces/Ruggedness/Ruggedness_output_cum_curmap.asc') %>% 
  mask(QN)
Rugged_cur <- ifel(Rugged_cur >100, 100, Rugged_cur)
plot(Rugged_cur)


roads_cur <- rast('./03_Results/Resistance_surfaces/Roads/Road_output_cum_curmap.asc') %>% 
  mask(QN)
roads_cur <- ifel(roads_cur >100, 100, roads_cur)
plot(roads_cur)

elevation_cur <- rast('./03_Results/Resistance_surfaces/Elevation/Elevation_output_cum_curmap.asc') %>% 
  mask(QN)
elevation_cur <- ifel(elevation_cur >100, 100, elevation_cur)
plot(elevation_cur)


# 6. Rank connectivity maps ----
# 6.1 Create 10000 random points
set.seed(375)
rand_pts <- spatSample(HSM_cur, size = 10000, method = 'random', na.rm = T, as.points = T)
plot(rand_pts); dim(rand_pts)

# 6.2 Compare correlations to HSM connectivity result
HSM_pts <- extract(HSM_cur, rand_pts)

NDVI_pts <- extract(NDVI_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, NDVI_pts$NDVI_output_cum_curmap) # 0.8837224

mintm_pts <- extract(mintemp_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, mintm_pts$Min_temp_output_cum_curmap) # 0.8443371


rain_pts <- extract(rainfall_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, rain_pts$Rainfall_output_cum_curmap) # 0.8619866

aspect_pts <- extract(aspect_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, aspect_pts$Aspect_output_cum_curmap) # 0.76028

land_pts <- extract(landuse_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, land_pts$Land_output_cum_curmap) # 0.6432878

nvis_pts <- extract(NVIS_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, nvis_pts$NVIS_output_cum_curmap) # 0.4691133

rain_drought_pts <- extract(Rain_drought_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, rain_drought_pts$Rainfall_90_10_output_cum_curmap) # 0.8630988

rain_flood_pts <- extract(Rain_flood_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, rain_flood_pts$Rainfall_11_24_output_cum_curmap) # 0.85599277

rugged_pts <- extract(Rugged_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, rugged_pts$Ruggedness_output_cum_curmap) # 0.7952246

build_pts <- extract(building_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, build_pts$Buildings_output_cum_curmap) # 0.5463696

roads_pts <- extract(roads_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, roads_pts$Road_output_cum_curmap) # 0.455055

elevation_pts <- extract(elevation_cur, rand_pts)
cor.test(HSM_pts$Habitat_suitability_output_cum_curmap, elevation_pts$Elevation_output_cum_curmap) # 0.7003844



# 7. Produce connectivity maps -----

HSM_m <- ggplot()+
  geom_spatraster(data = HSM_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(a) Habitat suitability", subtitle = "") +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off') +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



NDVI_m <-ggplot()+
  geom_spatraster(data = NDVI_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(b) NDVI", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.883"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off') +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")


rain_drought_m <- ggplot()+
  geom_spatraster(data = Rain_drought_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(c) Rainfall 1990-2010", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.863"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



rainfall_m <- ggplot()+
  geom_spatraster(data = rainfall_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(d) Rainfall", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.861"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



rain_flood_m <- ggplot()+
  geom_spatraster(data = Rain_flood_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(e) Rainfall 2011-2024", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.855"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



mintemp_m <- ggplot()+
  geom_spatraster(data = mintemp_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(f) Minimum temperature", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.844"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



rugged_m <- ggplot()+
  geom_spatraster(data = Rugged_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(g) Ruggedness", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.795"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")




aspect_m <- ggplot()+
  geom_spatraster(data = aspect_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(h) Aspect", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.760"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



elevation_m <- ggplot()+
  geom_spatraster(data = aspect_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(i) Elevation", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.700"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



landuse_m <- ggplot()+
  geom_spatraster(data = landuse_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(j) Landuse", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.643"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



build_m <- ggplot()+
  geom_spatraster(data = building_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(k) Buildings", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.546"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")


NVIS_m <- ggplot()+
  geom_spatraster(data = NVIS_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(l) Broad vegetation group", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.469"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")



road_m <- ggplot()+
  geom_spatraster(data = roads_cur) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = "Connectivity", title = "(m) Roads", subtitle = expression(paste("Pearson's ", italic("r"), " = 0.455"))) +
  annotation_scale(location = "bl", pad_y = unit(0.07, 'cm'), pad_x = unit(3, 'cm'), text_cex = 1.2)+
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.45, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(5.8, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 25),
        legend.text = element_text(size = 20),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 2.5, 0.1), "cm"))+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  coord_sf(clip = 'off')+
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  labs(x = "", y = "")




cur_legend <- get_legend(rain_flood_m)
cur_maps <- plot_grid(HSM_m + theme(legend.position = 'none'), NDVI_m + theme(legend.position = "none"), rain_drought_m + theme(legend.position = 'none'), rainfall_m + theme(legend.position = 'none'), rain_flood_m + theme(legend.position = 'none'), cur_legend,
                        mintemp_m + theme(legend.position = 'none'), rugged_m + theme(legend.position = 'none'), aspect_m + theme(legend.position = 'none'), elevation_m + theme(legend.position = 'none'), landuse_m + theme(legend.position = 'none'), NULL,
                        build_m + theme(legend.position = 'none'), NVIS_m + theme(legend.position = 'none'), road_m + theme(legend.position = 'none'), NULL, NULL, NULL,
                        nrow = 3, ncol = 6, rel_widths = c(1,1,1,1,1,0.5))

cur_maps
ggsave("./03_Results/Plots/Circuitscape_Connectivity_maps/Connectivity_maps.png",  width = 55, height = 33.9, units = "cm", dpi = 300, limitsize = FALSE)


# 7.1 Add presence points
HSM_m_pres <- HSM_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')

NDVI_m_pres <- NDVI_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')

mintemp_m_pres <- mintemp_m +
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')

rainfall_m_pres <- rainfall_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


aspect_m_pres <- aspect_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


elevation_m_pres <- elevation_m +
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


landuse_m_pres <- landuse_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


rain_drought_m_pres <- rain_drought_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


rain_flood_m_pres <- rain_flood_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


rugged_m_pres <- rugged_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


road_m_pres <- road_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


build_m_pres <- build_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


NVIS_m_pres <- NVIS_m + 
  geom_spatvector(data = BTRW_cds, aes(alpha = 0.5), size = 1)+
  labs(alpha = "") +
  scale_alpha_continuous(labels = "BTRW presences")+
  guides(alpha = guide_legend(override.aes = list(size=3))) +
  coord_sf(clip = 'off')


cur_pres_legend <- get_legend(rain_flood_m_pres)

cur_pres_maps <- plot_grid(HSM_m_pres + theme(legend.position = 'none'), NDVI_m_pres + theme(legend.position = "none"), rain_drought_m_pres + theme(legend.position = 'none'), rainfall_m_pres + theme(legend.position = 'none'), rain_flood_m_pres + theme(legend.position = 'none'), cur_pres_legend,
                      mintemp_m_pres + theme(legend.position = 'none'), rugged_m_pres + theme(legend.position = 'none'), aspect_m_pres + theme(legend.position = 'none'), elevation_m_pres + theme(legend.position = 'none'), landuse_m_pres + theme(legend.position = 'none'), NULL,
                      build_m_pres + theme(legend.position = 'none'), NVIS_m_pres + theme(legend.position = 'none'), road_m_pres + theme(legend.position = 'none'), NULL, NULL, NULL,
                      nrow = 3, ncol = 6, rel_widths = c(1,1,1,1,1,0.5))

cur_pres_maps
ggsave("./03_Results/Plots/Circuitscape_Connectivity_maps/Connectivity_maps_wpres.png",  width = 55, height = 33.9, units = "cm", dpi = 300, limitsize = FALSE)

# Next steps
# Incorporate SDM and spatial planning tool - Peter Baxter - spatial conservation planning CBCS
# Need some infrastructure barrier data - so road data to investigate resistance to movement!
# Maps illustrating habitat connectivity
    # Finalised for current connectivity results. 

# Prioritise populations for management based on degree of isolation
  # How do we want to define a population? Grouping points which are within a minimum distance of each other like 3km which is where populations are genetically distinct. Then we calculate the distance between the populations, create a table ranking them by their isolation distance

# identify areas for restoration with corridor creation or stepping stone habitat
  # Need some road data and NDVI or landuse to determine areas which would not support this. We could use the habitat suitability map to determine whether these areas are currently considered to be appropriate for BTRW to assist in determining restoration needs

