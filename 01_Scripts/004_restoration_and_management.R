# Written by Felicity Charles
# 06/01/2026
# Caveat emptor

# This script investigates opportunities for restoration and managment. Investigating the need for vegetation restoration, stepping stone habitat and or corridor creation, and management of fire regimes and pest species.


# R version 4.5.1


# 1. Load required packages ----
#remotes::install_version("terra", version = "1.8-70", type = "source")
library(terra) # terra_1.8-70
library(dplyr) # dplyr_1.1.4 
library(sf) # 1.0-21
library(ggplot2) # 4.0.0
library(tidyterra) # 0.7.2
library(ggspatial) #1.1.10
library(cowplot) # 1.2.0
library(RColorBrewer)# 1.1-3
library(ggnewscale) # 0.5.2
library(leastcostpath) #2.0.13
library(sampbias) # 2.0.0
sessionInfo()
 
# 2. Load data ----
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


BTRW_pops <- vect('./03_Results/BTRW_pops.gpkg')

e <- ext(1920000, 2060010, -3250000, -3075010)

BTRW_coords <- BTRW_pres[, 4:5]
colnames(BTRW_coords) <- c("x", "y")
BTRW_coords$x <- as.numeric(BTRW_pres[,4])
BTRW_coords$y <- as.numeric(BTRW_pres[,5])
BTRW_cds <- vect(BTRW_coords, geom = c("x", "y"), crs = "EPSG:4326") %>% 
  project('EPSG:3577')

Aus <- vect('./00_Data/Australia_shapefile/STE_2021_AUST_GDA2020.shp') %>% 
  project("EPSG:3577") %>% 
  crop(e)

canal <- vect('./00_Data/Environmental_data/Hydrographic_features/Canal_areas.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

lake <- vect('./00_Data/Environmental_data/Hydrographic_features/Lakes.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

pond <- vect('./00_Data/Environmental_data/Hydrographic_features/Pondage.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

reservoir <- vect('./00_Data/Environmental_data/Hydrographic_features/Reservoirs.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

watercourse <- vect('./00_Data/Environmental_data/Hydrographic_features/Watercourse_areas.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)



# 3. Explore fire data ----
# Script authors recently undertook work to improve accuracy of fire frequency estimates in southeast Queensland from satellite data. Using the outputs of the modelling from https://doi.org/10.32942/X24331 but note that fire data was from 1987 to 2023. While 1987 is earlier than the start of records for BTRW, it will provide a better idea of whether inappropriate fire regimes even slightly prior to occurrence records is impacting BTRW.
rtemp <- rast(xmin = 1920000, xmax = 2060010, ymin = -3250000, ymax = -3075010, res = 30, crs = 'EPSG:3577')

Sent_freq <- rast('./00_Data/Environmental_data/Fire_data/GLM_pred.tif') %>% 
  crop(e) %>% 
  project(rtemp)
Sent_freq; plot(Sent_freq)


QPWS_fire_hist <- vect('./00_Data/Environmental_data/QPWS_fire_history/Fire_history___QPWS.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e) %>% 
  mask(canal, inverse = T) %>% 
  mask(lake, inverse = T) %>% 
  mask(pond, inverse = T) %>% 
  mask(reservoir, inverse = T) %>% 
  mask(watercourse, inverse = T)

QPWS_fire_hist <- subset(QPWS_fire_hist, QPWS_fire_hist$OUTYEAR >=1987)
unique(QPWS_fire_hist$OUTYEAR)
rtemp <- rast(xmin = 1920000, xmax = 2060010, ymin = -3250000, ymax = -3075010, res = 30, crs = 'EPSG:3577')
QPWS_freq <- rasterize(QPWS_fire_hist, rtemp, field = 'OUTYEAR', fun = 'count')
plot(QPWS_freq)
#writeRaster(QPWS_freq, './00_Data/Environmental_data/QPWS_fire_history/QPWS_fire_freq.tif')

Sent_freq <- mask(Sent_freq, QPWS_freq, inverse = T)
plot(Sent_freq)

fire_freq <- mosaic(QPWS_freq, Sent_freq)
plot(fire_freq); fire_freq


# Fire regime recommendations
fire_reg <- vect('./00_Data/Environmental_data/DP_SEQ_VEG_FIRE_RGM_100K_A/SEQ_VEG_FIRE_RGM_100K_A.shp') %>% 
  project('EPSG:3577') 
fire_reg <- fire_reg[!is.na(fire_reg$FREQUENCY),]

fire_reg <- subset(fire_reg, fire_reg$FVG != "Water" & fire_reg$FVG != "Sand")
fire_reg$FREQUENCY <- ifelse(fire_reg$FREQUENCY == "Do not intentionally burn", -1, fire_reg$FREQUENCY)
fire_reg$FREQUENCY <- ifelse(fire_reg$FREQUENCY == "As required", -2, fire_reg$FREQUENCY)
fire_reg$FREQUENCY <- ifelse(fire_reg$FREQUENCY == "No fire", 0 , fire_reg$FREQUENCY)
fire_reg$FREQUENCY <- ifelse(fire_reg$FREQUENCY == "n/a", NA_character_, fire_reg$FREQUENCY)
unique(fire_reg$FREQUENCY)


table(fire_reg$FVG, is.na(fire_reg$FREQUENCY))
# Calculate minimum and maximum fire frequency for 36 years
# Non-remnant vegetation has no fire frequency information so these will all be allocated 0s for minimum and maximum intervals of fire
# Extract minimum interval (number before dash, 0 for non-numeric)
fire_reg$min_int <- ifelse(grepl("\\d+-\\d+", fire_reg$FREQUENCY),
                        as.numeric(sub("(\\d+)-.*", "\\1", fire_reg$FREQUENCY)),
                        0)
# Extract maximum interval (number after dash, 0 for non-numeric)  
fire_reg$max_int <- ifelse(grepl("\\d+-\\d+", fire_reg$FREQUENCY),
                        as.numeric(sub(".*-(\\d+).*", "\\1", fire_reg$FREQUENCY)),
                        0)
fire_reg$max_ff <- ifelse(round(36/fire_reg$min_int) == "Inf", 0 , round(36/fire_reg$min_int))
fire_reg$min_ff <- ifelse(round(36/fire_reg$max_int) == "Inf", 0 , round(36/fire_reg$max_int))
head(fire_reg); dim(fire_reg)



# 4. Create buffers around populations ----
BTRW_pops; plet(BTRW_pops)
# Add geographical isolation information to populations 
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist <1000, 0, NA)
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist >1000 & BTRW_pops$nearest_pop_dist <2000, 1,  BTRW_pops$isolation_rank)
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist > 1999 & BTRW_pops$nearest_pop_dist <3000, 2, BTRW_pops$isolation_rank)
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist > 2999 & BTRW_pops$nearest_pop_dist <4000, 3, BTRW_pops$isolation_rank)
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist >3999 & BTRW_pops$nearest_pop_dist <5000, 4, BTRW_pops$isolation_rank)
BTRW_pops$isolation_rank <- ifelse(BTRW_pops$nearest_pop_dist >= 5000, 5, BTRW_pops$isolation_rank)

BTRW_pop_buf1 <- buffer(BTRW_pops, 1000)
BTRW_pop_buf1$dist <- "0-1km"
BTRW_pop_buf2 <- buffer(BTRW_pops, 2000) %>% 
  erase(buffer(BTRW_pops, 1000))
BTRW_pop_buf2$dist <- "1-2km"
BTRW_pop_buf3 <- buffer(BTRW_pops, 3000) %>% 
  erase(buffer(BTRW_pops, 2000))
BTRW_pop_buf3$dist <- "2-3km"


BTRW_pop_buf <- rbind(BTRW_pop_buf1, BTRW_pop_buf2, BTRW_pop_buf3)
# For visualisation so that we can preserve the mosaic of results we also need a buffer for 0-3km only
BTRW_pop_buf_agg <- buffer(BTRW_pops, 3000) 
plot(BTRW_pop_buf); BTRW_pop_buf


# 5. Stepping stone habitat or corridor creation ----
BTRW_pops; plot(BTRW_pops)
# 5.1 Identify populations with low connectivity (below the 75th position between min and max connectivity) ----
HSM_cur <- rast('./03_Results/Resistance_surfaces/Habitat_suitability/Habitat_suitability_output_cum_curmap.asc') %>% 
  mask(Aus) 
HSM_q99 <- global(HSM_cur, quantile, probs = 0.99, na.rm = T)[[1]]
HSM_win <- clamp(HSM_cur, upper = HSM_q99, values = T)
mn_HSM <- global(HSM_win, "min", na.rm = T)[[1]]
mx_HSM <- global(HSM_win, "max", na.rm = T)[[1]]
HSM_cur <- (HSM_win - mn_HSM)/ (mx_HSM - mn_HSM)
HSM_cur; plet(HSM_cur)


BTRW_pop_con <- extract(HSM_cur, BTRW_pops)
unique(round((BTRW_pop_con$Habitat_suitability_output_cum_curmap)))

BTRW_pops$connected <- ifelse(BTRW_pop_con$Habitat_suitability_output_cum_curmap <= 0.40, 0, 1) # If this is failing to run, check version of terra as updated versions throw an error at this line
BTRW_pops$x <- crds(centroids(BTRW_pops))[,1]
BTRW_pops$y <- crds(centroids(BTRW_pops))[,2]


BTRW_pop_noncons <- BTRW_pops[BTRW_pops$connected ==0, ]
dim(BTRW_pop_noncons) # 59 of 115 populations are considered to not be well connected.

# 5.2 Find nearest high connectivity population ----
# Folllowing https://gis.stackexchange.com/questions/437968/r-finding-closest-point-to-each-point-and-filtering-which-points-to-consider
n <- 115
BTRW_dist <- st_distance(st_as_sf(BTRW_pops))
BTRW_dist_df <- as.data.frame(expand.grid(i=1:n, j = 1:n))
BTRW_dist_df$dist <- c(BTRW_dist)
BTRW_dist_df$pop_i <- BTRW_pops$pop_id[BTRW_dist_df$i]
BTRW_dist_df$pop_j <- BTRW_pops$pop_id[BTRW_dist_df$j]
BTRW_dist_df$connectivity_i <- BTRW_pops$connected[BTRW_dist_df$i]
BTRW_dist_df$connectivity_j <- BTRW_pops$connected[BTRW_dist_df$j]
head(BTRW_dist_df); unique(BTRW_dist_df$connectivity_i)

# For each population (pop_j) find the nearest population with connectivity (connectivity_i) == 1
BTRW_dist_df <- BTRW_dist_df[BTRW_dist_df$pop_i != BTRW_dist_df$pop_j & BTRW_dist_df$connectivity_j != 1 & BTRW_dist_df$connectivity_i != 0, ] # Remove rows where the population ID is the same, then remove rows where population j is highly connected (not needing to be connected to a high connectivity population), then remove rows where population i has low connectivity

# Now for each population j find the nearest high connectivity population i
BTRW_cons_pop <- data.frame()
pops <- unique(BTRW_dist_df$pop_j)
for(i in pops){
  dist_df <- BTRW_dist_df[BTRW_dist_df$pop_j == i, ]
  dist_df <- dist_df[order(dist_df$dist), ]
  BTRW_cons_pop <- rbind(BTRW_cons_pop, dist_df[1, c(3, 5,4)])
}
BTRW_cons_pop 
names(BTRW_cons_pop) <- c("Distance", "Low_con_pop", "High_con_pop_for_connection")
head(BTRW_cons_pop)


# 5.3 Create lines to connect low connectivity populations to high connectivity populations along pathways of least resistance to movement ----
# 5.3.1 Create origin and destination points ----
BTRW_cor_origin <- centroids(vect(st_as_sf(BTRW_pop_noncons[, c(1,6,7,8,9)], coords = c('x', 'y'), crs = 'EPSG:3577')))
plot(BTRW_cor_origin)


BTRW_cor_dest <- as.data.frame(BTRW_cons_pop$High_con_pop_for_connection)
colnames(BTRW_cor_dest) <- 'pop_id'
BTRW_cor_dest$x <- BTRW_pops$x[match(BTRW_cor_dest$pop_id, BTRW_pops$pop_id)]
BTRW_cor_dest$y <- BTRW_pops$y[match(BTRW_cor_dest$pop_id, BTRW_pops$pop_id)]
head(BTRW_cor_dest)
BTRW_cor_dest <- vect(st_as_sf(BTRW_cor_dest, coords = c('x', 'y'), crs = 'EPSG:3577'))
plot(BTRW_cor_dest, add = T, col = 'blue')


# 5.3.2 Create conductance matrix  ----
# According to the following Stack Overflow post which the Joseph Lewis (leastcostpath package creator) answered, create_cs() actually expects conductance values with higher values being more conductive/less costly so inversion of cumulative current is not required prior to create cost surface for least cost path creation https://stackoverflow.com/questions/77975556/incoherent-least-cost-path
hsm_matrix <- create_cs(x = HSM_cur <- rast('./03_Results/Resistance_surfaces/Habitat_suitability/Habitat_suitability_output_cum_curmap.asc') %>% 
                          mask(Aus))
plot(hsm_matrix)

# 5.3.4 Create least cost path ----
BTRW_connectivity <- vect()

for(i in 1:nrow(BTRW_cor_origin)){
  BTRW_lcp <- create_lcp(x = hsm_matrix,
                         origin = BTRW_cor_origin[i], 
                         destination = BTRW_cor_dest[i])
  BTRW_connectivity <- rbind(BTRW_connectivity, BTRW_lcp)
}
BTRW_connectivity
BTRW_connectivity$pop_id <- BTRW_cor_origin$pop_id
BTRW_connectivity$isolation_rank <- BTRW_cor_origin$isolation_rank
plot(centroids(BTRW_pops[BTRW_pops$connected == 0,]), col = 'red');plot(BTRW_cor_dest, add = T); plot(BTRW_connectivity, col = 'blue', add = T) # Check all non-connected populations have a connection
crs(BTRW_connectivity) <- 'EPSG:3577'
plot(hsm_matrix); plot(BTRW_connectivity, add = T) # Check pathways are going through areas that would be less resistant.

# 5.4 Create buffers around connectivity corridors ----
# Now we can extract information for buffers around these lines in to determine where restoration is needed.
BTRW_connectivity_buf1 <- buffer(BTRW_connectivity, 1000)
BTRW_connectivity_buf1$dist <- "0-1km"
length(unique(BTRW_connectivity_buf1$pop_id))
plot(BTRW_connectivity_buf1)
plot(aggregate(BTRW_connectivity_buf1))

BTRW_connectivity_buf2 <- buffer(BTRW_connectivity, 2000) 
BTRW_connectivity_buf2$dist <- "0-2km"
length(unique(BTRW_connectivity_buf2$pop_id))

BTRW_connectivity_buf3 <- buffer(BTRW_connectivity, 3000) 
BTRW_connectivity_buf3$dist <- "0-3km"
length(unique(BTRW_connectivity_buf3$pop_id))



BTRW_connectivity_buf <- rbind(BTRW_connectivity_buf1, BTRW_connectivity_buf2, BTRW_connectivity_buf3)
length(unique(BTRW_connectivity_buf$pop_id))

BTRW_connectivity_buf$x <- crds(centroids(BTRW_connectivity_buf))[,1]
BTRW_connectivity_buf$y <- crds(centroids(BTRW_connectivity_buf))[,2]
plot(BTRW_connectivity_buf); plot(BTRW_connectivity, col = 'red', add = T)




# 5.5 Extract landscape information for connectivity corridors ----
# Habitat suitability
hsm <- vect('./00_Data/BTRW_data/DES HSM_Petrogale penicillata/DES HSM_Petrogale penicillata.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)
head(hsm)
hsm <- hsm[,c(14:16,18:19)] # Keep columns related to habitat suitability
hsm$HSM_SUIT <- factor(hsm$HSM_SUIT, levels = c("Very low", "Low", "Medium", "High", "Very high")) # Reorganise categorical levels
hsm$hsm_num <- ifelse(hsm$HSM_SUIT == "Very low", 1, NA)
hsm$hsm_num <- ifelse(hsm$HSM_SUIT == "Low", 2, hsm$hsm_num)
hsm$hsm_num <- ifelse(hsm$HSM_SUIT == "Medium", 3, hsm$hsm_num)
hsm$hsm_num <- ifelse(hsm$HSM_SUIT == "High", 4, hsm$hsm_num)
hsm$hsm_num <- ifelse(hsm$HSM_SUIT == "Very high", 5, hsm$hsm_num)
hsm 


# Land use
landuse <- rast('./00_Data/Environmental_data/NLUM_v7_250_ALUMV8_2020_21_alb_package_20241128/NLUM_v7_250_ALUMV8_2020_21_alb.tif') %>% 
  crop(e)
activeCat(landuse) <- 'SIMP'
cats_landuse <- cats(landuse)[[1]]
cats_landuse$SIMP[cats_landuse$SIMP == "Nature conservation" | cats_landuse$SIMP == "Managed resource protection"] <- "Conservation area" 
cats_landuse$SIMP[cats_landuse$SIMP == "Grazing native vegetation" | cats_landuse$SIMP == "Production native forests" | cats_landuse$SIMP == "Plantation forests" | cats_landuse$SIMP == "Grazing modified pastures" | cats_landuse$SIMP == "Dryland cropping" | cats_landuse$SIMP == "Dryland horticulture" | cats_landuse$SIMP == "Irrigated pastures" | cats_landuse$SIMP == "Irrigated cropping" | cats_landuse$SIMP == "Irrigated horticulture" | cats_landuse$SIMP == "Intensive horticulture and animal production"] <- "Agricultural land"
cats_landuse$SIMP[cats_landuse$SIMP == "Urban residential" | cats_landuse$SIMP == "Rural residential and farm infrastructure"] <- "Residential land"
cats_landuse$SIMP[cats_landuse$SIMP == "No data/offshore" | cats_landuse$SIMP == "Water"] <- "NA"
unique(cats_landuse$SIMP)
levels(landuse) <- cats_landuse
activeCat(landuse) <- "SIMP"
landuse; plot(landuse)
land <- as.polygons(landuse)
land;plot(land)
# NOTE: other intensive uses is mainly characterised by areas that have structures. 


BTRW_connectivity_buf <- st_intersection(st_as_sf(hsm), st_as_sf(BTRW_connectivity_buf))
BTRW_connectivity_buf

BTRW_connectivity_buf <- st_intersection(st_as_sf(land), st_as_sf(BTRW_connectivity_buf))
unique(is.na(BTRW_connectivity_buf))
BTRW_connectivity_buf <- vect(BTRW_connectivity_buf)




BTRW_connectivity_buf$connection <- ifelse(BTRW_connectivity_buf$HSM_VALUE >=4 & BTRW_connectivity_buf$SIMP == "Conservation area" | BTRW_connectivity_buf$SIMP == "Other minimal use", 1, NA) # Corridor creation
BTRW_connectivity_buf$connection <- ifelse(BTRW_connectivity_buf$HSM_VALUE <=3 & BTRW_connectivity_buf$SIMP == "Conservation area" | BTRW_connectivity_buf$SIMP == "Other minimal use" | BTRW_connectivity_buf$SIMP == "Agricultural land", 2, BTRW_connectivity_buf$connection) # Stepping stone habitat creation
unique(BTRW_connectivity_buf$connection) # Values can be NAs, we will assign a label to NA values in ggplot
BTRW_connectivity_buf$connection <- as.factor(BTRW_connectivity_buf$connection)
BTRW_pops$connected <- as.factor(BTRW_pops$connected)

display.brewer.all()
brewer.pal(9,'Blues')

p_con <- ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_connectivity_buf, aes(fill = connection, col = connection))+
  scale_fill_manual(values = c("#252525", "#969696", 'lightgray'), na.value = 'lightgray', labels = c("Corridor", "Stepping stone habitat", "Infrastructure dominated land"), name = "Connection creation type") +
  scale_color_manual(values = c("#252525", "#969696", 'lightgray'), na.value = 'lightgray', labels = c("Corridor", "Stepping stone habitat", "Infrastructure dominated land"), name = "Connection creation type") +
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  new_scale_color()+
  new_scale_fill()+
  geom_spatvector(data = BTRW_pops, aes(fill = connected), col = 'black', lwd = 0.1) + 
  scale_fill_manual(values = c("#9ECAE1", "#1F78B4"), labels = c("Low", "High"), name = "Population connectivity") +
  labs(title = "(a)")
p_con





# To simplify each connection from low to high connectivity populations, lets determine the connection creation type which is the majority
BTRW_connection <- table(BTRW_connectivity_buf$pop_id, BTRW_connectivity_buf$connection)
BTRW_connection_df <- as.data.frame.matrix(BTRW_connection)
dom_con <- as.data.frame(max.col(BTRW_connection_df))
colnames(dom_con) <- "dom_con"
dom_con$pop_id <- rownames(BTRW_connection_df)
dom_con

BTRW_connectivity_buf <- merge(BTRW_connectivity_buf, dom_con, by = 'pop_id')
head(BTRW_connectivity_buf)
unique(BTRW_connectivity_buf$dom_con)
table(BTRW_connectivity_buf$dom_con) # 1 == Corridor, 2 == Stepping stone habitat
BTRW_connectivity_buf$dom_con <- factor(BTRW_connectivity_buf$dom_con)



p_dom_con <-ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_connectivity_buf[BTRW_connectivity_buf$dom_con == "2"], aes(fill = dom_con, col = dom_con)) +
  geom_spatvector(data = BTRW_connectivity_buf[BTRW_connectivity_buf$dom_con == "1"], aes(fill = dom_con, col = dom_con)) +
  scale_fill_manual(values = c("1" = "#252525", "2" = "#969696"), labels = c("1" = "Corridor", "2"  = "Stepping stone habitat"), name = "Dominant connection type") +
  scale_color_manual(values = c("1" = "#252525", "2" = "#969696"), labels = c("1" = "Corridor", "2"  = "Stepping stone habitat"), name = "Dominant connection type") +
  theme_bw() +
  theme_cowplot(font_size = 17) +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  new_scale_color()+
  new_scale_fill()+
  geom_spatvector(data = BTRW_pops, aes(fill = connected), col = 'black', lwd = 0.1) + 
  scale_fill_manual(values = c("#9ECAE1", "#1F78B4"), labels = c("Low", "High"), name = "Population connectivity") +
  labs(title = "(b)")
p_dom_con

plot_grid(p_con, p_dom_con, nrow = 2) 
ggsave("./03_Results/Plots/Connectivity_creation.png", width = 20, height = 32, dpi = 300, units = 'cm')



# Graphical abstract map
p_dom_con2 <-ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_connectivity_buf[BTRW_connectivity_buf$dom_con == "2"], aes(fill = dom_con, col = dom_con)) +
  geom_spatvector(data = BTRW_connectivity_buf[BTRW_connectivity_buf$dom_con == "1"], aes(fill = dom_con, col = dom_con)) +
  scale_fill_manual(values = c("1" = "#252525", "2" = "#969696"), labels = c("1" = "Corridor", "2"  = "Stepping stone habitat"), name = "Dominant connection type") +
  scale_color_manual(values = c("1" = "#252525", "2" = "#969696"), labels = c("1" = "Corridor", "2"  = "Stepping stone habitat"), name = "Dominant connection type") +
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))+
  new_scale_color()+
  new_scale_fill()+
  geom_spatvector(data = BTRW_pops, aes(fill = connected), col = 'black', lwd = 0.1) + 
  scale_fill_manual(values = c("#9ECAE1", "#1F78B4"), labels = c("Low", "High"), name = "Population connectivity", guide = "none") # Supress legend
p_dom_con2
ggsave("./03_Results/Plots/Connection_type_graph_ab.png", width = 20, height = 16, dpi = 300, units = 'cm')


# How many populations
BTRW_dom_con_pop <- BTRW_connectivity_buf %>% 
  distinct(pop_id, dom_con, .keep_all = TRUE)
table(BTRW_dom_con_pop$dom_con)
table(BTRW_dom_con_pop$dom_con, BTRW_dom_con_pop$isolation_rank)


# 6. Fire regime management  ----
# 6.1 For population buffers ----
BTRW_fire_freq <- extract(fire_freq, BTRW_pop_buf) %>% 
  print()
BTRW_pop_buf$Fire_frequency <- BTRW_fire_freq$layer

BTRW_fire_freq_full <- extract(fire_freq, BTRW_pop_buf_agg)
BTRW_pop_buf_agg$Fire_frequency <- BTRW_fire_freq_full$layer

BTRW_fire_reg <- st_intersection(st_as_sf(BTRW_pop_buf), st_as_sf(fire_reg)) # Use intersect for polygon - polygon information extraction
length(unique(BTRW_fire_reg$pop_id)) # 114
length(unique(BTRW_pops$pop_id)) # 115
# One population has no fire data
BTRW_fire_reg <- vect(BTRW_fire_reg)
unique(BTRW_fire_reg$FREQUENCY)
unique(BTRW_fire_reg$min_ff)



BTRW_fire_reg_full <- st_intersection(st_as_sf(BTRW_pop_buf_agg), st_as_sf(fire_reg)) # Use intersect for polygon - polygon information extraction
length(unique(BTRW_fire_reg_full$pop_id)) # 114
length(unique(BTRW_pops$pop_id)) # 115
# One population has no fire data
BTRW_fire_reg_full <- vect(BTRW_fire_reg_full)



# Determine fire regime frequency status
# All buffers
BTRW_fire_reg$frequency_status <- ifelse(BTRW_fire_reg$Fire_frequency > BTRW_fire_reg$max_ff, "Higher", NA)
BTRW_fire_reg$frequency_status <- ifelse(BTRW_fire_reg$Fire_frequency < BTRW_fire_reg$min_ff, "Lower", BTRW_fire_reg$frequency_status)
BTRW_fire_reg$frequency_status <- ifelse(is.na(BTRW_fire_reg$frequency_status), "Within", BTRW_fire_reg$frequency_status)
unique(BTRW_fire_reg$frequency_status)
head(BTRW_fire_reg)
unique(round(BTRW_fire_reg$Fire_frequency[BTRW_fire_reg$frequency_status == "Higher"])) # Only 1 to 2 fires


BTRW_fire_reg$status <- ifelse(BTRW_fire_reg$frequency_status == "Lower", 1, NA)
BTRW_fire_reg$status <- ifelse(BTRW_fire_reg$frequency_status == "Within", 2, BTRW_fire_reg$status)
BTRW_fire_reg$status <- ifelse(BTRW_fire_reg$frequency_status == "Higher" & round(BTRW_fire_reg$Fire_frequency) <= 1, 3, BTRW_fire_reg$status)
BTRW_fire_reg$status <- ifelse(BTRW_fire_reg$frequency_status == "Higher" & round(BTRW_fire_reg$Fire_frequency) >=2, 4, BTRW_fire_reg$status)
unique(BTRW_fire_reg$status)
table(BTRW_fire_reg$frequency_status, BTRW_fire_reg$dist) # Within their dispersal distance a lot of populations have been exposed to higher fire frequencies than recommended, similar number within recommendations. Relatively fewer at lower recommendations.

# Full 3km extent
BTRW_fire_reg_full$frequency_status <- ifelse(BTRW_fire_reg_full$Fire_frequency > BTRW_fire_reg_full$max_ff, "Higher", NA)
BTRW_fire_reg_full$frequency_status <- ifelse(BTRW_fire_reg_full$Fire_frequency < BTRW_fire_reg_full$min_ff, "Lower", BTRW_fire_reg_full$frequency_status)
BTRW_fire_reg_full$frequency_status <- ifelse(is.na(BTRW_fire_reg_full$frequency_status), "Within", BTRW_fire_reg_full$frequency_status)
unique(BTRW_fire_reg_full$frequency_status)
head(BTRW_fire_reg_full)
unique(round(BTRW_fire_reg_full$Fire_frequency[BTRW_fire_reg_full$frequency_status == "Higher"])) # Only 1 to 2 fires


BTRW_fire_reg_full$status <- ifelse(BTRW_fire_reg_full$frequency_status == "Lower", 1, NA)
BTRW_fire_reg_full$status <- ifelse(BTRW_fire_reg_full$frequency_status == "Within", 2, BTRW_fire_reg_full$status)
BTRW_fire_reg_full$status <- ifelse(BTRW_fire_reg_full$frequency_status == "Higher" & round(BTRW_fire_reg_full$Fire_frequency) <= 1, 3, BTRW_fire_reg_full$status)
BTRW_fire_reg_full$status <- ifelse(BTRW_fire_reg_full$frequency_status == "Higher" & round(BTRW_fire_reg_full$Fire_frequency) >=2, 4, BTRW_fire_reg_full$status)
unique(BTRW_fire_reg_full$status)





pal1 <-  c("#92C5DE", "lightgray", "#F4A582", "#E58267", "#D6604D")
BTRW_fire_reg_full <- BTRW_fire_reg_full %>% 
  arrange(status) 
freq_stat <- 
  ggplot() +
  geom_spatvector(data = Aus, fill = "transparent") +
  geom_spatvector(data = BTRW_fire_reg_full, aes(fill = status, col = status))+
  scale_fill_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5),  labels = c("Lower", "Within", "", "", "Higher"), limits = c(1,5))+
  scale_color_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5), labels = c("Lower", "Within", "", "", "Higher"), limits = c(1,5))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  geom_spatvector(data = BTRW_pops, fill = NA, col = "black", size = 1, aes(alpha = 1), linewidth = 0.1) +
  labs(fill = "Fire frequency \nstatus", col = "Fire frequency \nstatus", title = "(b)", alpha = "") +
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
freq_stat

fire_freq_r <- round(fire_freq)
fire_hist <- ggplot()+
  geom_spatvector(data = Aus, fill = "transparent") +
  geom_spatraster(data = fire_freq_r) +
  scale_fill_viridis_c(na.value = 'transparent', limits = c(1,9), breaks = c(1, 3, 5, 7, 9)) +
  labs(fill = 'Fire frequency', title = "(a)")+
  theme_bw()+
  theme_cowplot(font_size = 17)+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())
fire_hist


# Determine the distribution of populations to each frequency status over each distance buffer
BTRW_freq_pop <- BTRW_fire_reg %>% 
  distinct(pop_id, frequency_status, dist, .keep_all = T)

table(BTRW_freq_pop$frequency_status, BTRW_freq_pop$dist) # Populations can fall into multiple frequency status classes at the same distance as fire history varies over fine spatial scales, so the area of one buffer does not necessarily have one fire history.
# We can see that in the majority of cases, fire regimes are higher than ecologically recommended


# Calculate proportion of areas of populations that have burnt once in 36 years
BTRW_freq_full <- BTRW_fire_reg_full %>% 
  distinct(pop_id, frequency_status, .keep_all = T)
BTRW_status <- tidyterra::count(BTRW_freq_full, status)
BTRW_stat_total <- sum(BTRW_status$n[3:4])

(BTRW_status$n[3]/BTRW_stat_total)*100 # 72% of population areas under higher than recommended fire frequencies exposed to 

# Calculate proportion of areas of populations that have burnt twice in 36 years
(BTRW_status$n[4]/BTRW_stat_total)*100 # 28% 



BTRW_FVG_pop <- as.data.frame(BTRW_fire_reg_full)
BTRW_FVG_pop <- BTRW_FVG_pop[!duplicated(paste(BTRW_FVG_pop$pop_id, BTRW_FVG_pop$FVG, BTRW_FVG_pop$frequency_status)), ]


table(BTRW_FVG_pop$FVG, BTRW_FVG_pop$frequency_status) # Mostly due to fire sensitive vegetation being burnt at higher fire frequencies than recommended.
table(BTRW_FVG_pop$FVG, BTRW_FVG_pop$FREQUENCY) # Fire sensitive vegetation has recommendations that are not to burn so any fire in these systems will result in higher than recommended fire frequencies.



# 6.2 For connectivity corridors/habitat ----
BTRW_con_fire <- extract(fire_freq, BTRW_connectivity_buf) %>% 
  print()
BTRW_connectivity_buf$Fire_frequency <- BTRW_con_fire$layer

BTRW_con_reg <- st_intersection(st_as_sf(BTRW_connectivity_buf), st_as_sf(fire_reg)) # Use intersect for polygon - polygon information extraction
length(unique(BTRW_con_reg$pop_id)) # 49
length(unique(BTRW_connectivity_buf$pop_id)) # 49

BTRW_con_reg <- vect(BTRW_con_reg)
unique(BTRW_con_reg$FREQUENCY)
unique(BTRW_con_reg$min_ff)
unique(round(BTRW_con_reg$Fire_frequency))

# Determine fire regime frequency status
BTRW_con_reg$frequency_status <- ifelse(BTRW_con_reg$Fire_frequency > BTRW_con_reg$max_ff, "Higher", NA)
BTRW_con_reg$frequency_status <- ifelse(BTRW_con_reg$Fire_frequency < BTRW_con_reg$min_ff, "Lower", BTRW_con_reg$frequency_status)
BTRW_con_reg$frequency_status <- ifelse(is.na(BTRW_con_reg$frequency_status), "Within", BTRW_con_reg$frequency_status)
unique(BTRW_con_reg$frequency_status)
head(BTRW_con_reg)
unique(round(BTRW_con_reg$Fire_frequency[BTRW_con_reg$frequency_status == "Higher"])) # Along corridors, areas with fire regimes higher than ecological recommendations have been burnt up to 4 times in 36 years, will add extra status category for those that are higher to reflect this.


BTRW_con_reg$status <- ifelse(BTRW_con_reg$frequency_status == "Lower", 1, NA)
BTRW_con_reg$status <- ifelse(BTRW_con_reg$frequency_status == "Within", 2, BTRW_con_reg$status)
BTRW_con_reg$status <- ifelse(BTRW_con_reg$frequency_status == "Higher" & round(BTRW_con_reg$Fire_frequency) <= 1, 3, BTRW_con_reg$status)
BTRW_con_reg$status <- ifelse(BTRW_con_reg$frequency_status == "Higher" & round(BTRW_con_reg$Fire_frequency) >=2 & round(BTRW_con_reg$Fire_frequency) <3, 4, BTRW_con_reg$status)
BTRW_con_reg$status <- ifelse(BTRW_con_reg$frequency_status == "Higher" & round(BTRW_con_reg$Fire_frequency) >=3, 5, BTRW_con_reg$status)
unique(BTRW_con_reg$status)


BTRW_con_reg <- BTRW_con_reg %>% 
  arrange(status) 

cor_freq <- ggplot() +
  geom_spatvector(data = Aus, fill = "transparent") +
  geom_spatvector(data = BTRW_con_reg, aes(fill = status, col = status))+
  scale_fill_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5),  labels = c("Lower", "Within", "", "", "Higher"))+
  scale_color_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5), labels = c("Lower", "Within", "", "", "Higher"))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17)+
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  geom_spatvector(data = BTRW_pops, fill = NA, col = "black", size = 1, aes(alpha = 1), linewidth = 0.1) +
  labs(fill = "Fire frequency \nstatus", col = "Fire frequency \nstatus", title = "(c)", alpha = "") +
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
cor_freq


fire_leg <- get_legend(cor_freq + theme(legend.direction = 'horizontal', legend.position = 'bottom', legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(0.5, 'cm')))
hist_leg <- get_legend(fire_hist)

top_p <- plot_grid(fire_hist+ theme(legend.position = 'none'), hist_leg, ncol = 2, rel_widths = c(1, 1))
bottom_p <- plot_grid(freq_stat + theme(legend.position = 'none'), cor_freq + theme(legend.position = 'none'))
plot_grid(top_p, bottom_p, fire_leg, nrow = 3, rel_heights = c(1,1,0.15))

ggsave("./03_Results/Plots/Fire_regime_management.png", width = 28, height = 48, dpi = 300, units = 'cm')

# Dominant fire frequency for corridors 
BTRW_cor_fire <- as.data.frame.matrix(table(BTRW_con_reg$pop_id, BTRW_con_reg$frequency_status))
labels <- c("1" = "Higher", "2" = "Lower", "3" = "Within")
labels[as.character(max.col(BTRW_cor_fire))] %>%  table()




# Calculate proportion of areas of populations that have burnt once in 36 years
BTRW_con_status <- tidyterra::count(BTRW_con_reg, status)
BTRW_con_stat_total <- sum(BTRW_con_status$n[3:5])

(BTRW_con_status$n[3]/BTRW_con_stat_total)*100 # 49% of population areas under higher than recommended fire frequencies exposed to 
(BTRW_con_status$n[4]/BTRW_con_stat_total)*100 # 39% 
(BTRW_con_status$n[5]/BTRW_con_stat_total)*100 # 12% 

BTRW_FVG_con <- as.data.frame(BTRW_con_reg)
BTRW_FVG_con <- BTRW_FVG_con[!duplicated(paste(BTRW_FVG_con$pop_id, BTRW_FVG_con$FVG, BTRW_FVG_con$frequency_status)), ]


table(BTRW_FVG_con$FVG, BTRW_FVG_con$frequency_status) # Mostly due to fire sensitive vegetation being burnt at higher fire frequencies than recommended.
table(BTRW_FVG_pop$FVG, BTRW_FVG_pop$FREQUENCY)



# Check how many connectivity buffers had data
ggplot() +
  geom_spatvector(data = Aus, fill = "transparent") +
  geom_spatvector(data = BTRW_con_reg, aes(fill = status, col = status))+
  geom_spatvector(data = BTRW_connectivity_buf3, fill = NA, col = "blue", linewidth = 0.1) + 
  scale_fill_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5),  labels = c("Lower", "Within", "", "", "Higher"))+
  scale_color_continuous(palette = pal1, breaks = c(1, 2, 3, 4, 5), labels = c("Lower", "Within", "", "", "Higher"))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17)+
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  geom_spatvector(data = BTRW_pops, fill = NA, col = "black", size = 1, aes(alpha = 1), linewidth = 0.1) +
  labs(fill = "Fire frequency \nstatus", col = "Fire frequency \nstatus", title = "(c)", alpha = "") +
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))


# 7. Sites for revegetation -----
# Does the site have remnant vegetation cover
# Does the site have the most common NDVI for that vegetation type

BVG <- rast('./00_Data/Environmental_data/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb', lyrs = "NVIS7_0_AUST_EXT_MVG_ALB")
BVG <-  crop(project(BVG, 'EPSG:3577'), e)
unique(BVG$NVIS7_0_AUST_EXT_MVG_ALB)
BVG1 <- BVG
BVG$remnant <- as.factor(ifel(BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Sea and estuaries" | BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Inland aquatic - freshwater, salt lakes, lagoons" | BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Cleared, non-native vegetation, buildings", 0, 1))
BVG$remnant <- as.factor(ifel(BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Naturally bare - sand, rock, claypan, mudflat",2, BVG$remnant)) # Naturally bare

NDVI <- rast('./00_Data/Environmental_data/Outputs/BOM_NDVI/Average_NDVI_aoi_cropped_test.asc') %>% 
  print()

BVG$NDVI <- NDVI$NDVI
BVG; plot(BVG)

# Determine most common NDVI for each vegetation type
BVG_veg <- unique(values(BVG$NVIS7_0_AUST_EXT_MVG_ALB))
BVG_veg
BVG_veg <- BVG_veg[!is.na(BVG_veg)]


get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create an empty layer for the raster
BVG$modal_NDVI <- NA

# Find the modal NDVI for each vegetation type
for(i in BVG_veg) {
  nvis_vals <- values(BVG$NVIS7_0_AUST_EXT_MVG_ALB)
  ndvi_vals <- values(BVG$NDVI)
  
  BVG_ndvi <- ndvi_vals[nvis_vals == i & !is.na(nvis_vals) & !is.na(ndvi_vals)]
  modal_val <- get_mode(BVG_ndvi)
  
  BVG_i <- BVG$NVIS7_0_AUST_EXT_MVG_ALB == i
  BVG$modal_NDVI <- ifel(BVG_i, modal_val, BVG$modal_NDVI)
}
unique(values(BVG$modal_NDVI))
head(BVG)

# Determine where NDVI is lower than the modal NDVI, identifying where revegetation is needed as ecosystem may be degraded despite remnant cover
BVG$NDVI_reveg <- ifel(BVG$NDVI < BVG$modal_NDVI, 0, 1)
plot(BVG$NDVI_reveg)


# Extract information for population buffers and connectivity corridors 
BTRW_pop_buf; plot(BTRW_pop_buf); dim(BTRW_pop_buf)
BTRW_connectivity_buf; plot(BTRW_connectivity_buf); dim(BTRW_connectivity_buf)

# For plotting information over the full buffer extent
Rem_veg_pop_full <- extract(BVG, BTRW_pop_buf_agg)
head(Rem_veg_pop_full); dim(Rem_veg_pop_full)
BTRW_pop_buf_agg$Remnant_veg <- as.factor(Rem_veg_pop_full$remnant)
BTRW_pop_buf_agg$NDVI_reveg <- as.factor(Rem_veg_pop_full$NDVI_reveg)
BTRW_pop_buf_agg$NDVI <- Rem_veg_pop_full$NDVI
BTRW_pop_buf_agg$modal_NDVI <- Rem_veg_pop_full$modal_NDVI
head(BTRW_pop_buf_agg); dim(BTRW_pop_buf_agg)

BTRW_pop_buf_agg$reveg <- ifelse(BTRW_pop_buf_agg$Remnant_veg == 2 & BTRW_pop_buf_agg$NDVI_reveg == 1, 0, NA) # No revegetation
BTRW_pop_buf_agg$reveg <- ifelse(BTRW_pop_buf_agg$Remnant_veg == 1 & BTRW_pop_buf_agg$NDVI_reveg == 1, 1, BTRW_pop_buf_agg$reveg) # No revegetation
BTRW_pop_buf_agg$reveg <- ifelse(BTRW_pop_buf_agg$Remnant_veg == 1 & BTRW_pop_buf_agg$NDVI_reveg == 0, 2, BTRW_pop_buf_agg$reveg) # If remnant vegetation cover is present but NDVI is suboptimal, some revegetation needed
BTRW_pop_buf_agg$reveg <- ifelse(BTRW_pop_buf_agg$Remnant_veg == 0 & BTRW_pop_buf_agg$NDVI_reveg == 1, 3, BTRW_pop_buf_agg$reveg)
BTRW_pop_buf_agg$reveg <- ifelse(BTRW_pop_buf_agg$Remnant_veg == 0 & BTRW_pop_buf_agg$NDVI_reveg == 0, 4, BTRW_pop_buf_agg$reveg)
unique(BTRW_pop_buf_agg$reveg)
table(BTRW_pop_buf_agg$pop_id, BTRW_pop_buf_agg$reveg) # Different approach needed to preserve the mosaic of values

# Create new BVG raster showing revegetation information
BVG_reveg <- BVG
BVG_reveg$reveg <- NA
BVG_reveg$reveg <- ifel(BVG_reveg$remnant == 2 & BVG_reveg$NDVI_reveg == 1, 0, BVG_reveg$reveg)
BVG_reveg$reveg <- ifel(BVG_reveg$remnant == 1 & BVG_reveg$NDVI_reveg == 1, 1, BVG_reveg$reveg)
BVG_reveg$reveg <- ifel(BVG_reveg$remnant == 1 & BVG_reveg$NDVI_reveg == 0, 2, BVG_reveg$reveg)
BVG_reveg$reveg <- ifel(BVG_reveg$remnant == 0 & BVG_reveg$NDVI_reveg == 1, 3, BVG_reveg$reveg)
BVG_reveg$reveg <- ifel(BVG_reveg$remnant == 0 & BVG_reveg$NDVI_reveg == 0, 4, BVG_reveg$reveg)

# For population buffers
BVG_reveg_pop <- mask(BVG_reveg$reveg, BTRW_pop_buf_agg)
BVG_reveg_pop # No naturally bare surfaces 

# For connectivity corridors 
BVG_reveg_con <- mask(BVG_reveg$reveg, BTRW_connectivity_buf)
BVG_reveg_con # No naturally bare surfaces



# For each population buffer
Rem_veg_pop <- extract(BVG, BTRW_pop_buf)
head(Rem_veg_pop); dim(Rem_veg_pop)
Rem_veg_pop$dist <- BTRW_pop_buf$dist[Rem_veg_pop$ID] # Add distance information
head(Rem_veg_pop); dim(Rem_veg_pop)
Rem_veg_pop$reveg <- ifelse(Rem_veg_pop$remnant == 2 & Rem_veg_pop$NDVI_reveg == 1, 0, NA) # No revegetation
Rem_veg_pop$reveg <- ifelse(Rem_veg_pop$remnant == 1 & Rem_veg_pop$NDVI_reveg == 1, 1, Rem_veg_pop$reveg) # No revegetation
Rem_veg_pop$reveg <- ifelse(Rem_veg_pop$remnant == 1 & Rem_veg_pop$NDVI_reveg == 0, 2, Rem_veg_pop$reveg) # If remnant vegetation cover is present but NDVI is suboptimal, some revegetation needed
Rem_veg_pop$reveg <- ifelse(Rem_veg_pop$remnant == 0 & Rem_veg_pop$NDVI_reveg == 1, 3, Rem_veg_pop$reveg)
Rem_veg_pop$reveg <- ifelse(Rem_veg_pop$remnant == 0 & Rem_veg_pop$NDVI_reveg == 0, 4, Rem_veg_pop$reveg)
unique(Rem_veg_pop$reveg)

# Get information for the connectivity corridors but use masked raster for plotting
#Rem_veg_con <- extract(BVG, BTRW_connectivity_buf) # Takes a while to run so hashed to avoid accidentally running when not needed
head(Rem_veg_con); dim(Rem_veg_con)
Rem_veg_con$dist <- BTRW_connectivity_buf$dist[Rem_veg_con$ID]
head(Rem_veg_con); dim(Rem_veg_con)
Rem_veg_con$reveg <- ifelse(Rem_veg_con$remnant == 2 & Rem_veg_con$NDVI_reveg == 1, 0, NA) # No revegetation
Rem_veg_con$reveg <- ifelse(Rem_veg_con$remnant == 1 & Rem_veg_con$NDVI_reveg == 1, 1, Rem_veg_con$reveg) # No revegetation
Rem_veg_con$reveg <- ifelse(Rem_veg_con$remnant == 1 & Rem_veg_con$NDVI_reveg == 0, 2, Rem_veg_con$reveg) # If remnant vegetation cover is present but NDVI is suboptimal, some revegetation needed
Rem_veg_con$reveg <- ifelse(Rem_veg_con$remnant == 0 & Rem_veg_con$NDVI_reveg == 1, 3, Rem_veg_con$reveg)
Rem_veg_con$reveg <- ifelse(Rem_veg_con$remnant == 0 & Rem_veg_con$NDVI_reveg == 0, 4, Rem_veg_con$reveg)
unique(Rem_veg_con$reveg)



write.csv(Rem_veg_pop_full, './03_Results/BTRW_pop_rem_veg_full.csv', col.names = T)
write.csv(Rem_veg_pop, './03_Results/BTRW_pop_rem_veg.csv')
write.csv(Rem_veg_con, './03_Results/BTRW_corridor_veg.csv')


# Plot opportunities for revegetation
brewer.pal(6, 'Greys')
pal2 <- c("#CCCCCC", "#969696", "#636363", "#252525")

veg_pop <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
        legend.box.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.margin = margin(0, 1.2, 0, 0))+
    geom_spatraster(data = BVG_reveg_pop)+
    scale_fill_continuous(palette = pal2, breaks = c(0,1,2,3,4), labels = c('Naturally bare', 'Remnant optimal NDVI', ' Remnant suboptimal NDVI', 'Non-remnant optimal NDVI', 'Non-remnant suboptimal NDVI'), name = 'Vegetation cover', na.value = 'transparent') +
  labs(title = "(a)", alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1)) +
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
veg_pop

veg_con <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 17) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
        legend.box.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.margin = margin(0, 1.2, 0, 0))+
  geom_spatraster(data = BVG_reveg_con)+
  scale_fill_continuous(palette = pal2, breaks = c(0,1,2,3,4), labels = c('Naturally bare', 'Remnant optimal NDVI', ' Remnant suboptimal NDVI', 'Non-remnant optimal NDVI', 'Non-remnant suboptimal NDVI'), name = 'Vegetation cover', na.value = 'transparent') +
  labs(title = "(b)", alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
veg_con

plot_grid(veg_pop, veg_con + theme(legend.position = "none"), align = 'v', nrow = 2)
ggsave('./03_Results/Plots/Revegetation_opportunities_combo.png', width = 20, height = 32, dpi = 300, units = 'cm')

  
# Investigate revegetation opportunities further
# For population centre
BVG_pop_cen <- extract(BVG, buffer(BTRW_pops, 1000))
head(BVG_pop_cen)
BVG_pop_cen$reveg <- ifelse(BVG_pop_cen$remnant == 2 & BVG_pop_cen$NDVI_reveg == 1, 0, NA) # No revegetation
BVG_pop_cen$reveg <- ifelse(BVG_pop_cen$remnant == 1 & BVG_pop_cen$NDVI_reveg == 1, 1, BVG_pop_cen$reveg) # No revegetation
BVG_pop_cen$reveg <- ifelse(BVG_pop_cen$remnant == 1 & BVG_pop_cen$NDVI_reveg == 0, 2, BVG_pop_cen$reveg) # If remnant vegetation cover is present but NDVI is suboptimal, some revegetation needed
BVG_pop_cen$reveg <- ifelse(BVG_pop_cen$remnant == 0 & BVG_pop_cen$NDVI_reveg == 1, 3, BVG_pop_cen$reveg)
BVG_pop_cen$reveg <- ifelse(BVG_pop_cen$remnant == 0 & BVG_pop_cen$NDVI_reveg == 0, 4, BVG_pop_cen$reveg)


plot(mask(BVG_reveg$reveg, buffer(BTRW_pops, 1000)))

BTRW_cen_veg <- BVG_pop_cen %>% 
  distinct(ID, reveg, .keep_all = T)
table(BTRW_cen_veg$reveg)


BTRW_cenveg_count <- count(BTRW_cen_veg, reveg)
BTRW_cenveg_total <- sum(BTRW_cenveg_count$n[1:4])
(BTRW_cenveg_count$n[1]/BTRW_cenveg_total)*100 # 32
(BTRW_cenveg_count$n[2]/BTRW_cenveg_total)*100 #34
(BTRW_cenveg_count$n[3]/BTRW_cenveg_total)*100# 34
(BTRW_cenveg_count$n[4]/BTRW_cenveg_total)*100# 0.60

BTRW_pop_veg <- Rem_veg_pop %>% 
  distinct(ID, reveg, dist, .keep_all = T)
BTRW_pop_veg
table(BTRW_pop_veg$reveg, BTRW_pop_veg$dist)

BTRW_remvegpop_count <- count(BTRW_pop_veg, reveg)
BTRW_remvegpop_total <- sum(BTRW_remvegpop_count$n[1:4])
(BTRW_remvegpop_count$n[1]/BTRW_remvegpop_total)*100 # 32%
(BTRW_remvegpop_count$n[2]/BTRW_remvegpop_total)*100 #34
(BTRW_remvegpop_count$n[3]/BTRW_remvegpop_total)*100 #34
(BTRW_remvegpop_count$n[4]/BTRW_remvegpop_total)*100 #0.71


# 0 = no reveg naturally bare
# 1= no reveg as remnant with optimal NDVI
# 2 = remanant with suboptimal NDVI
# 3 = non-remnant, optimal NDVI
# 4 = non-remnant, suboptimal NDVI

BTRW_con_veg <- Rem_veg_con %>% 
  distinct(ID, reveg, dist, .keep_all = T)
table(BTRW_con_veg$reveg, BTRW_con_veg$dist)

BTRW_con_reveg <- count(BTRW_con_veg, reveg)
BTRW_reveg_total <- sum(BTRW_con_reveg$n[1:4])
(BTRW_con_reveg$n[1]/BTRW_reveg_total)*100 # 18%
(BTRW_con_reveg$n[2]/BTRW_reveg_total)*100 #28
(BTRW_con_reveg$n[3]/BTRW_reveg_total)*100 #54
(BTRW_con_reveg$n[4]/BTRW_reveg_total)*100 #0.24




# 8. Pest management ----
# 8.1 Load occurrence records -----
pest_WQ <- read.csv('./00_Data/Pest_occurrence_records/Pest Records All_WPSQ QSA FoPQ.csv', header = T)
head(pest_WQ); dim(pest_WQ) # Coordinates in EPSG:4326
pest_WQ$Precision <- "1" #Add precision information 

pest_WQ$Date.start <- ifelse(nchar(pest_WQ$Date.start) == 9, paste0("0", pest_WQ$Date.start), pest_WQ$Date.start)
pest_WQ$year <- as.character(substr(pest_WQ$Date.start, (nchar(pest_WQ$Date.start) - 3), nchar(pest_WQ$Date.start)))
pest_WQ$month <- as.character(substr(pest_WQ$Date.start, 4, 5))
pest_WQ$day <- as.character(substr(pest_WQ$Date.start, 1,2))
pest_WQ$Date <- paste0(pest_WQ$year, "-", pest_WQ$month, "-", pest_WQ$day)
pest_WQ$Datum <- "EPSG:4326"
head(pest_WQ)

pest_WQ_fox <- pest_WQ[pest_WQ$Species == "Vulpes vulpes", ]
head(pest_WQ_fox); dim(pest_WQ_fox); unique(pest_WQ_fox$Species)
pest_WQ_fox <- pest_WQ_fox[, c(7, 4:5, ncol(pest_WQ_fox), 12:ncol(pest_WQ_fox)-1)]
head(pest_WQ_fox)
colnames(pest_WQ_fox) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day', 'Date')


pest_WQ_cat <- pest_WQ[pest_WQ$Species == "Felis catus", ]
head(pest_WQ_cat); dim(pest_WQ_cat); unique(pest_WQ_cat$Species)
head(pest_WQ_cat); dim(pest_WQ_cat); unique(pest_WQ_cat$Species)
pest_WQ_cat <- pest_WQ_cat[, c(7, 4:5, ncol(pest_WQ_cat), 12:ncol(pest_WQ_cat)-1)]
head(pest_WQ_cat)
colnames(pest_WQ_cat) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day', 'Date')



# 8.1.1 Lantana camara ----
lantana_1 <- read.csv('./00_Data/Pest_occurrence_records/Lantana_camara-2026-01-13/records-2026-01-13.csv', header = T)
lantana_2 <- read.csv('./00_Data/Pest_occurrence_records/Lantana_camara_WildNet_sighting_list_primary_fields_20260113_144714.csv', header = T)
sum(lantana_1$eventDate == "")  # Some records have no date
sum(lantana_2$Start.date == "")

str(lantana_1)
lantana_1 <- lantana_1[, c('eventDate', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'coordinateUncertaintyInMeters')]
lantana_1 <- lantana_1[lantana_1$coordinateUncertaintyInMeters <= 1000, ]
lantana_1 <- lantana_1[lantana_1$eventDate != "", ]
sum(lantana_1$Start.date == "")
unique(is.na(lantana_1))

lantana_1 <- lantana_1[grepl("^NA", rownames(lantana_1)) == F, ] # Remove rows with rownames of NA.x
unique(is.na(lantana_1))
colnames(lantana_1) <- c('Start.date', 'y', 'x', 'Datum', 'Precision')
unique(lantana_1$Start.date)


lantana_1$year <- as.character(substr(lantana_1$Start.date, 1, 4))
lantana_1$month <- as.character(substr(lantana_1$Start.date, 6, 7))
lantana_1$day <- as.character(substr(lantana_1$Start.date, 9,10))
unique(lantana_1$day)
unique(lantana_1$month)
unique(lantana_1$year)
lantana_1 <- lantana_1[lantana_1$year <2026,]
unique(lantana_1$year)
lantana_1$Date <- paste0(lantana_1$year, "-", lantana_1$month, "-", lantana_1$day)

str(lantana_2)
lantana_2 <- lantana_2[,c('Start.date', 'Latitude', 'Longitude', 'Datum', 'Precision')]
lantana_2 <- lantana_2[lantana_2$Precision <= 1000, ]
unique(is.na(lantana_2))
lantana_2 <- lantana_2[grepl("^NA", rownames(lantana_2)) == F, ] # Remove rows with rownames of NA.x
unique(is.na(lantana_2))


lantana_2$year <- as.character(substr(lantana_2$Start.date, 7, 11))
lantana_2$month <- as.character(substr(lantana_2$Start.date, 4, 5))
lantana_2$day <- as.character(substr(lantana_2$Start.date, 1, 2))
unique(lantana_2$day)
unique(lantana_2$month)
unique(lantana_2$year)
lantana_2 <- lantana_2[lantana_2$year >1989, ]
lantana_2 <- lantana_2[lantana_2$year <2026, ]
colnames(lantana_2) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day')
lantana_2$Date <- paste0(lantana_2$year, "-", lantana_2$month, "-", lantana_2$day)
head(lantana_2); unique(is.na(lantana_2))



# 8.1.2 Vulpes vulpes ----
fox_1 <- read.csv('./00_Data/Pest_occurrence_records/Vulpes_vulpes_records-2026-01-13/Vulpes_vulpes_records-2026-01-13.csv', header = T)
fox_2 <- read.csv('./00_Data/Pest_occurrence_records/Vulpes_vulpes_WildNet_sighting_list_primary_fields_20260113_144548.csv', header = T)
sum(fox_1$eventDate == "") 
sum(fox_2$Start.date == "")

str(fox_1)
fox_1 <- fox_1[, c('eventDate', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'coordinateUncertaintyInMeters')]
fox_1 <- fox_1[fox_1$coordinateUncertaintyInMeters <= 1000, ]
sum(fox_1$Start.date == "")
unique(is.na(fox_1))

fox_1 <- fox_1[grepl("^NA", rownames(fox_1)) == F, ] # Remove rows with rownames of NA.x
unique(is.na(fox_1))
colnames(fox_1) <- c('Start.date', 'y', 'x', 'Datum', 'Precision')
unique(fox_1$Start.date)


fox_1$year <- as.character(substr(fox_1$Start.date, 1, 4))
fox_1$month <- as.character(substr(fox_1$Start.date, 6, 7))
fox_1$day <- as.character(substr(fox_1$Start.date, 9,10))
unique(fox_1$day)
unique(fox_1$month)
unique(fox_1$year)
fox_1 <- fox_1[fox_1$year <2026,]
fox_1$Date <- paste0(fox_1$year, "-", fox_1$month, "-", fox_1$day)



str(fox_2)
fox_2 <- fox_2[,c('Start.date', 'Latitude', 'Longitude', 'Datum', 'Precision')]
fox_2 <- fox_2[fox_2$Precision <= 1000, ]
unique(is.na(fox_2))

fox_2$year <- as.character(substr(fox_2$Start.date, 7, 11))
fox_2$month <- as.character(substr(fox_2$Start.date, 4, 5))
fox_2$day <- as.character(substr(fox_2$Start.date, 1, 2))
colnames(fox_2) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day')
unique(fox_2$day)
unique(fox_2$month)
unique(fox_2$year)
fox_2 <- fox_2[fox_2$year >1989, ]
fox_2 <- fox_2[fox_2$year <2026, ]
fox_2$Date <- paste0(fox_2$year, "-", fox_2$month, "-", fox_2$day)
head(fox_2); unique(is.na(fox_2))


# 8.1.3 Felis catus ----
cat_1 <- read.csv('./00_Data/Pest_occurrence_records/Felis_Catus_records-2026-01-13/Felis_Catus_records-2026-01-13.csv', header = T)
cat_2 <- read.csv('./00_Data/Pest_occurrence_records/Felis_catus_WildNet_sighting_list_primary_fields_20260113_144523.csv', header = T)
sum(cat_1$eventDate == "")  # Some records have no date
sum(cat_2$Start.date == "")

str(cat_1)
cat_1 <- cat_1[, c('eventDate', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'coordinateUncertaintyInMeters')]
cat_1 <- cat_1[cat_1$coordinateUncertaintyInMeters <= 1000, ]
cat_1 <- cat_1[cat_1$eventDate != "", ]
sum(cat_1$Start.date == "")
unique(is.na(cat_1))

cat_1 <- cat_1[grepl("^NA", rownames(cat_1)) == F, ] # Remove rows with rownames of NA.x
unique(is.na(cat_1))
colnames(cat_1) <- c('Start.date', 'y', 'x', 'Datum', 'Precision')
unique(cat_1$Start.date)


cat_1$year <- as.character(substr(cat_1$Start.date, 1, 4))
cat_1$month <- as.character(substr(cat_1$Start.date, 6, 7))
cat_1$day <- as.character(substr(cat_1$Start.date, 9,10))
unique(cat_1$day)
unique(cat_1$month)
unique(cat_1$year)
cat_1$Date <- paste0(cat_1$year, "-", cat_1$month, "-", cat_1$day)

str(cat_2)
cat_2 <- cat_2[,c('Start.date', 'Latitude', 'Longitude', 'Datum', 'Precision')]
cat_2 <- cat_2[cat_2$Precision <= 1000, ]
unique(is.na(cat_2))

cat_2$year <- as.character(substr(cat_2$Start.date, 7, 11))
cat_2$month <- as.character(substr(cat_2$Start.date, 4, 5))
cat_2$day <- as.character(substr(cat_2$Start.date, 1, 2))
unique(cat_2$day)
unique(cat_2$month)
unique(cat_2$year)
cat_2 <- cat_2[cat_2$year >1989, ]
colnames(cat_2) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day')
cat_2$Date <- paste0(cat_2$year, "-", cat_2$month, "-", cat_2$day)
head(cat_2); unique(is.na(cat_2))


# 8.1.4 Canis familiaris ----
dog_1 <- read.csv('./00_Data/Pest_occurrence_records/Canis_familiaris_records-2026-01-13/Canis_familiaris_records-2026-01-13.csv', header = T)
dog_2 <- read.csv('./00_Data/Pest_occurrence_records/Canis_familiaris_WildNet_sighting_list_primary_fields_20260113_144651.csv', header = T)
sum(dog_1$eventDate == "")  # Some records have no date
sum(dog_2$Start.date == "")

str(dog_1)
dog_1 <- dog_1[, c('eventDate', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'coordinateUncertaintyInMeters')]
dog_1 <- dog_1[dog_1$coordinateUncertaintyInMeters <= 1000, ]
dog_1 <- dog_1[dog_1$eventDate != "", ]
sum(dog_1$Start.date == "")
unique(is.na(dog_1))

dog_1 <- dog_1[grepl("^NA", rownames(dog_1)) == F, ] # Remove rows with rownames of NA.x
unique(is.na(dog_1))
colnames(dog_1) <- c('Start.date', 'y', 'x', 'Datum', 'Precision')
unique(dog_1$Start.date)


dog_1$year <- as.character(substr(dog_1$Start.date, 1, 4))
dog_1$month <- as.character(substr(dog_1$Start.date, 6, 7))
dog_1$day <- as.character(substr(dog_1$Start.date, 9,10))
unique(dog_1$day)
unique(dog_1$month)
unique(dog_1$year)
dog_1 <- dog_1[dog_1$year <2026,]
unique(dog_1$year)
dog_1$Date <- paste0(dog_1$year, "-", dog_1$month, "-", dog_1$day)

str(dog_2)
dog_2 <- dog_2[,c('Start.date', 'Latitude', 'Longitude', 'Datum', 'Precision')]
dog_2 <- dog_2[dog_2$Precision <= 1000, ]
unique(is.na(dog_2))

dog_2$year <- as.character(substr(dog_2$Start.date, 7, 11))
dog_2$month <- as.character(substr(dog_2$Start.date, 4, 5))
dog_2$day <- as.character(substr(dog_2$Start.date, 1, 2))
unique(dog_2$day)
unique(dog_2$month)
unique(dog_2$year)
dog_2 <- dog_2[dog_2$year >1989, ]
dog_2 <- dog_2[dog_2$year <2026, ]
colnames(dog_2) <- c('Start.date', 'y', 'x', 'Datum', 'Precision', 'year', 'month', 'day')
dog_2$Date <- paste0(dog_2$year, "-", dog_2$month, "-", dog_2$day)
head(dog_2); unique(is.na(dog_2))




# 8.2 Convert to spatial data ----
lantana1 <- vect(st_as_sf(lantana_1, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
plet(lantana1)
lantana1$x <- crds(lantana1)[,1]
lantana1$y <- crds(lantana1)[,2]


lantana2 <- vect(st_as_sf(lantana_2, coords = c('x', 'y'), crs = 'EPSG:7844')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
plet(c(lantana2, lantana1))
lantana2$x <- crds(lantana2)[,1]
lantana2$y <- crds(lantana2)[,2]


cat1 <- vect(st_as_sf(cat_1, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
cat1$x <- crds(cat1)[,1]
cat1$y <- crds(cat1)[,2]

cat2 <- vect(st_as_sf(cat_2, coords = c('x', 'y'), crs = 'EPSG:7844')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
cat2$x <- crds(cat2)[,1]
cat2$y <- crds(cat2)[,2]

cat3 <- vect(st_as_sf(pest_WQ_cat, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
cat3$x <- crds(cat3)[,1]
cat3$y <- crds(cat3)[,2]


fox1 <- vect(st_as_sf(fox_1, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
fox1$x <- crds(fox1)[,1]
fox1$y <- crds(fox1)[,2]

fox2 <- vect(st_as_sf(fox_2, coords = c('x', 'y'), crs = 'EPSG:7844')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
fox2$x <- crds(fox2)[,1]
fox2$y <- crds(fox2)[,2]

fox3 <- vect(st_as_sf(pest_WQ_fox, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
fox3$x <- crds(fox3)[,1]
fox3$y <- crds(fox3)[,2]


dog1 <- vect(st_as_sf(dog_1, coords = c('x', 'y'), crs = 'EPSG:4326')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
dog1$x <- crds(dog1)[,1]
dog1$y <- crds(dog1)[,2]

dog2 <- vect(st_as_sf(dog_2, coords = c('x', 'y'), crs = 'EPSG:7844')) %>% 
  project('EPSG:3577') %>% 
  crop(e)
dog2$x <- crds(dog2)[,1]
dog2$y <- crds(dog2)[,2]




# 8.3 Remove duplicated records and combine into one dataset ----
# Duplicate records are those which have same date and location but need to look at both dataframes
lantana_2_dedup <- lantana2[!(lantana2$Date %in% lantana1$Date) & !(lantana2$y %in% lantana1$y) & !(lantana2$x %in% lantana1$x), ]
dim(lantana2); dim(lantana_2_dedup); dim(lantana2)-(dim(lantana_2_dedup)) # 259 duplicate records removed.
lantana <- rbind(lantana1, lantana_2_dedup)

fox_2_dedup <- fox2[!(fox2$Date %in% fox1$Date) & !(fox2$y %in% fox1$y) & !(fox2$x %in% fox1$x), ]
dim(fox2); dim(fox_2_dedup); dim(fox2)-dim(fox_2_dedup) # 24 duplicate records
fox <- rbind(fox1, fox_2_dedup, fox3)

cat_2_dedup <- cat2[!(cat2$Date %in% cat1$Date) & !(cat2$y %in% cat1$y) & !(cat2$x %in% cat1$x),]
dim(cat2); dim(cat_2_dedup); dim(cat2)-dim(cat_2_dedup) # 17 duplicate records
cat <- rbind(cat1, cat_2_dedup, cat3)

dog_2_dedup <- dog2[!(dog2$Date %in% dog1$Date) & !(dog2$y %in% dog1$y) & !(dog2$x %in% dog1$x),]
dim(dog2); dim(dog_2_dedup); dim(dog2)-dim(dog_2_dedup) # 35 duplicate records
dog <- rbind(dog1, dog_2_dedup)



# 8.4 Calculate abundance of pest species in population buffers and along connectivity corridors ---- 
BTRW_pop_buf; plot(BTRW_pop_buf)
BTRW_connectivity_buf; plot(BTRW_connectivity_buf)

lantana_overlap <- vect(st_intersection(st_as_sf(lantana), st_as_sf(BTRW_pop_buf)))
lantana_overlap$pop_id_buf <- paste0(lantana_overlap$pop_id, " ", lantana_overlap$dist)
head(lantana_overlap)
lantana_count <- count(lantana_overlap, pop_id_buf)
unique(lantana_count$n)
lantana_count$pop_id <- as.character(sub(" .*", "", lantana_count$pop_id_buf))
lantana_count$dist <- as.character(substr(lantana_count$pop_id_buf, nchar(paste0(lantana_count$pop_id, " "))+1, nchar(lantana_count$pop_id_buf)))
head(lantana_count)

lantana_cor <- vect(st_intersection(st_as_sf(lantana), st_as_sf(BTRW_connectivity_buf)))
lantana_cor$pop_id_buf <- paste0(lantana_cor$pop_id, " ", lantana_cor$dist)
lantana_cor_count <- count(lantana_cor, pop_id_buf)
lantana_cor_count$pop_id <- as.character(sub(" .*", "", lantana_cor_count$pop_id_buf))
lantana_cor_count$dist <- as.character(substr(lantana_cor_count$pop_id_buf, nchar(paste0(lantana_cor_count$pop_id, " "))+1, nchar(lantana_cor_count$pop_id_buf)))
head(lantana_cor_count)


cat_overlap <- vect(st_intersection(st_as_sf(cat), st_as_sf(BTRW_pop_buf)))
cat_overlap$pop_id_buf <- paste0(cat_overlap$pop_id, " ", cat_overlap$dist)
cat_overlap <- count(cat_overlap, pop_id_buf)
cat_overlap$pop_id <- as.character(sub(" .*", "", cat_overlap$pop_id_buf))
cat_overlap$dist <- as.character(substr(cat_overlap$pop_id_buf, nchar(paste0(cat_overlap$pop_id, " "))+1, nchar(cat_overlap$pop_id_buf)))
head(cat_overlap)

cat_cor <- vect(st_intersection(st_as_sf(cat), st_as_sf(BTRW_connectivity_buf)))
cat_cor$pop_id_buf <- paste0(cat_cor$pop_id, " ", cat_cor$dist)
cat_cor <- count(cat_cor, pop_id_buf)
cat_cor$pop_id <- as.character(sub(" .*", "", cat_cor$pop_id_buf))
cat_cor$dist <- as.character(substr(cat_cor$pop_id_buf, nchar(paste0(cat_cor$pop_id, " "))+1, nchar(cat_cor$pop_id_buf)))
head(cat_cor)


fox_overlap <- vect(st_intersection(st_as_sf(fox), st_as_sf(BTRW_pop_buf)))
fox_overlap$pop_id_buf <- paste0(fox_overlap$pop_id, " ", fox_overlap$dist)
fox_overlap <- count(fox_overlap, pop_id_buf)
fox_overlap$pop_id <- as.character(sub(" .*", "", fox_overlap$pop_id_buf))
fox_overlap$dist <- as.character(substr(fox_overlap$pop_id_buf, nchar(paste0(fox_overlap$pop_id, " "))+1, nchar(fox_overlap$pop_id_buf)))
head(fox_overlap)

fox_cor <- vect(st_intersection(st_as_sf(fox), st_as_sf(BTRW_connectivity_buf)))
fox_cor$pop_id_buf <- paste0(fox_cor$pop_id, " ", fox_cor$dist)
fox_cor <- count(fox_cor, pop_id_buf)
fox_cor$pop_id <- as.character(sub(" .*", "", fox_cor$pop_id_buf))
fox_cor$dist <- as.character(substr(fox_cor$pop_id_buf, nchar(paste0(fox_cor$pop_id, " "))+1, nchar(fox_cor$pop_id_buf)))
head(fox_cor)


dog_overlap <- vect(st_intersection(st_as_sf(dog), st_as_sf(BTRW_pop_buf)))
dog_overlap$pop_id_buf <- paste0(dog_overlap$pop_id, " ", dog_overlap$dist)
dog_overlap <- count(dog_overlap, pop_id_buf)
dog_overlap$pop_id <- as.character(sub(" .*", "", dog_overlap$pop_id_buf))
dog_overlap$dist <- as.character(substr(dog_overlap$pop_id_buf, nchar(paste0(dog_overlap$pop_id, " "))+ 1, nchar(dog_overlap$pop_id_buf)))
head(dog_overlap)

dog_cor <- vect(st_intersection(st_as_sf(dog), st_as_sf(BTRW_connectivity_buf)))
dog_cor$pop_id_buf <- paste0(dog_cor$pop_id, " ", dog_cor$dist)
dog_cor <- count(dog_cor, pop_id_buf)
dog_cor$pop_id <- as.character(sub(" .*", "", dog_cor$pop_id_buf))
dog_cor$dist <- as.character(substr(dog_cor$pop_id_buf, nchar(paste0(dog_cor$pop_id, " "))+1, nchar(dog_cor$pop_id_buf)))
head(dog_cor)


# 8.5 Add pest counts to population buffer and corridor spatial data sets ----
BTRW_pop_buf$lantana_count <- ifelse(BTRW_pop_buf$pop_id %in% lantana_count$pop_id & BTRW_pop_buf$dist %in% lantana_count$dist, lantana_count$n, NA)
BTRW_pop_buf
unique(BTRW_pop_buf$lantana_count)
unique(BTRW_pop_buf$lantana_count) %in% unique(lantana_count$n) # The only one that is not in lantana_count was NA, so this has worked correctly

BTRW_connectivity_buf$lantana_count <- ifelse(BTRW_pop_buf$pop_id %in% lantana_cor_count$pop_id & BTRW_pop_buf$dist %in% lantana_cor_count$dist, lantana_cor_count$n, NA)
unique(BTRW_connectivity_buf$lantana_count)
unique(BTRW_connectivity_buf$lantana_count) %in% unique(lantana_cor_count$n)




BTRW_pop_buf$cat_count <- ifelse(BTRW_pop_buf$pop_id %in% cat_overlap$pop_id & BTRW_pop_buf$dist %in% cat_overlap$dist, cat_overlap$n, NA)
unique(BTRW_pop_buf$cat_count)
unique(BTRW_pop_buf$cat_count) %in% unique(cat_overlap$n)


BTRW_connectivity_buf$cat_count <- ifelse(BTRW_connectivity_buf$pop_id %in% cat_cor$pop_id & BTRW_connectivity_buf$dist %in% cat_cor$dist, cat_cor$n, NA)
unique(BTRW_connectivity_buf$cat_count)
unique(BTRW_connectivity_buf$cat_count) %in% unique(cat_cor$n)




BTRW_pop_buf$fox_count <- ifelse(BTRW_pop_buf$pop_id %in% fox_overlap$pop_id & BTRW_pop_buf$dist %in% fox_overlap$dist, fox_overlap$n, NA)
unique(BTRW_pop_buf$fox_count)
unique(BTRW_pop_buf$fox_count) %in% unique(fox_overlap$n)


BTRW_connectivity_buf$fox_count <- ifelse(BTRW_connectivity_buf$pop_id %in% fox_cor$pop_id & BTRW_connectivity_buf$dist %in% fox_cor$dist, fox_cor$n, NA)
unique(BTRW_connectivity_buf$fox_count)
unique(BTRW_connectivity_buf$fox_count) %in% unique(fox_cor$n)



BTRW_pop_buf$dog_count <- ifelse(BTRW_pop_buf$pop_id %in% dog_overlap$pop_id & BTRW_pop_buf$dist %in% dog_overlap$dist, dog_overlap$n, NA)
unique(BTRW_pop_buf$dog_count)
unique(BTRW_pop_buf$dog_count) %in% unique(dog_overlap$n)


BTRW_connectivity_buf$dog_count <- ifelse(BTRW_connectivity_buf$pop_id %in% dog_cor$pop_id & BTRW_connectivity_buf$dist %in% dog_cor$dist, dog_cor$n, NA)
unique(BTRW_connectivity_buf$dog_count)
unique(BTRW_connectivity_buf$dog_count) %in% unique(dog_cor$n)



# 8.6 Determine spatial sampling bias ----
head(lantana)[1,]
lantana # point data for species occurrences
lantana$Precision <- as.numeric(lantana$Precision)
lantana$year <- as.numeric(lantana$year)
lantana$month <- as.numeric(lantana$month)
lantana$day <- as.numeric(lantana$day)
lantana_df <- as.data.frame(lantana)
lantana_df$species <- 'Lantana camara'
rtemp <- rast(ext(e), res = 1000, crs = 'EPSG:3577') # Study area raster
lantana_rast <- rasterize(lantana, rtemp, fun = "count")
lantana_rast[is.na(lantana_rast)] <-  0 # Replace NAs, do not trim


fox # point data for species occurrences
fox$Precision <- as.numeric(fox$Precision)
fox$year <- as.numeric(fox$year)
fox$month <- as.numeric(fox$month)
fox$day <- as.numeric(fox$day)
fox_df <- as.data.frame(fox)
fox_df$species <- "Vulpes vulpes"
fox_rast <- rasterize(fox, rtemp, fun = 'count')
fox_rast[is.na(fox_rast)] <-  0 # Replace NAs, do not trim


cat # point data for species occurrences
cat$Precision <- as.numeric(cat$Precision)
cat$year <- as.numeric(cat$year)
cat$month <- as.numeric(cat$month)
cat$day <- as.numeric(cat$day)
cat_df <- as.data.frame(cat)
cat_df$species <- "Felis catus"
cat_rast <- rasterize(cat, rtemp, fun = 'count')
cat_rast[is.na(cat_rast)] <-  0 # Replace NAs, do not trim

dog # point data for species occurrences
dog$Precision <- as.numeric(dog$Precision)
dog$year <- as.numeric(dog$year)
dog$month <- as.numeric(dog$month)
dog$day <- as.numeric(dog$day)
dog_df <- as.data.frame(dog)
dog_df$species <- 'Canis familiaris' 
dog_rast <- rasterize(dog, rtemp, fun = 'count')
dog_rast[is.na(dog_rast)] <-  0 # Replace NAs, do no trim

# The following code is based on underlying code for the sampbias package. Functions from the sampbias were not able to be run so code was copied from GitHub and modified to work for the data. Code was downloaded from https://github.com/azizka/sampbias.git on the 30th January 2026


# Create gaz rasters 
road <- vect('./00_Data/Environmental_data/Roads_and_tracks/Queensland_roads_and_tracks.shp') %>% 
  project("EPSG:3577") %>% 
  crop(e)

small_build <- vect('./00_Data/Environmental_data/Building_points/Building_points.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

large_build <- vect('./00_Data/Environmental_data/Building_areas/Building_areas.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)

pop_centres <- vect('./00_Data/Environmental_data/Population_centres/Population_centres.shp') %>% 
  project("EPSG:3577") %>% 
  crop(e)


geo_features <- list(roads = road,
                     small_buildings = small_build,
                     large_buildings = large_build,
                     population_centres = pop_centres)

dis.ras <- geo_features


# Define all the parameters
mcmc_rescale_distances <- 1000
mcmc_iterations <- 10000
mcmc_burnin <- 2000
mcmc_outfile <- NULL  # or specify a file path like "mcmc_output.txt"
prior_q <- c(1, 0.01)
prior_w <- c(1, 1)
run_null_model <- FALSE
use_hyperprior <- TRUE
verbose <- TRUE

# 8.6.1 Lantana spatial bias ----
lantana_dis.ras <- dis_rast(geo_features, lantana_rast)
lantana_dis.vec <- lapply(lantana_dis.ras, terra::values)
lantana_dis.vec <- as.data.frame(do.call(cbind, lantana_dis.vec))
names(lantana_dis.vec) <- names(lantana_dis.ras)

lantana_dis.vec <- data.frame(cell_id = seq_len(nrow(lantana_dis.vec)),
                      record_count = values(lantana_rast)[, 1],
                      lantana_dis.vec)
lantana_dis.vec <- lantana_dis.vec[complete.cases(lantana_dis.vec), ]
rec_count <- c(sum(lantana_dis.vec$record_count == 0),
               sum(lantana_dis.vec$record_count > 0))


lantana_out <- sampbias:::.RunSampBias( # To find this internal function have to use ::: to access
  x = lantana_dis.vec,
  rescale_distances = mcmc_rescale_distances,
  iterations = mcmc_iterations,
  burnin = mcmc_burnin,
  prior_q = prior_q,
  prior_w = prior_w,
  outfile = mcmc_outfile,
  run_null_model = run_null_model,
  use_hyperprior = use_hyperprior,
  verbose = verbose
)
head(lantana_out)

lantana_bias_results <- list(summa = list(total_occ = nrow(lantana_df),
                         total_sp = length(unique(lantana_df$species)),
                         extent = ext(rtemp),
                         res = res(rtemp),
                         restrict_sample = NULL,
                         rescale_distances = mcmc_rescale_distances,
                         data_availability = rec_count),
            occurrences = lantana_rast,
            bias_estimate = lantana_out,
            distance_rasters = lantana_dis.ras)

class(lantana_bias_results) <- append("sampbias", class(lantana_bias_results))


# Project bias onto spatial grid
lantana_ras <- lantana_bias_results$distance_rasters
for(i in seq_along(lantana_ras)) {
  lantana_ras[[i]] <- lantana_ras[[i]] / lantana_bias_results$summa$rescale_distances
} # Extract distance rasters and rescale

# Get mean posterior weights and sort according to importance
mean_w_lantana <- colMeans(lantana_bias_results$bias_estimate)[-c(1:4, ncol(lantana_bias_results$bias_estimate))] %>% sort() %>% rev()

# Sort as mean_w
ord_lantana <- match(gsub("w_", "", names(mean_w_lantana)), names(lantana_ras))
lantana_ras <- lantana_ras[ord_lantana]

# use factors if provided to select certain biasing effects for projection
lantana_factors <- names(lantana_bias_results$distance_rasters)
mean_w_lantana <- mean_w_lantana[gsub("w_", "", names(mean_w_lantana)) %in% lantana_factors]
lantana_ras <- lantana_ras[names(lantana_ras) %in% lantana_factors]

# Calculate the values for each raster cell
lantana_lambdas <- list()

# Get the mean estimate for the mean rate
lantana_mean_q <- colMeans(lantana_bias_results$bias_estimate)[4]

# Calculate the sampling rate when increasingly adding biasing factors
for (i in seq_along(mean_w_lantana)) {
  # Convert raster subset to matrix
  raster_subset <- rast(lantana_ras[1:i])
  X_matrix <- terra::values(raster_subset)
  
  lantana_lambdas[[i]] <- sampbias:::get_lambda_ij(
    q = lantana_mean_q,
    w = mean_w_lantana[1:i],
    X = X_matrix
  ) %>% 
    as.matrix()
}

# add a plot for the relative variation
lantana_perc <- round((lantana_lambdas[[length(lantana_lambdas)]] - lantana_mean_q)/ lantana_mean_q * 100, 2)
lantana_lambdas[[length(lantana_lambdas) + 1]] <- lantana_perc

# re-transfor to a spatial raster
lantana_out <- list()
lantana_temp <- lantana_bias_results$distance_rasters[[1]]
for (i in seq_along(lantana_lambdas)) {
 values(lantana_temp) <- lantana_lambdas[[i]][, 1]
 lantana_out[[i]] <- lantana_temp
}

lantana_out[[i+1]] <- lantana_rast
lantana_out <- rast(lantana_out)

# Define the names
lantana_nam <- vector()
for( i in seq_along(mean_w_lantana)) {
  lantana_nam <- c(lantana_nam, paste(gsub("w_", "", names(mean_w_lantana)[1:i]), collapse = "+"))
}

# Add the total percentage change and the occurrences 
lantana_nam <- c(lantana_nam, "Total_percentage", "occurrences")
names(lantana_out) <- lantana_nam
lantana_out

set.seed(42)
lantana_sampbias <- sampbias::map_bias(
  x = lantana_out,
  sealine = FALSE,  
  type = "sampling_rate"  
)

# Create plot of bias considering all geo_features
names(lantana_out)
lantana_spatbias <- lantana_out[["roads+small_buildings+large_buildings+population_centres"]]

plo_lantana <-  data.frame(geo_lon = crds(lantana_spatbias)[, 1], geo_lat = crds(lantana_spatbias)[, 2], sampling_bias = values(lantana_spatbias)[, 1]) %>%
  filter(!is.na(sampling_bias))


# Create plot
Aus <- Aus[Aus$STE_CODE21 == 3]
plo_lantana_rast <- terra::mask(rast(plo_lantana, crs = 'EPSG:3577'), Aus)
plot(plo_lantana_rast)
plo_lantana_masked <- as.data.frame(plo_lantana_rast, xy = T)


min(plo_lantana_masked$sampling_bias, na.rm = T)
max(plo_lantana_masked$sampling_bias, na.rm = T)

lantana_spatbias_p <- ggplot()+
  geom_raster(data = plo_lantana_masked, aes(x = x, y = y, fill = sampling_bias))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.6) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.3, "cm"), width = unit(0.7, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.box = 'vertical',
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm')
  )+
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "transparent",
    name = "Estimated sampling rate",
    limits = c(0, 0.2563),
    breaks = c(0, 0.256)) +
  theme(axis.title = element_blank()) +
  new_scale_fill() +
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))+
  labs(alpha = "")





# 8.6.2 Fox spatial bias ----
fox_dis.ras <- dis_rast(geo_features, fox_rast)
fox_dis.vec <- lapply(fox_dis.ras, terra::values)
fox_dis.vec <- as.data.frame(do.call(cbind, fox_dis.vec))
names(fox_dis.vec) <- names(fox_dis.ras)

fox_dis.vec <- data.frame(cell_id = seq_len(nrow(fox_dis.vec)),
                              record_count = values(fox_rast)[, 1],
                              fox_dis.vec)
fox_dis.vec <- fox_dis.vec[complete.cases(fox_dis.vec), ]
rec_count <- c(sum(fox_dis.vec$record_count == 0),
               sum(fox_dis.vec$record_count > 0))

set.seed(42)
fox_out <- sampbias:::.RunSampBias( # To find this internal function have to use ::: to access
  x = fox_dis.vec,
  rescale_distances = mcmc_rescale_distances,
  iterations = mcmc_iterations,
  burnin = mcmc_burnin,
  prior_q = prior_q,
  prior_w = prior_w,
  outfile = mcmc_outfile,
  run_null_model = run_null_model,
  use_hyperprior = use_hyperprior,
  verbose = verbose
)
head(fox_out)

fox_bias_results <- list(summa = list(total_occ = nrow(fox_df),
                                          total_sp = length(unique(fox_df$species)),
                                          extent = ext(rtemp),
                                          res = res(rtemp),
                                          restrict_sample = NULL,
                                          rescale_distances = mcmc_rescale_distances,
                                          data_availability = rec_count),
                             occurrences = fox_rast,
                             bias_estimate = fox_out,
                             distance_rasters = fox_dis.ras)

class(fox_bias_results) <- append("sampbias", class(fox_bias_results))


# Project bias onto spatial grid
fox_ras <- fox_bias_results$distance_rasters
for(i in seq_along(fox_ras)) {
  fox_ras[[i]] <- fox_ras[[i]] / fox_bias_results$summa$rescale_distances
} # Extract distance rasters and rescale

# Get mean posterior weights and sort according to importance
mean_w_fox <- colMeans(fox_bias_results$bias_estimate)[-c(1:4, ncol(fox_bias_results$bias_estimate))] %>% sort() %>% rev()

# Sort as mean_w
ord_fox <- match(gsub("w_", "", names(mean_w_fox)), names(fox_ras))
fox_ras <- fox_ras[ord_fox]

# use factors if provided to select certain biasing effects for projection
fox_factors <- names(fox_bias_results$distance_rasters)
mean_w_fox <- mean_w_fox[gsub("w_", "", names(mean_w_fox)) %in% fox_factors]
fox_ras <- fox_ras[names(fox_ras) %in% fox_factors]

# Calculate the values for each raster cell
fox_lambdas <- list()

# Get the mean estimate for the mean rate
fox_mean_q <- colMeans(fox_bias_results$bias_estimate)[4]

# Calculate the sampling rate when increasingly adding biasing factors
for (i in seq_along(mean_w_fox)) {
  # Convert raster subset to matrix
  raster_subset <- rast(fox_ras[1:i])
  X_matrix <- terra::values(raster_subset)
  
  fox_lambdas[[i]] <- sampbias:::get_lambda_ij(
    q = fox_mean_q,
    w = mean_w_fox[1:i],
    X = X_matrix
  ) %>% 
    as.matrix()
}

# add a plot for the relative variation
fox_perc <- round((fox_lambdas[[length(fox_lambdas)]] - fox_mean_q)/ fox_mean_q * 100, 2)
fox_lambdas[[length(fox_lambdas) + 1]] <- fox_perc

# re-transfor to a spatial raster
fox_out <- list()
fox_temp <- fox_bias_results$distance_rasters[[1]]
for (i in seq_along(fox_lambdas)) {
  values(fox_temp) <- fox_lambdas[[i]][, 1]
  fox_out[[i]] <- fox_temp
}

fox_out[[i+1]] <- fox_rast
fox_out <- rast(fox_out)

# Define the names
fox_nam <- vector()
for( i in seq_along(mean_w_fox)) {
  fox_nam <- c(fox_nam, paste(gsub("w_", "", names(mean_w_fox)[1:i]), collapse = "+"))
}

# Add the total percentage change and the occurrences 
fox_nam <- c(fox_nam, "Total_percentage", "occurrences")
names(fox_out) <- fox_nam
fox_out


fox_sampbias <- sampbias::map_bias(
  x = fox_out,
  sealine = FALSE,  
  type = "sampling_rate"  
)

# Create plot of bias considering all geo_features
names(fox_out)
fox_spatbias <- fox_out[["roads+large_buildings+population_centres+small_buildings"]]

plo_fox <-  data.frame(geo_lon = crds(fox_spatbias)[, 1], geo_lat = crds(fox_spatbias)[, 2], sampling_bias = values(fox_spatbias)[, 1]) %>%
  filter(!is.na(sampling_bias))


# Create plot
plo_fox_rast <- terra::mask(rast(plo_fox, crs = 'EPSG:3577'), Aus)
plot(plo_fox_rast)
plo_fox_masked <- as.data.frame(plo_fox_rast, xy = T, na.rm = F)
min(plo_fox_masked$sampling_bias, na.rm = T)
max(plo_fox_masked$sampling_bias, na.rm = T)


fox_spatbias_p <- ggplot()+
  geom_raster(data = plo_fox_masked, aes(x = x, y = y, fill = sampling_bias))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.box = 'vertical',
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm')
  )+
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "transparent",
    name = "Estimated sampling rate",
    limits = c(0.005, 0.029),
    breaks = c(0.005, 0.029)
  ) +
  theme(axis.title = element_blank()) +
  new_scale_fill() +
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))+
  labs(alpha = "")




# 8.6.3 Cat spatial bias ----
cat_dis.ras <- dis_rast(geo_features, cat_rast)
cat_dis.vec <- lapply(cat_dis.ras, terra::values)
cat_dis.vec <- as.data.frame(do.call(cbind, cat_dis.vec))
names(cat_dis.vec) <- names(cat_dis.ras)

cat_dis.vec <- data.frame(cell_id = seq_len(nrow(cat_dis.vec)),
                          record_count = values(cat_rast)[, 1],
                          cat_dis.vec)
cat_dis.vec <- cat_dis.vec[complete.cases(cat_dis.vec), ]
rec_count <- c(sum(cat_dis.vec$record_count == 0),
               sum(cat_dis.vec$record_count > 0))

set.seed(42)
cat_out <- sampbias:::.RunSampBias( # To find this internal function have to use ::: to access
  x = cat_dis.vec,
  rescale_distances = mcmc_rescale_distances,
  iterations = mcmc_iterations,
  burnin = mcmc_burnin,
  prior_q = prior_q,
  prior_w = prior_w,
  outfile = mcmc_outfile,
  run_null_model = run_null_model,
  use_hyperprior = use_hyperprior,
  verbose = verbose
)
head(cat_out)

cat_bias_results <- list(summa = list(total_occ = nrow(cat_df),
                                      total_sp = length(unique(cat_df$species)),
                                      extent = ext(rtemp),
                                      res = res(rtemp),
                                      restrict_sample = NULL,
                                      rescale_distances = mcmc_rescale_distances,
                                      data_availability = rec_count),
                         occurrences = cat_rast,
                         bias_estimate = cat_out,
                         distance_rasters = cat_dis.ras)

class(cat_bias_results) <- append("sampbias", class(cat_bias_results))


# Project bias onto spatial grid
cat_ras <- cat_bias_results$distance_rasters
for(i in seq_along(cat_ras)) {
  cat_ras[[i]] <- cat_ras[[i]] / cat_bias_results$summa$rescale_distances
} # Extract distance rasters and rescale

# Get mean posterior weights and sort according to importance
mean_w_cat <- colMeans(cat_bias_results$bias_estimate)[-c(1:4, ncol(cat_bias_results$bias_estimate))] %>% sort() %>% rev()

# Sort as mean_w
ord_cat <- match(gsub("w_", "", names(mean_w_cat)), names(cat_ras))
cat_ras <- cat_ras[ord_cat]

# use factors if provided to select certain biasing effects for projection
cat_factors <- names(cat_bias_results$distance_rasters)
mean_w_cat <- mean_w_cat[gsub("w_", "", names(mean_w_cat)) %in% cat_factors]
cat_ras <- cat_ras[names(cat_ras) %in% cat_factors]

# Calculate the values for each raster cell
cat_lambdas <- list()

# Get the mean estimate for the mean rate
cat_mean_q <- colMeans(cat_bias_results$bias_estimate)[4]

# Calculate the sampling rate when increasingly adding biasing factors
for (i in seq_along(mean_w_cat)) {
  # Convert raster subset to matrix
  raster_subset <- rast(cat_ras[1:i])
  X_matrix <- terra::values(raster_subset)
  
  cat_lambdas[[i]] <- sampbias:::get_lambda_ij(
    q = cat_mean_q,
    w = mean_w_cat[1:i],
    X = X_matrix
  ) %>% 
    as.matrix()
}

# add a plot for the relative variation
cat_perc <- round((cat_lambdas[[length(cat_lambdas)]] - cat_mean_q)/ cat_mean_q * 100, 2)
cat_lambdas[[length(cat_lambdas) + 1]] <- cat_perc

# re-transfor to a spatial raster
cat_out <- list()
cat_temp <- cat_bias_results$distance_rasters[[1]]
for (i in seq_along(cat_lambdas)) {
  values(cat_temp) <- cat_lambdas[[i]][, 1]
  cat_out[[i]] <- cat_temp
}

cat_out[[i+1]] <- cat_rast
cat_out <- rast(cat_out)

# Define the names
cat_nam <- vector()
for( i in seq_along(mean_w_cat)) {
  cat_nam <- c(cat_nam, paste(gsub("w_", "", names(mean_w_cat)[1:i]), collapse = "+"))
}

# Add the total percentage change and the occurrences 
cat_nam <- c(cat_nam, "Total_percentage", "occurrences")
names(cat_out) <- cat_nam
cat_out


cat_sampbias <- sampbias::map_bias(
  x = cat_out,
  sealine = FALSE, 
  type = "sampling_rate" 
)

# Create plot of bias considering all geo_features
names(cat_out)
cat_spatbias <- cat_out[["roads+large_buildings+population_centres+small_buildings"]]

plo_cat <-  data.frame(geo_lon = crds(cat_spatbias)[, 1], geo_lat = crds(cat_spatbias)[, 2], sampling_bias = values(cat_spatbias)[, 1]) %>%
  filter(!is.na(sampling_bias))


# Create plot
plo_cat_rast <- terra::mask(rast(plo_cat, crs = 'EPSG:3577'), Aus)
plot(plo_cat_rast)
plo_cat_masked <- as.data.frame(plo_cat_rast, xy = T, na.rm = F)


min(plo_cat_masked$sampling_bias, na.rm = T)
max(plo_cat_masked$sampling_bias, na.rm = T)
# low sampling effort relative to species occurrences 
cat_spatbias_p <- ggplot()+
  geom_raster(data = plo_cat_masked, aes(x = x, y = y, fill = sampling_bias))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = 'vertical',
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm')
  )+
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "transparent",
    name = "Estimated sampling rate",
    limits = c(0.0090, 0.00935),
    breaks = c(0.0090, 0.00935)
  ) +
  theme(axis.title = element_blank()) +
  new_scale_fill() +
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))+
  labs(alpha = "")



# 8.6.4 Dog spatial bias ----
dog_dis.ras <- dis_rast(geo_features, dog_rast)
dog_dis.vec <- lapply(dog_dis.ras, terra::values)
dog_dis.vec <- as.data.frame(do.call(cbind, dog_dis.vec))
names(dog_dis.vec) <- names(dog_dis.ras)

dog_dis.vec <- data.frame(cell_id = seq_len(nrow(dog_dis.vec)),
                          record_count = values(dog_rast)[, 1],
                          dog_dis.vec)
dog_dis.vec <- dog_dis.vec[complete.cases(dog_dis.vec), ]
rec_count <- c(sum(dog_dis.vec$record_count == 0),
               sum(dog_dis.vec$record_count > 0))

set.seed(42)
dog_out <- sampbias:::.RunSampBias( # To find this internal function have to use ::: to access
  x = dog_dis.vec,
  rescale_distances = mcmc_rescale_distances,
  iterations = mcmc_iterations,
  burnin = mcmc_burnin,
  prior_q = prior_q,
  prior_w = prior_w,
  outfile = mcmc_outfile,
  run_null_model = run_null_model,
  use_hyperprior = use_hyperprior,
  verbose = verbose
)
head(dog_out)

dog_bias_results <- list(summa = list(total_occ = nrow(dog_df),
                                      total_sp = length(unique(dog_df$species)),
                                      extent = ext(rtemp),
                                      res = res(rtemp),
                                      restrict_sample = NULL,
                                      rescale_distances = mcmc_rescale_distances,
                                      data_availability = rec_count),
                         occurrences = dog_rast,
                         bias_estimate = dog_out,
                         distance_rasters = dog_dis.ras)

class(dog_bias_results) <- append("sampbias", class(dog_bias_results))


# Project bias onto spatial grid
dog_ras <- dog_bias_results$distance_rasters
for(i in seq_along(dog_ras)) {
  dog_ras[[i]] <- dog_ras[[i]] / dog_bias_results$summa$rescale_distances
} # Extract distance rasters and rescale

# Get mean posterior weights and sort according to importance
mean_w_dog <- colMeans(dog_bias_results$bias_estimate)[-c(1:4, ncol(dog_bias_results$bias_estimate))] %>% sort() %>% rev()

# Sort as mean_w
ord_dog <- match(gsub("w_", "", names(mean_w_dog)), names(dog_ras))
dog_ras <- dog_ras[ord_dog]

# use factors if provided to select certain biasing effects for projection
dog_factors <- names(dog_bias_results$distance_rasters)
mean_w_dog <- mean_w_dog[gsub("w_", "", names(mean_w_dog)) %in% dog_factors]
dog_ras <- dog_ras[names(dog_ras) %in% dog_factors]

# Calculate the values for each raster cell
dog_lambdas <- list()

# Get the mean estimate for the mean rate
dog_mean_q <- colMeans(dog_bias_results$bias_estimate)[4]

# Calculate the sampling rate when increasingly adding biasing factors
for (i in seq_along(mean_w_dog)) {
  # Convert raster subset to matrix
  raster_subset <- rast(dog_ras[1:i])
  X_matrix <- terra::values(raster_subset)
  
  dog_lambdas[[i]] <- sampbias:::get_lambda_ij(
    q = dog_mean_q,
    w = mean_w_dog[1:i],
    X = X_matrix
  ) %>% 
    as.matrix()
}

# add a plot for the relative variation
dog_perc <- round((dog_lambdas[[length(dog_lambdas)]] - dog_mean_q)/ dog_mean_q * 100, 2)
dog_lambdas[[length(dog_lambdas) + 1]] <- dog_perc

# re-transfor to a spatial raster
dog_out <- list()
dog_temp <- dog_bias_results$distance_rasters[[1]]
for (i in seq_along(dog_lambdas)) {
  values(dog_temp) <- dog_lambdas[[i]][, 1]
  dog_out[[i]] <- dog_temp
}

dog_out[[i+1]] <- dog_rast
dog_out <- rast(dog_out)

# Define the names
dog_nam <- vector()
for( i in seq_along(mean_w_dog)) {
  dog_nam <- c(dog_nam, paste(gsub("w_", "", names(mean_w_dog)[1:i]), collapse = "+"))
}

# Add the total percentage change and the occurrences 
dog_nam <- c(dog_nam, "Total_percentage", "occurrences")
names(dog_out) <- dog_nam
dog_out


dog_sampbias <- sampbias::map_bias(
  x = dog_out,
  sealine = FALSE,  
  type = "sampling_rate" 
)

# Create plot of bias considering all geo_features
names(dog_out)
dog_spatbias <- dog_out[["roads+population_centres+small_buildings+large_buildings"]]

plo_dog <-  data.frame(geo_lon = crds(dog_spatbias)[, 1], geo_lat = crds(dog_spatbias)[, 2], sampling_bias = values(dog_spatbias)[, 1]) %>%
  filter(!is.na(sampling_bias))


# Create plot
plo_dog_rast <- terra::mask(rast(plo_dog, crs = 'EPSG:3577'), Aus)
plot(plo_dog_rast)
plo_dog_masked <- as.data.frame(plo_dog_rast, xy = T, na.rm = F)

min(plo_dog_masked$sampling_bias, na.rm = T)
max(plo_dog_masked$sampling_bias, na.rm = T)


dog_spatbias_p <- ggplot()+
  geom_raster(data = plo_dog_masked, aes(x = x, y = y, fill = sampling_bias))+
  theme_bw()+
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = 'vertical',
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm')
  )+
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "transparent",
    name = "Estimated sampling rate",
    limits = c(0.0102, 0.01043),
    breaks = c(0.0102, 0.01043)
  ) +
  theme(axis.title = element_blank()) +
  new_scale_fill() +
  geom_spatvector(data = Aus, fill = 'transparent')+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))+
  labs(alpha = "")

# 8.6.5 Extract spatial bias information for populations and corridors for each species  ----
lantana_pop <- extract(lantana_spatbias, BTRW_pop_buf_agg)
cat_pop <- extract(cat_spatbias, BTRW_pop_buf_agg)
fox_pop <- extract(fox_spatbias, BTRW_pop_buf_agg)
dog_pop <- extract(dog_spatbias, BTRW_pop_buf_agg)
BTRW_pop_buf_agg$lantana_spatbias <- lantana_pop$`roads+small_buildings+large_buildings+population_centres`
BTRW_pop_buf_agg$cat_spatbias <- cat_pop$`roads+small_buildings+large_buildings+population_centres`
BTRW_pop_buf_agg$fox_spatbias <- fox_pop$`roads+small_buildings+large_buildings+population_centres`
BTRW_pop_buf_agg$dog_spatbias <- dog_pop$`roads+small_buildings+population_centres+large_buildings`

lantana_corrid <- extract(lantana_spatbias, BTRW_connectivity_buf)
cat_corrid <- extract(cat_spatbias, BTRW_connectivity_buf)
fox_corrid <- extract(fox_spatbias, BTRW_connectivity_buf)
dog_corrid <- extract(dog_spatbias, BTRW_connectivity_buf)
BTRW_connectivity_buf$lantana_spatbias <- lantana_corrid$`roads+small_buildings+large_buildings+population_centres`
BTRW_connectivity_buf$cat_spatbias <- cat_corrid$`roads+small_buildings+large_buildings+population_centres`
BTRW_connectivity_buf$fox_spatbias <- fox_corrid$`roads+small_buildings+large_buildings+population_centres`
BTRW_connectivity_buf$dog_spatbias <- dog_corrid$`roads+small_buildings+population_centres+large_buildings`



# 8.7 Plot pest species counts for population buffers and connectivity corridors and the spatial bias layer ----

display.brewer.all()
brewer.pal(9, 'Blues')
pal3 <- c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")

brewer.pal(9, 'Greys')
pal4 <- c("#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000")


BTRW_pop_buf <- BTRW_pop_buf %>% 
  arrange(lantana_count) 

lantana_pop <- ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    plot.title = element_text(size = 34)
  )+
  geom_spatvector(data = BTRW_pop_buf, aes(fill = lantana_count, col = lantana_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,70), breaks = c(1,25,50,70)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,70), breaks = c(1,25,50,70)) +
  labs(title = bold("(a) ")~italic(Lantana~camara), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))




lantana_corridor <- ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    plot.title = element_text(size = 34)
  )+
  geom_spatvector(data = BTRW_connectivity_buf, aes(fill = lantana_count, col = lantana_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, breaks = c(1,25,50,70), limits = c(1,70)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, breaks = c(1,25,50,70), limits = c(1,70)) +
  labs(title = bold("(e) ")~italic(Lantana~camara), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))



BTRW_pop_buf <- BTRW_pop_buf %>% 
  arrange(cat_count) 
cat_pop <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title.position = "top",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.title = element_text(face = 'bold', size = 32),
    legend.text = element_text(size = 28),
    plot.background = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, 'cm'),
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    plot.title = element_text(size = 34)
  )+
  geom_spatvector(data = BTRW_pop_buf, aes(fill = cat_count, col = cat_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,21), breaks = c(1,7,14,21)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = 'Number of records', palette = pal3, limits = c(1,21), breaks = c(1,7,14,21)) +
  labs(title = bold("(b) ")~italic(Felis~catus), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
cat_pop



cat_corridor <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
         legend.title.position = "top",
         legend.key.height = unit(0.4, 'cm'),
         legend.key.width = unit(1.5, 'cm'),
         legend.title = element_text(face = 'bold', size = 32),
         legend.text = element_text(size = 28),
         plot.background = element_blank(),
         plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
         legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
         legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, 'cm'),
        legend.box.spacing = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        plot.title = element_text(size = 34))+
  geom_spatvector(data = BTRW_connectivity_buf, aes(fill = cat_count, col = cat_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,21), breaks = c(1,7,14,21)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = 'Number of records', palette = pal3, limits = c(1,21), breaks = c(1,7,14,21)) +
  labs(title = bold("(f) ")~italic(Felis~catus), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))



BTRW_pop_buf <- BTRW_pop_buf %>% 
  arrange(fox_count) 
fox_pop <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
         legend.title.position = "top",
         legend.key.height = unit(0.4, 'cm'),
         legend.key.width = unit(1.5, 'cm'),
         legend.title = element_text(face = 'bold', size = 32),
         legend.text = element_text(size = 28),
         plot.background = element_blank(),
         plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
         legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
         legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, 'cm'),
        legend.box.spacing = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        plot.title = element_text(size = 34))+
  geom_spatvector(data = BTRW_pop_buf, aes(fill = fox_count, col = fox_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,19), breaks = c(1,6,12,19)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = 'Number of records', palette = pal3, limits = c(1,19), breaks = c(1,6,12,19)) +
  labs(title = bold("(c) ")~italic(Vulpes~vulpes), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
fox_pop


fox_corridor <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title.position = "top",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(1.5, 'cm'),
        legend.title = element_text(face = 'bold', size = 32),
        legend.text = element_text(size = 28),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, 'cm'),
        legend.box.spacing = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        plot.title = element_text(size = 34))+
  geom_spatvector(data = BTRW_connectivity_buf, aes(fill = fox_count, col = fox_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,19), breaks = c(1,6,12,19)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = 'Number of records', palette = pal3, limits = c(1,19), breaks = c(1,6,12,19)) +
  labs(title = bold("(g) ")~italic(Vulpes~vulpes), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))




BTRW_pop_buf <- BTRW_pop_buf %>% 
  arrange(dog_count) 
dog_pop <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme( legend.position = "bottom",
         legend.direction = "horizontal",
         legend.box = "vertical",
         legend.title.position = "top",
         legend.key.height = unit(0.4, 'cm'),
         legend.key.width = unit(1.5, 'cm'),
         legend.title = element_text(face = 'bold', size = 32),
         legend.text = element_text(size = 28),
         plot.background = element_blank(),
         plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
         legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
         legend.margin = margin(0, 0, 0, 0),
         legend.spacing = unit(0, 'cm'),
         legend.box.spacing = unit(0, 'cm'),
         legend.spacing.y = unit(0, 'cm'),
         plot.title = element_text(size = 34))+
  geom_spatvector(data = BTRW_pop_buf, aes(fill = dog_count, col = dog_count))+
  scale_fill_continuous(na.value = "#D9D9D9", name = "Number of records", palette = pal3, limits = c(1,12), breaks =c(1,4,8,12)) +
  scale_colour_continuous(na.value = "#D9D9D9", name = 'Number of records', palette = pal3, limits = c(1,12), breaks =c(1,4,8,12)) +
  labs(title = bold("(d) ")~italic(Canis~lupus~familiaris), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))
dog_pop




dog_corridor <- 
  ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.9, "cm"), text_cex = 1.8) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(1.5, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.07, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_cowplot(font_size = 32) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title.position = "top",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(1.5, 'cm'),
        legend.title = element_text(face = 'bold', size = 32),
        legend.text = element_text(size = 28),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.box.margin = unit(c(0, 0, 0, 2), "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, 'cm'),
        legend.box.spacing = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        plot.title = element_text(size = 34))+
  geom_spatvector(data = BTRW_connectivity_buf, aes(fill = dog_count, col = dog_count))+
  scale_fill_continuous(name = "Number of records", palette = pal3, limits = c(1,12), breaks =c(1,4,8,12), na.value = "#D9D9D9") +
  scale_colour_continuous(name = 'Number of records', palette = pal3, limits = c(1,12), breaks =c(1,4,8,12), na.value = "#D9D9D9") +
  labs(title = bold("(h) ")~italic(Canis~lupus~familiaris), alpha = "")+
  geom_spatvector(data = BTRW_pops, col = 'black', lwd = 0.3, fill = NA, aes(alpha = 1))+
  scale_alpha_continuous(labels = expression(bold("BTRW population")), guide = guide_legend(override.aes = list(linewidth = 0.5)))


lantana_spat_p <- lantana_spatbias_p + labs(title = bold("(i) ")~italic(Lantana~camara)) + theme(plot.title = element_text(size = 36))
cat_spat_p <- cat_spatbias_p + labs(title = bold("(j) ")~italic(Felis~catus))+ theme(plot.title = element_text(size = 36))
fox_spat_p <- fox_spatbias_p + labs(title = bold("(k) ")~italic(Vulpes~vulpes))+ theme(plot.title = element_text(size = 36))
dog_spat_p <- dog_spatbias_p + labs(title = bold("(l) ")~italic(Canis~lupus~familiaris))+ theme(plot.title = element_text(size = 36))

lantana_leg <- get_legend(lantana_pop)
cat_leg <- get_legend(cat_pop)
fox_leg <- get_legend(fox_pop)
dog_leg <- get_legend(dog_pop)

samp_leg_lantana <- get_legend(lantana_spat_p)
samp_leg_cat <- get_legend(cat_spat_p)
samp_leg_fox <- get_legend(fox_spat_p)
samp_leg_dog <- get_legend(dog_spat_p)

pest_plot <- plot_grid(
  lantana_pop + theme(legend.position = "none"),
  cat_pop + theme(legend.position = "none"),
  fox_pop + theme(legend.position = "none"),
  dog_pop + theme(legend.position = "none"),
  lantana_corridor + theme(legend.position = "none"),
  cat_corridor + theme(legend.position = "none"),
  fox_corridor + theme(legend.position = "none"),
  dog_corridor + theme(legend.position = "none"),
  lantana_leg, cat_leg, fox_leg, dog_leg,
  lantana_spat_p + theme(legend.position = "none"),
  cat_spat_p + theme(legend.position = "none"),
  fox_spat_p + theme(legend.position = "none"),
  dog_spat_p + theme(legend.position = "none"),
  samp_leg_lantana, samp_leg_cat, samp_leg_fox, samp_leg_dog,
  align = "hv",
  axis = "tblr",
  nrow = 5,
  rel_heights = c(1, 1, 0.3, 1, 0.35)) +
  theme(plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"))

ggsave("./03_Results/Plots/Pest_management.png", width = 105, height = 99, dpi = 300, units = 'cm')




# Overall pest prevalence across populations
pest_prevalence <- BTRW_pop_buf %>% 
  distinct(pop_id, .keep_all = TRUE) %>%
  summarise(
    total_pops = n(),
    # Lantana
    n_pops_lantana = sum(lantana_count > 0, na.rm = TRUE),
    prop_pops_lantana = round(sum(lantana_count > 0, na.rm = TRUE) / n(), 2),
    mean_lantana = round(mean(lantana_count, na.rm = TRUE), 2),
    # Cats
    n_pops_cats = sum(cat_count > 0, na.rm = TRUE),
    prop_pops_cats = round(sum(cat_count > 0, na.rm = TRUE) / n(), 2),
    mean_cat = round(mean(cat_count, na.rm = TRUE), 2),
    # Foxes
    n_pops_foxes = sum(fox_count > 0, na.rm = TRUE),
    prop_pops_foxes = round(sum(fox_count > 0, na.rm = TRUE) / n(), 2),
    mean_fox = round(mean(fox_count, na.rm = TRUE), 2),
    # Dogs
    n_pops_dogs = sum(dog_count > 0, na.rm = TRUE),
    prop_pops_dogs = round(sum(dog_count > 0, na.rm = TRUE) / n(), 2),
    mean_dog = round(mean(dog_count, na.rm = TRUE), 2)
  )
as.list(pest_prevalence)

# Overall pest prevalance across connectivity paths
pest_cor_prevalence <- BTRW_connectivity_buf %>% 
  distinct(pop_id, .keep_all = TRUE) %>%
  summarise(
    total_pops = n(),
    # Lantana
    n_pops_lantana = sum(lantana_count > 0, na.rm = TRUE),
    prop_pops_lantana = round(sum(lantana_count > 0, na.rm = TRUE) / n(), 2),
    mean_lantana = round(mean(lantana_count, na.rm = TRUE), 2),
    # Cats
    n_pops_cats = sum(cat_count > 0, na.rm = TRUE),
    prop_pops_cats = round(sum(cat_count > 0, na.rm = TRUE) / n(), 2),
    mean_cat = round(mean(cat_count, na.rm = TRUE), 2),
    # Foxes
    n_pops_foxes = sum(fox_count > 0, na.rm = TRUE),
    prop_pops_foxes = round(sum(fox_count > 0, na.rm = TRUE) / n(), 2),
    mean_fox = round(mean(fox_count, na.rm = TRUE), 2),
    # Dogs
    n_pops_dogs = sum(dog_count > 0, na.rm = TRUE),
    prop_pops_dogs = round(sum(dog_count > 0, na.rm = TRUE) / n(), 2),
    mean_dog = round(mean(dog_count, na.rm = TRUE), 2)
  )
as.list(pest_cor_prevalence)

# Determine which populations are predator free
predator_free <- BTRW_pop_buf[BTRW_pop_buf$dist == "2-3km" &
                                is.na(BTRW_pop_buf$lantana_count) & 
                                is.na(BTRW_pop_buf$cat_count) & 
                                is.na(BTRW_pop_buf$fox_count) & 
                                is.na(BTRW_pop_buf$dog_count), ]

nrow(predator_free)


writeVector(BTRW_pop_buf, './03_Results/BTRW_population_buffer_information.gpkg', overwrite = T)
writeVector(BTRW_connectivity_buf, './03_Results/BTRW_connectivity_buffer_information.gpkg', overwrite = T)
save.image('./02_Workspaces/004_restoration_and_management.RData')
