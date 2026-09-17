# Written by Felicity Charles
# 11/12/2025
# Caveat emptor


## This script analyses patch isolation for prioritisation of Brush-tailed Rock Wallaby population management 

# R version 4.5.1
# 1. Load packages ----
library(terra) # terra_1.8-70
library(dplyr) # dplyr_1.1.4 
library(ggplot2) # 4.0.0
library(tidyterra) # 0.7.2
library(ggspatial) #1.1.10
library(cowplot) # 1.2.0
library(igraph)
library(RColorBrewer)
library(sf)

# 2. Read in the data ----
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

# Create presence point only information
# Need to convert data from lon/lat to meter based
e <- ext(1920000, 2060000, -3250000, -3075000)

BTRW_coords <- BTRW_pres[, c(4:5, 8)]
colnames(BTRW_coords) <- c("y", "x", 'year')
BTRW_coords$x <- as.numeric(BTRW_coords$x)
BTRW_coords$y <- as.numeric(BTRW_coords$y)
head(BTRW_coords); str(BTRW_coords)
BTRW_cds <- vect(BTRW_coords, geom = c("x", "y"), crs = "EPSG:4326") %>% 
  project('EPSG:3577')
BTRW_cds; plet(BTRW_cds) # Reduced set of points to 506 records, only reduced the number of records by 41 from 547 but three records were also not capture by the habitat suitability modelling provided.


places <- vect('./00_Data/Environmental_data/Place_names/Place_names_gazetteer.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)
places <- places[places$place_name =="Brisbane"| places$place_name =="Toowoomba"| places$place_name =="Esk" | places$place_name == "Boonah"]
places <- places[!duplicated(places$place_name),]

Aus <- vect('./00_Data/Australia_shapefile/STE_2021_AUST_GDA2020.shp') %>% 
  project("EPSG:3577") %>% 
  crop(e)








### FOCUSSING ON SMALLER AREA DUE TO CIRCUITSCAPE COMPUTATIONAL INTENSITY 
# Below shows a map of the habitat suitability data overlayed and the subset of this area that we are modelling
BTRW_hsm <- rast('./00_Data/Environmental_data/Outputs/BTRW_HSM/BTRW_HSM_cropped.asc')
e <- ext(1920000, 2060000, -3250000, -3075000)
plot(BTRW_hsm)
plot(e, add = T)
plot(BTRW_cds, add = T) 

BTRW_cds <- crop(BTRW_cds, e)
# From the literature we know that genetic differentiation between populations of brush-tailed rock wallaby observed over 3 km - Piggott 2006 https://doi.org/10.1111/j.1365-294X.2005.02783.x. Yellow-footed rock wallaby and black-footed rock wallaby also showed strong genetic differentiation between colonies 1-3 km apart - Hazlitt 2004 https://doi.org/10.1111/j.1365-294X.2004.02342.x
# Females may disperse over distances less than 400 m but males may disperse over greater distances, but average is about 400 m Piggott 2005 https://doi.org/10.1111/j.1365-294X.2005.02784.x
# Hazlitt investigated allele sharing in females and males over 100 m distance classes, with 600 m showing lower allele sharing than expected from random for females https://doi.org/10.1111/j.1365-294X.2004.02342.x
# Data is in EPSG:3577 which is measured in metres, so 3 km is 3000 m


# Create population polygons ---- 
# Add buffer around points based on normal dispersal distances of BTRW - gives us some idea of populations/subgroups within the landscape
BTRW_buf <- buffer(BTRW_cds, 600) # Based on dispersal distance of BTRW
plot(BTRW_buf); BTRW_buf
BTRW_inter <- relate(BTRW_buf, BTRW_buf, 'intersects') # Get intersection information, where do individual points share common space
BTRW_adj <- graph_from_adjacency_matrix(BTRW_inter, mode = 'undirected') # Create adjacency matrix to draw connections between individual points (create clusters)
BTRW_buf$pop_id <- components(BTRW_adj)$membership # Find all the clusters and assign a population ID 
BTRW_intersected <- aggregate(BTRW_buf, by = 'pop_id') # Group populations and remove dissolve the buffer to create a single polygon for a population 
plot(BTRW_intersected); BTRW_intersected
writeVector(BTRW_intersected, './03_Results/BTRW_pops.gpkg')

# Determine degree of isolation ----
BTRW_dist <- as.matrix(distance(BTRW_intersected, pairs = F)) # Determine the minimum distance from the edge of one population to the next nearest population

# Calculate distance to nearest neighbour
BTRW_intersected$nearest_ID <- NA
BTRW_intersected$nearest_pop_dist <- NA

for(i in 1:nrow(BTRW_intersected)){
  dists <- BTRW_dist[i, ]
  dists[i] <- Inf # Overwrite value of self distance of 0.00 as infinite
  BTRW_intersected$nearest_ID[i] <- which.min(dists)
  BTRW_intersected$nearest_pop_dist[i] <- min(dists)
}
BTRW_intersected

# Sort by nearest_pop_dist
BTRW_intersected <-  BTRW_intersected[order(BTRW_intersected$nearest_pop_dist, decreasing = T), ]
BTRW_intersected

# Based on populations being genetically distinct over distances greater than 3 km, lets remove populations that are closer than 1 km
BTRW_nonisolated_pops <- BTRW_intersected[BTRW_intersected$nearest_pop_dist <1000, ]
BTRW_isolated_pops <- BTRW_intersected[BTRW_intersected$nearest_pop_dist >1000, ]

unique(BTRW_isolated_pops$nearest_pop_dist)

# Add ranking for degree of isolation to provide information on the proximity of nearby populations
# 1 = 1-2 km
# 2 = 2-3 km
# 3 = 3-4 km
# 4 = 4-5 km
# 5 = > 5 km

BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist <1000, 0, NA)
BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist >1000 & BTRW_intersected$nearest_pop_dist <2000, 1,  BTRW_intersected$isolation_rank)
BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist > 1999 & BTRW_intersected$nearest_pop_dist <3000, 2, BTRW_intersected$isolation_rank)
BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist > 2999 & BTRW_intersected$nearest_pop_dist <4000, 3, BTRW_intersected$isolation_rank)
BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist >3999 & BTRW_intersected$nearest_pop_dist <5000, 4, BTRW_intersected$isolation_rank)
BTRW_intersected$isolation_rank <- ifelse(BTRW_intersected$nearest_pop_dist >= 5000, 5, BTRW_intersected$isolation_rank)
table(BTRW_intersected$isolation_rank)

plot(BTRW_isolated_pops, col = BTRW_isolated_pops$isolation_rank)

BTRW_intersected_buf <- buffer(BTRW_intersected, 300)
brewer.pal(7, 'RdBu')
pal <- c("#2166AC", "#67A9CF","#D1E5F0" , "#FDDBC7", "#EF8A62", "#B2182B")

# Create a spatial map
ggplot() +
  geom_spatvector(data = Aus, fill = NA) +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  geom_spatvector(data = BTRW_intersected_buf, aes(fill = isolation_rank)) +
  scale_fill_continuous(palette = pal) +
  labs(fill = "Population isolation \nranking", col = "Population isolation \nranking") +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 18),
        legend.text = element_text(size = 14),
        plot.background = element_blank())+
  theme_bw() +
  theme_cowplot(font_size = 17)+
  labs(x = "", y = "")
ggsave("./03_Results/Plots/Population_isolation/Rank.png", width = 20, height = 16, dpi = 300, units = 'cm')

geo <- ggplot() +
  geom_spatvector(data = Aus, fill = NA) +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  geom_spatvector(data = BTRW_intersected_buf, aes(fill = isolation_rank), col = 'black', lwd = 0.1) +
  scale_fill_continuous(palette = pal, labels = c("0-1", "1-2", "2-3", "3-4", "4-5", ">5")) +
  labs(fill = "Geographical isolation (km)", col = "Geographical isolation (km)") +
  annotation_scale(location = 'bl', pad_y = unit(0.1, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.03, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme_cowplot(font_size = 17) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        plot.background = element_blank(),
        plot.margin = margin(t = 0.1, r = 0.2, b = 0.1, l = 0.2, unit = "cm")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 1)) +
  labs(x = "", y = "", title = "(a)")
ggsave("./03_Results/Plots/Population_isolation/Geographic.png", width = 20, height = 16, dpi = 300, units = 'cm')

save.image('./02_Workspaces/003_population_isolation.RData')


# Reproductive viability ----
# Get some information on individual records within these populations, table of population ID and years of occurrences, show whether new individuals may be being recruited into the population or if these are populations lost from the landscape
# Create buffers around populations
BTRW_buf # Gives information of every individual
BTRW_intersected # Gives information for every population
BTRW_isolated_pops
BTRW_nonisolated_pops$isolation_rank <- 0
BTRW_populations <- rbind(BTRW_isolated_pops, BTRW_nonisolated_pops) # Gives information on isolation for all populations
head(BTRW_pres)

# Create a data frame containing information on the individual and year from BTRW_pres and information from BTRW_populations on the population ID, nearest population, and isolation rank
BTRW_ind_pop <- st_intersection(st_as_sf(BTRW_cds), st_as_sf(BTRW_populations))

# Add column for decadal timeframe for easier visualisation
BTRW_ind_pop$decade <- ifelse(BTRW_ind_pop$year <=1999, "1990s", NA)
BTRW_ind_pop$decade <- ifelse(BTRW_ind_pop$year >1999 & BTRW_ind_pop$year <=2009, "2000s", BTRW_ind_pop$decade)
BTRW_ind_pop$decade <- ifelse(BTRW_ind_pop$year >2009 & BTRW_ind_pop$year <=2019, "2010s", BTRW_ind_pop$decade)
BTRW_ind_pop$decade <- ifelse(is.na(BTRW_ind_pop$decade), "2020s", BTRW_ind_pop$decade)
unique(BTRW_ind_pop$decade)

table(BTRW_ind_pop$decade, BTRW_ind_pop$pop_id)
table(BTRW_ind_pop$decade)

# Add a column on population persistence - are there records for the population in past 10 years?
BTRW_ind_pop$persistence <- ifelse(BTRW_ind_pop$year >=2014, 1, 0)
BTRW_ind_pop$ind <- 1
# Add information to isolated pops as to whether the population is persistent in the landscape in recent years
persistence <- aggregate(BTRW_ind_pop[, "persistence"],
                         by = list(pop_id = BTRW_ind_pop$pop_id), 
                         FUN = max)
BTRW_populations$persistence <- as.factor(persistence$persistence)
table(BTRW_populations$persistence)

BTRW_populations_buf <- buffer(BTRW_populations, 300)

tempo <- ggplot() +
  geom_spatvector(data = Aus, fill = NA) +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1) +
  geom_spatvector(data = BTRW_populations_buf, aes(fill = persistence), col = 'black', lwd = 0.1) +
  scale_fill_manual(values = c("red", "blue"), labels = c("No", "Yes")) +
  labs(fill = "Records since 2014") +
  annotation_scale(location = 'bl', pad_y = unit(0.1, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.03, 'cm'), style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme_cowplot(font_size = 17) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        plot.background = element_blank(),
        plot.margin = margin(t = 0.1, r = 0.2, b = 0.1, l = 0.2, unit = "cm")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1)) +
  labs(x = "", y = "", title = "(b)")
ggsave("./03_Results/Plots/Population_isolation/Persistence.png", width = 20, height = 16, dpi = 300, units = 'cm')



plot_grid(geo, tempo, nrow = 1)
ggsave("./03_Results/Plots/Population_isolation/Isolation_v2.png", width = 20, height = 16, dpi = 300, units = 'cm')
# Could evaluate population isolation in a more sophisticated manner by incorporating the temporal aspect but whether this is useful being a cryptic species and we don't know have information on absences, this may just be useful to provide some information on which populations to check in on
