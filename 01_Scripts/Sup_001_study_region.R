# Written by Felicity Charles
# 06/01/2026
# Caveat emptor

# Code to produce a map of the study region

# R version 4.5.1

# Load required packages ----
library(terra) # terra_1.8-70
library(dplyr) # dplyr_1.1.4 
library(ggplot2) # 4.0.0
library(tidyterra) # 0.7.2
library(ggspatial) #1.1.10
library(cowplot) # 1.2.0
library(RColorBrewer) # 1.1-3
library(sf) # 1.0-21
library(ggnewscale) # 0.5.2

# Load the data
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


Aus <- vect('./00_Data/Australia_shapefile/STE_2021_AUST_GDA2020.shp') %>% 
  project("EPSG:3577")
QN <- Aus[Aus$STATE_NAME == "Queensland" | Aus$STATE_NAME == "New South Wales"]

e <- ext(1920000, 2060000, -3250000, -3075000)

Aus <- crop(Aus, e)

BTRW_coords <- BTRW_pres[, c(4:5, 8)]
colnames(BTRW_coords) <- c("y", "x", "year")
head(BTRW_coords)
BTRW_coords$year <- as.numeric(BTRW_coords$year)

BTRW_cds <- vect(BTRW_coords, geom = c("x", "y"), crs = "EPSG:4326") %>% 
  project('EPSG:3577') %>% 
  crop(e)
BTRW_cds
plot(BTRW_cds)

places <- vect('./00_Data/Environmental_data/Place_names/Place_names_gazetteer.shp') %>% 
  project('EPSG:3577') %>% 
  crop(e)
places <- places[places$place_name =="Brisbane"| places$place_name =="Toowoomba"| places$place_name =="Esk" | places$place_name == "Boonah"]
places <- places[!duplicated(places$place_name),]


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
landuse # Ignore error
# Convert to polygons
landuse_poly <- as.polygons(landuse, na.rm = T)
landuse_poly <- landuse_poly[!is.na(landuse_poly$SIMP)]
landuse_poly$SIMP <- as.factor(landuse_poly$SIMP)
str(landuse_poly$SIMP)
landuse_poly$SIMP <- factor(landuse_poly$SIMP, levels = c("Residential land", "Agricultural or intensive use land", "Conservation area"))
levels(landuse_poly$SIMP)

BVG <- rast('./00_Data/Environmental_data/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb', lyrs = "NVIS7_0_AUST_EXT_MVG_ALB")
BVG <-  crop(project(BVG, 'EPSG:3577'), e)
unique(BVG$NVIS7_0_AUST_EXT_MVG_ALB)

BVG$remnant <- as.factor(ifel(BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Sea and estuaries" | BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Inland aquatic - freshwater, salt lakes, lagoons" | BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Cleared, non-native vegetation, buildings" | BVG$NVIS7_0_AUST_EXT_MVG_ALB == "Naturally bare - sand, rock, claypan, mudflat", NA, 1))
head(BVG)

# Want to have two greens to display remnant vegetation cover "#BAE4B3" for non-protected areas and #6f886b for protected areas
cons_poly <- landuse_poly[landuse_poly$SIMP == "Conservation area", ]
cons_rast <- rasterize(cons_poly, BVG, field = 1, background = 0)


BVG$remnant_cons <- as.factor(ifel(BVG$remnant == 1 & cons_rast == 1, 1, ifel(BVG$remnant == 1 & cons_rast == 0, 2, NA))) # 1 = conservation, 2 = non-conservation land


# Create study area map ----
# Plot remnant vegetation cover as background, overlay black polygon outlines for conservation areas. Use patterns for agricultural land and residential land
# Add presence points with gradient colour by year single colour
# Plot landuse in background with gradient from dark grey to light grey - light grey = minimal use
# Veg cover as black outlines??
brewer.pal(5, "Greys")
brewer.pal(5, "Greens")

display.brewer.all()
brewer.pal(9, "Blues")
pal <- c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")

# To figure out the colouring of remnant veg on map
blended <- col2rgb("#94b68f")/255 * 0.4 + col2rgb("#252525")/255 * 0.6
rgb(blended[1], blended[2], blended[3])


blended_non_cons <- col2rgb("#BAE4B3")/255 * 0.4 + col2rgb("#969696")/255 * 0.6 # #BAE4B3 at 0.4 alpha over agricultural grey
rgb(blended_non_cons[1], blended_non_cons[2], blended_non_cons[3])

ggplot()+
  geom_spatvector(data = Aus, fill = 'transparent')+
  theme_bw() +
  annotation_scale(location = 'bl', pad_y = unit(0.2, 'cm'), pad_x = unit(0.7, "cm"), text_cex = 1.2) +
  annotation_north_arrow(location = "bl", which_north = T, height = unit(.9, "cm"), width = unit(.5, "cm"), pad_y = unit(0.05, "cm"), pad_x = unit(0.05, 'cm'), style = north_arrow_fancy_orienteering) +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_blank())+
  theme_cowplot(font_size = 17) +
  geom_spatvector(data = landuse_poly, aes(fill = SIMP), col = NA) +
  scale_fill_manual(values = c("Residential land" = "#F0F0F0", "Agricultural or intensive use land" = "#969696", "Conservation area" = "#252525"),  labels = c("Residential", "Conservation and \nminimal use", "Agricultural and \nother intensive use"), name = "Land use") +
  new_scale_fill()+
  geom_spatraster(data = BVG, aes(fill = remnant_cons), alpha = 0.4) +
  scale_fill_manual(values = c("1" = "#94b68f", "2" = "#BAE4B3"), na.value = "transparent", na.translate = FALSE, name = "Remnant vegetation", labels = c("Conservation land", "Non-conservation land"), breaks = c("1", "2"), guide = guide_legend(override.aes = list(fill = c("#515F4F", "#A4B5A2"), alpha = 1))) + # NA.translate makes sure NAs are not added to the legend
  geom_spatvector(data = BTRW_cds, fill = NA, size = 0.7, aes(col = year)) +
  scale_color_continuous(palette = pal, breaks = c(1990, 2000, 2010, 2020, 2025), name = 'BTRW record year') +
  geom_sf_text(data = places, aes(label = place_name, geometry = geometry), show.legend = F, fontface = 'bold', size = 3.1)+
  labs(x = "", y = "")
ggsave("./03_Results/Plots/Study_area_without_roads.png", width = 22, height = 18, dpi = 300, units = 'cm')





