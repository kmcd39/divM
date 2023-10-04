# Original Date: 2022-09-28
# Last Updated: 2022-10-29 by MB
# Author:S Ogletree, M Browning
# Description: Clean up source data for PAD-US-AR creation
# Concept: Clean up datasets to CONUS only. Dissolve to eliminate overlaps.
#          Validate geometries as source data often contain invalid geometries.
#          Dissolved layers used to get percent in county and tract.
#          All intermediate data written to geopackage for better performance.
#          Recommend restarting computer and closing cloud syncing of files (Box)
#          Times below were obtained on 2019 MBP with 2.6Ghz i7 and 32GB mem

# ------------------------------------------------------------------------%
# Set up  -----------------------------------------------------------------
# ------------------------------------------------------------------------%

# function for installing needed packages
installpkg <- function(x) {
  if (x %in% rownames(installed.packages()) == FALSE) {
    if (x %in% rownames(available.packages()) == FALSE) {
      paste(x, "is not a valid package - please check again...")
    } else {
      install.packages(x)
    }

  } else {
    paste(x, "package already installed...")
  }
}

# load and install necessary packages
required_packages_1  <-
  c("tidyverse", "sf", "tictoc", "beepr", "here")
lapply(required_packages_1, installpkg)
invisible(lapply(required_packages_1, library, character.only = TRUE))

# !! if different values result check package versions !!
# Run with tidyr 1.3.2, sf 1.0-8, tictoc 1.1, beepr 1.3, here 1.0.1

# ------------------------------------------------------------------------%
# Census regions ----------------------------------------------------------
# ------------------------------------------------------------------------%

# source: NHGIS and US Census (circa 2019)
# https://urldefense.com/v3/__https://data2.nhgis.org/__;!!PTd7Sdtyuw!R0utMYmn79QaKg0yAbg6Z0I5_raU7jmDVXQ9JmA-uEu35rfsF1dvraoK8GRHCSVdYP04vdTIlgNUn1gAPfEsewMtjPM$
# downloaded 07 July 2022
regions <- st_read("../1_source_data/census_regions/regions.shp")

# ------------------------------------------------------------------------%
# ESRI parks --------------------------------------------------------------
# ------------------------------------------------------------------------%

# source: ESRI USA Parks Dataset
# https://www.arcgis.com/home/item.html?id=578968f975774d3fab79fe56c8c90941
# downloaded summer 2022
esri <- st_read("../1_source_data/ESRI/ESRI_bystate.shp")

# match CRS
esri2 <- st_transform(esri, st_crs(regions))

# clip esri parks by the regions for CONUS parks
esri3 <- st_crop(esri2, regions)
summary(esri3$geometry) # note that some multipolygons became polygons

# count features and get the distribution of areas
c("Total no. of features: ", nrow(esri3)) #61,030

# make valid
esri3 <- st_make_valid(esri3)

# add region to the features
tic("intersect")
esri3_int <- st_intersection(esri3, regions)
toc(log = T) #142 sec

# count features and get the distribution of areas by region
table(esri3_int$NAME.1)

# make sure it's all valid before dissolving
esri3 <- st_make_valid(esri3)

# save to intermediate data
st_write(esri3, "../2_intermediate_data/esri_parks_conus.shp")
# ! writing as geopackage results in error so we save this as a shapefile !

# dissolve to remove overlaps and double counting
tic("dissolve")
esri4 <- esri3 %>% st_union() %>% st_sf()
toc(log = T) #101 sec

# make sure this is valid after dissolve
esri4 <- st_make_valid(esri4)

# get dissolved area by region
tic("split by region")
esri_reg <-
  st_intersection(esri4, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(esri4)) / 1000000,
  "SqKm") #1,409,517
c("South area (dissolved):", esri_reg$area_diss[1] / 1000000, "SqKm") #172,960
c("Northeast area (dissolved):",
  esri_reg$area_diss[4] / 1000000,
  "SqKm") #52,082
c("Midwest area (dissolved):",
  esri_reg$area_diss[2] / 1000000,
  "SqKm") #110,524
c("West area (dissolved):", esri_reg$area_diss[3] / 1000000, "SqKm") #700,588
toc(log = T) #21 sec

# save to intermediate data
tic("export again")
st_write(esri4, "../2_intermediate_data/esri_parks_dissolve.gpkg")
toc(log = T) #1 sec

# ------------------------------------------------------------------------%
# Trust for Public Land (TPL) ParkServe -----------------------------------
# ------------------------------------------------------------------------%

# source: Trust for Public Land ParkServe version 05042022
# https://urldefense.com/v3/__https://www.tpl.org/parkserve/downloads__;!!PTd7Sdtyuw!R0utMYmn79QaKg0yAbg6Z0I5_raU7jmDVXQ9JmA-uEu35rfsF1dvraoK8GRHCSVdYP04vdTIlgNUn1gAPfEsFYplWNg$
# downloaded: 07 May 2022
tpl <- st_read("../1_source_data/TPL_ParkServe/ParkServe_Parks.shp")

# make sure it's all valid before continuing
tic("valid")
tpl <- st_make_valid(tpl)
toc(log = T) #59 sec

# check crs
tpl2 <- st_transform(tpl, st_crs(regions))

# clip tpl parks by regions for CONUS parks
tpl3 <- st_crop(tpl2, regions)

# make sure it's all valid before continuing
tic("valid")
tpl3 <- st_make_valid(tpl3)
toc(log = T) #47 sec

# count features and get the distribution of areas at this point
c("Total no. of features: ", nrow(tpl3)) #135,179

# add region to the features
tic("intersect")
tpl_int <- st_intersection(tpl3, regions)
table(tpl_int$NAME) # count by region
toc(log = T) #270 sec2

# export before dissolve
st_write(tpl3, "../2_intermediate_data/tpl_parks_conus.gpkg")

# dissolve to remove overlaps and double counting
tic("dissolve")
tpl4 <- tpl3 %>% st_union() %>% st_sf()
toc(log = T) #348 sec

# make sure this is valid after dissolve
tic("valid again")
tpl4 <- st_make_valid(tpl4)
toc(log = T) #363 sec

# get dissolved part split by region
tic("intersect again")
tpl_reg <-
  st_intersection(tpl4, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(tpl4)) / 1000000,
  "SqKm")
c("South area (dissolved):", tpl_reg$area_diss[1] / 1000000, "SqKm")
c("Northeast area (dissolved):",
  tpl_reg$area_diss[4] / 1000000,
  "SqKm")
c("Midwest area (dissolved):",
  tpl_reg$area_diss[2] / 1000000,
  "SqKm")
c("West area (dissolved):", tpl_reg$area_diss[3] / 1000000, "SqKm")
toc(log = T) #48 sec

# save to intermediate data
st_write(tpl4, "../2_intermediate_data/tpl_parks_dissolve.gpkg")

# ------------------------------------------------------------------------%
# Open Street Map (OSM) data ----------------------------------------------
# ------------------------------------------------------------------------%

# source: Open Street Map
# https://urldefense.com/v3/__https://download.geofabrik.de/north-america.html__;!!PTd7Sdtyuw!R0utMYmn79QaKg0yAbg6Z0I5_raU7jmDVXQ9JmA-uEu35rfsF1dvraoK8GRHCSVdYP04vdTIlgNUn1gAPfEsWKoEUVE$
# downloaded 08 June 2022
osm <- st_read("../1_source_data/OSM/osm_polys_states_cut.shp")

# ensure crs matches regions
osm <- st_transform(osm, st_crs(regions))

# validate geometries
osm <- st_make_valid(osm)

# clip the osm parks by the regions for CONUS parks
osm <- st_crop(osm, regions)

# select tags we want
ltag <- c("dog_park", "garden", "nature_reserve", "park")
btag <- c("national_park", "protected_area")

# filter for these tags
osm1 <- osm %>% filter(leisure %in% ltag)
osm2 <- osm %>% filter(boundary %in% btag)

# add area - in sq mt
osm1 <- osm1 %>% mutate(area = as.numeric(st_area(osm1)))
osm2 <- osm2 %>% mutate(area = as.numeric(st_area(osm2)))

### can count features and get the distribution of areas at this point ###
c("Total no. of features leisure tags: ", nrow(osm1)) #309,160
c("Total no. of features boundary tags: ", nrow(osm2)) #51,966

# add region to the features
tic("intersect")
osm1_int <- st_intersection(osm1, regions)
table(osm1_int$NAME) # count leisure tags by region
osm2_int <- st_intersection(osm2, regions)
table(osm2_int$NAME) # count boundary tags by region
toc(log = T) #700 sec

# save selected OSM data
osm1 %>%
  mutate(fid = 1:nrow(.), osm_id = as.character(osm_id))  %>%
  select(fid, osm_id, area) %>%
  st_write("../2_intermediate_data/osm_leisure_conus.gpkg")

osm2 %>%
  mutate(fid = 1:nrow(.), osm_id = as.character(osm_id))  %>%
  select(fid, osm_id, area) %>%
  st_write("../2_intermediate_data/osm_boundary_conus.gpkg")

# dissolve to remove overlaps and double counting
tic("dissolve")
osm_1l <- osm1 %>% st_union() %>% st_sf()
osm_2l <- osm2 %>% st_union() %>% st_sf()
toc(log = T) #377 sec

# make sure this is valid after dissolve
tic("valid")
osm_1l <- st_make_valid(osm_1l)
osm_2l <- st_make_valid(osm_2l)
toc(log = T) #375 sec

# get the dissolved part split by region
tic("dissolve for area calculation by region")
osm_l_reg <-
  st_intersection(osm_1l, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(osm_1l)) / 1000000,
  "SqKm")
c("South area (dissolved):",
  osm_l_reg$area_diss[1] / 1000000,
  "SqKm")
c("Northeast area (dissolved):",
  osm_l_reg$area_diss[4] / 1000000,
  "SqKm")
c("Midwest area (dissolved):",
  osm_l_reg$area_diss[2] / 1000000,
  "SqKm")
c("West area (dissolved):", osm_l_reg$area_diss[3] / 1000000, "SqKm")

osm_b_reg <-
  st_intersection(osm_2l, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(osm_2l)) / 1000000,
  "SqKm")
c("South area (dissolved):",
  osm_b_reg$area_diss[1] / 1000000,
  "SqKm")
c("Northeast area (dissolved):",
  osm_b_reg$area_diss[4] / 1000000,
  "SqKm")
c("Midwest area (dissolved):",
  osm_b_reg$area_diss[2] / 1000000,
  "SqKm")
c("West area (dissolved):", osm_b_reg$area_diss[3] / 1000000, "SqKm")
toc(log = T) #70 sec

# save to intermediate data
st_write(osm_1l, "../2_intermediate_data/osm_leisure_dissolve.gpkg")
st_write(osm_2l, "../2_intermediate_data/osm_boundary_dissolve.gpkg")

# ------------------------------------------------------------------------%
# Complete ----------------------------------------------------------------
# ------------------------------------------------------------------------%

beep(3) # fun sound for completion
