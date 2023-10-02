# Original Date: 2022-10-04
# Last updated : 2022-10-29 by MB
# S Ogletree, M Browning
# Description: Processing for PADUS-AR
#              determine percent area for census geographies in park lands
# Concept: Script will calculate zonal statistics on PAD-US-AR with counties, tracts, zip codes.
#          Will also intersect with with 1/2 mile buffers.
#          !! Processing tracts and ZCTA will be slow.
#          Whole script could take 3 hours to run !!


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
  c("tidyverse",
    "tigris",
    "sf",
    "openxlsx",
    "tictoc",
    "beepr",
    "here")
lapply(required_packages_1, installpkg)
invisible(lapply(required_packages_1, library, character.only = TRUE))

options(tigris_use_cache = TRUE) # so that you only download once

# !! if different values result check package versions !!
# Run with tidyverse 1.3.2, tigris 1.6.1, sf 1.0-8, openxlsx 4.2.5, tictoc 1.1, beepr 1.3, here 1.0.1

# ----------------------------------------------------------------------%
# load data -------------------------------------------------------------
# ----------------------------------------------------------------------%

# load PAD-US-AR dataset
padus_ar <- st_read("../3_data/padus_ar_dissolve.gpkg")

# break dissolved multipart into single parts and validate (single parts process faster)
padus_ar <- st_cast(padus_ar, "POLYGON")
padus_ar <- st_make_valid(padus_ar)

# select states in CONUS, no AK or HI, but add DC
sts <- c(state.abb[-c(2, 11)], "DC")
conus <-
  states(year = 2019) %>% filter(STUSPS %in% sts) %>% st_union() %>% st_sf()

# set CRS for CONUS
conus <- st_transform(conus, st_crs(padus_ar))

# select states for counties and set CRS
county <- sts %>% map_df(~ counties(state = .x, year = 2019))
county <- st_transform(county, st_crs(padus_ar))

# select states for tracts and set CRS
tract <-
  unique(county$STATEFP) %>% map_df(~ tracts(state = .x, year = 2019))
tract <- st_transform(tract, st_crs(padus_ar))

# select states for zip codes and set CRS
zcta <- zctas(year = 2019)
zcta <- st_transform(zcta, st_crs(padus_ar))
zcta <- zcta %>% filter(st_intersects(zcta, conus, sparse = FALSE))

# ----------------------------------------------------------------------%
# intersect and calculate percent --------------------------------------
# ----------------------------------------------------------------------%

# counties --------------------------------------------------------------

# start timer
tic("counties")

# ensure crs are equal
st_crs(county) == st_crs(padus_ar)

# add full county area
county <- county %>% mutate(c_area = as.numeric(st_area(county)))

# run intersection
c1 <-
  st_intersection(county, padus_ar) %>% mutate(p_area = as.numeric(st_area(.)))

# calculate park cover - group & summarize in case different parks overlap
c1a <-
  c1 %>% st_drop_geometry() %>% group_by(GEOID) %>% summarise(c_area = first(c_area),
                                                              p_area = sum(p_area, na.rm = T))
c2 <-
  c1a %>% mutate(pc_park = round((p_area / c_area) * 100, 3)) %>% select(GEOID, pc_park)

# join to county data frame, units with no overlap will be NA, make 0
countypark <-  county %>%
  select(GEOID, NAME) %>%
  left_join(c2) %>%
  st_drop_geometry() %>%
  mutate(pc_park = ifelse(is.na(pc_park), 0, pc_park))

# export data
write.xlsx(countypark, "../3_data/padus_ar_cov_cnty.xlsx")
write.csv(countypark,
          file = "../3_data/padus_ar_cov_cnty.txt",
          row.names = F,
          na = "")
# txt file maintains leading zeros
# when joining in GIS, do not automatically detect fields
# also specify that comma separates values

toc() #238 sec

# tracts ------------------------------------------------------------------

# start timer
tic("tracts")

# ensure crs are equal
st_crs(tract) == st_crs(padus_ar)

# add full tract area
tract <- tract %>% mutate(t_area = as.numeric(st_area(tract)))

# run intersection
t1 <-
  st_intersection(tract, padus_ar) %>% mutate(p_area = as.numeric(st_area(.)))

# calculate park cover - group & summarize in case different parks overlap
t1a <-
  t1 %>% st_drop_geometry() %>% group_by(GEOID) %>% summarise(t_area = first(t_area),
                                                              p_area = sum(p_area, na.rm = T))
t2 <-
  t1a %>% mutate(pc_park = round((p_area / t_area) * 100, 3)) %>% select(GEOID, pc_park)

# join to tract data frame, units with no overlap will be NA, make 0
tractpark <-  tract %>%
  select(GEOID, STATEFP, COUNTYFP) %>%
  left_join(t2) %>%
  st_drop_geometry() %>%
  mutate(pc_park = ifelse(is.na(pc_park), 0, pc_park))

# export data
write.xlsx(tractpark, "../3_data/padus_ar_cov_trct.xlsx")
write.csv(tractpark,
          file = "../3_data/padus_ar_cov_trct.txt",
          row.names = F,
          na = "")
# txt file maintains leading zeros
# when joining in GIS, do not automatically detect fields
# also specify that comma separates values

toc(log = T) #1095 sec

# tracts with a buffer ----------------------------------------

# start timer
tic("tracts with buffer")

# create buffer
tractbuf <- st_buffer(tract, 804.672) # 804.672 = half mile

# ensure crs are equal
st_crs(tractbuf) == st_crs(padus_ar)

# add full tract area
tractbuf <-
  tractbuf %>% mutate(tb_area = as.numeric(st_area(tractbuf)))

# run intersection
tb1 <-
  st_intersection(tractbuf, padus_ar) %>% mutate(p_area = as.numeric(st_area(.)))

# calculate park cover - group & summarize in case different parks overlap
tb1a <-
  tb1 %>% st_drop_geometry() %>% group_by(GEOID) %>% summarise(tb_area = first(tb_area),
                                                               p_area = sum(p_area, na.rm = T))
tb2 <-
  tb1a %>% mutate(pc_park = round((p_area / tb_area) * 100, 3)) %>% select(GEOID, pc_park)

# join to tractbuf data frame, units with no overlap will be NA, make 0
tractbufpark <-  tractbuf %>%
  select(GEOID, STATEFP, COUNTYFP) %>%
  left_join(tb2) %>%
  st_drop_geometry() %>%
  mutate(pc_park = ifelse(is.na(pc_park), 0, pc_park))

# export data
write.xlsx(tractbufpark, "../3_data/padus_ar_cov_btrct.xlsx")
write.csv(tractbufpark,
          file = "../3_data/padus_ar_cov_btrct.txt",
          row.names = F,
          na = "")
# txt file maintains leading zeros
# when joining in GIS, do not automatically detect fields
# also specify that comma separates values

toc(log = T) #1930 sec

# ZCTA ----------------------------------------

# start timer
tic("zip codes")

# ensure crs are equal
st_crs(zcta) == st_crs(padus_ar)

# add full tract area
zcta <- zcta %>% mutate(z_area = as.numeric(st_area(zcta)))

# run intersection
z1 <-
  st_intersection(zcta, padus_ar) %>% mutate(p_area = as.numeric(st_area(.)))

# calculate park cover - group & summarize in case different parks overlap
z1a <-
  z1 %>% st_drop_geometry() %>% group_by(ZCTA5CE10) %>% summarise(z_area = first(z_area),
                                                                  p_area = sum(p_area, na.rm = T))
z2 <-
  z1a %>% mutate(pc_park = round((p_area / z_area) * 100, 3)) %>% select(ZCTA5CE10, pc_park)

# join to tract data frame, units with no overlap will be NA, make 0
zctapark <-  zcta %>%
  select(ZCTA5CE10) %>%
  left_join(z2) %>%
  st_drop_geometry() %>%
  mutate(pc_park = ifelse(is.na(pc_park), 0, pc_park))

# export data
write.xlsx(zctapark, "../3_data/padus_ar_cov_zip.xlsx")
write.csv(zctapark,
          "../3_data/padus_ar_cov_zip.txt",
          row.names = F,
          na = "")

toc(log = T) # 526 sec

# ------------------------------------------------------------------------%
# Complete ----------------------------------------------------------------
# ------------------------------------------------------------------------%

beep(3) # fun sound for completion