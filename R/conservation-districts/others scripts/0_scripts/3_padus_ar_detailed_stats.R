# Original Date: 2022-10-03
# Last updated : 2022-10-29 by MB
# M Browning, S Ogletree
# Description: Calculate statistics for PAD-US-AR
# Concept: Calculate detailed statistics for each step in the PAD-US-AR curation

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
  c("tidyr", "dplyr", "sf", "tictoc", "beepr", "here")
lapply(required_packages_1, installpkg)
invisible(lapply(required_packages_1, library, character.only = TRUE))

# !! if different values result check package versions !!
# Run with tidyr 1.1.4, dplyr 1.0.7, sf 1.0-8, tictoc 1.1, beepr 1.3, here 1.0.1

# ------------------------------------------------------------------------%
# census regions ----------------------------------------------------------
# ------------------------------------------------------------------------%

# source: https://data2.nhgis.org/
# 2019 Census Regions - downloaded 07 July 2022
regions <- st_read("../1_source_data/census_regions/regions.shp")

# ------------------------------------------------------------------------%
# PADUSV2.1 --------------------------------------------------------------
# ------------------------------------------------------------------------%
# source: https://www.sciencebase.gov/catalog/item/5f186a2082cef313ed843257
# release: 2020-09-15

# start timer
tic("detailed statistics for PAD-US-AR dataset")

# load data
padus <-
  sf::st_read("../1_source_data/PADUS/PAD_US2_1.gdb", layer = "PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement")

#check geometries
summary(st_geometry(padus)) # some are multi-surface

# make geometry type consistent
padus2 <- st_cast(padus, "MULTIPOLYGON")

#check geometries
summary(st_geometry(padus2)) # should all be MULTIPOLYGON

# set CRS
padus3 <- st_transform(padus2, st_crs(regions))
st_crs(regions) == st_crs(padus3) #should be true
summary(st_geometry(padus3)) # should all be MULTIPOLYGON

# overcome invalid geometry
padus4 <- st_make_valid(padus3)

# clip parks by the regions for CONUS parks
padus5 <- sf::st_crop(padus4, regions)

########################################################
# remove marine feature class ##########################
# use this dataframe padus6 for PAD-US V2.1 statistics #
########################################################

#start timer
tic("padus6 calculations")

# create dataset
padus6 <- padus5[padus5$FeatClass != "Marine",]

# count number of features = 430,836
c("Total no. of features: ", nrow(padus6))

# designation units
tic("designation")
padus6_desg <- padus6[padus6$FeatClass == "Designation",]
c("Total no. of features: ", nrow(padus6_desg)) # 13,239
padus6_desg_dis <- padus6_desg %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus6_desg_dis)) / 1000000,
  "SqKm") # 631,489 km2
toc(log = T) # 408 sec

# easement units
tic("easement")
padus6_ease <- padus6[padus6$FeatClass == "Easement",]
c("Total no. of features: ", nrow(padus6_ease)) # 163,191
padus6_ease_dis <- padus6_ease %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus6_ease_dis)) / 1000000,
  "SqKm") # 137,812 km2
toc(log = T) # 143 sec

# proclamation units
tic("proclamation")
padus6_ease <- padus6[padus6$FeatClass == "Proclamation",]
c("Total no. of features: ", nrow(padus6_ease)) # 2,708
padus6_ease_dis <- padus6_ease %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus6_ease_dis)) / 1000000,
  "SqKm") # 2,449,636 km2
toc(log = T) #

# fee units
tic("fee")
padus6_fee <- padus6[padus6$FeatClass == "Fee",]
c("Total no. of features: ", nrow(padus6_fee)) # 251,698
padus6_fee_dis <- padus6_fee %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus6_fee_dis)) / 1000000,
  "SqKm") # 2,045,284 km2
toc(log = T) # 3,027 sec !! 1 hr !!
beep(3)

# designation, fee, easement, and proclamation
# dissolve to remove overlaps and double counting
tic("padus6 dissolve")
padus6_dis <- padus6 %>% st_union() %>% st_sf()
padus6_dis <-
  st_make_valid(padus6_dis) # make sure valid after dissolve
toc() # 17,564 sec !! 5 hr !!
beep(3)

# area by census region
tic("area by region")
padus6 <- st_intersection(padus6, regions)
table(padus6$NAME) # numbers of units MW = 118496, NE = 110327, S = 90815 W = 110650
padus6_reg <-
  st_intersection(padus6_dis, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(padus6_dis)) / 1000000,
  "SqKm")
c("South area (dissolved):",
  padus6_reg$area_diss[1] / 1000000,
  "SqKm")
c("Northeast area (dissolved):",
  padus6_reg$area_diss[4] / 1000000,
  "SqKm")
c("Midwest area (dissolved):",
  padus6_reg$area_diss[2] / 1000000,
  "SqKm")
c("West area (dissolved):",
  padus6_reg$area_diss[3] / 1000000,
  "SqKm")
# Total = 3,619,326 S = 438,958 NE = 117,498 MW = 817,404 W = 2,094,608 km2
toc()

################################################
# remove proclamation ##########################
################################################

tic("remove proclamation")
padus7 <- padus6[padus6$FeatClass != "Proclamation",]
c("Total no. of features: ", nrow(padus7)) # 427,538
padus7_dis <- padus7 %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus7_dis)) / 1000000,
  "SqKm") # 2,198,438 km2
toc(log = T) # 7022 sec !! 2 hrs !!
beep(3)

#################################################
# remove closed access ##########################
#################################################

tic("remove closed access")
padus8 <- padus7[padus7$Pub_Access != "XA",]
c("Total no. of features: ", nrow(padus8)) # 307,356
padus8_dis <- padus8 %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus8_dis)) / 1000000,
  "SqKm") # 2,081,342 km2
toc(log = T) # 5548 sec !! 1.5 hrs !!
beep(3)

#################################################
# remove unknown access select land managers ####
#################################################

tic("remove select unknown")
UK_BadLandMang <-
  list(
    "UK_ARS",
    "UK_BLM",
    "UK_DOD",
    "UK_DOE",
    "UK_FWS",
    "UK_JNT",
    "UK_NGO",
    "UK_NOAA",
    "UK_NPS",
    "UK_NRCS",
    "UK_OTHF",
    "UK_OTHS",
    "UK_PVT",
    "UK_RWD",
    "UK_SLB",
    "UK_TRIB",
    "UK_UNK",
    "UK_UNKL",
    "UK_USBR"
  )
padus8 <- padus8 %>%
  mutate(Pub_Access_Mang_Name = paste(Pub_Access, sep = "_", Mang_Name))
padus9 <-
  padus8 %>% filter(!Pub_Access_Mang_Name %in% UK_BadLandMang)
c("Total no. of features: ", nrow(padus9)) # 272,385
padus9_dis <- padus9 %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus9_dis)) / 1000000,
  "SqKm") # 2,038,766 km2
toc(log = T) # 5011 sec !! 1.5 hrs !!
beep(3)

#########################
# remove select SRMA ####
#########################

tic("remove select SRMA")
SRMA_BadStates <-
  list(
    "SRMA_MS",
    "SRMA_OK",
    "SRMA_ND",
    "SRMA_SD",
    "SRMA_MT",
    "SRMA_ID",
    "SRMA_WY",
    "SRMA_UT",
    "SRMA_WA",
    "SRMA_OR",
    "SRMA_AZ",
    "SRMA_NM",
    "SRMA_TX",
    "SRMA_CO",
    "SRMA_LA",
    "SRMA_NV"
  )
padus9 <- padus9 %>%
  mutate(Des_Tp_State_Nm = paste(Des_Tp, sep = "_", State_Nm))
padus10 <- padus9 %>% filter(!Des_Tp_State_Nm %in% SRMA_BadStates)
c("Total no. of features: ", nrow(padus10)) # 248,871
padus10_dis <- padus10 %>% st_union() %>% st_sf()
c("Total area (dissolved):",
  as.numeric(st_area(padus10_dis)) / 1000000,
  "SqKm") # 1,866,564 km2

# area by census region
padus10 <- st_intersection(padus10, regions)
table(padus10$NAME) # MW = 69,493 NE = 63,530 S = 54,456 W = 61,693
padus10_reg <-
  st_intersection(padus10_dis, regions) %>% mutate(area_diss = st_area(.))
c("Total area (dissolved):",
  as.numeric(st_area(padus10_dis)) / 1000000,
  "SqKm")
c("South area (dissolved):",
  padus10_reg$area_diss[1] / 1000000,
  "SqKm")
c("Northeast area (dissolved):",
  padus10_reg$area_diss[4] / 1000000,
  "SqKm")
c("Midwest area (dissolved):",
  padus10_reg$area_diss[2] / 1000000,
  "SqKm")
c("West area (dissolved):",
  padus10_reg$area_diss[3] / 1000000,
  "SqKm")
# Total = 1,866,564 S = 172,563 NE = 68,037 MW = 161,952 W = 1,464,012 km2

toc() # 6,320 sec !! 2 hrs !!

# ------------------------------------------------------------------------%
# Complete ----------------------------------------------------------------
# ------------------------------------------------------------------------%

beep(3) # fun sound for completion