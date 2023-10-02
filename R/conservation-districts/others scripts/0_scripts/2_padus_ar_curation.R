# Original Date: 2022-10-03
# Last updated : 2022-10-31 by MB
# M Browning, S Ogletree
# Description: Curate PAD-US-AR dataset
# Concept: Limit PAD-US to parks intended for public recreation.
#          Clean up dataset to CONUS Dissolve to eliminate overlaps.
#          Validate geometries as source data often contain invalid geometries.
#          Dissolve layers used to get percent in county and tract.
#          All intermediate data written to geopackage for better performance.

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
  c("tidyverse", "sf", "rgdal", "tictoc", "beepr", "here")
lapply(required_packages_1, installpkg)
invisible(lapply(required_packages_1, library, character.only = TRUE))

# !! if different values result check package versions !!
# Run with tidyverse 1.3.2, sf 1.0-8, rgdal 1.5-28, tictoc 1.1, beepr 1.3, here 1.0.1

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
tic("Curate PAD-US-AR dataset")

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

# remove marine feature class
padus6 <- padus5[padus5$FeatClass != "Marine",]

# exclude proclamation class
padus7 <- padus6[padus6$FeatClass != "Proclamation",]

# exclude closed access
padus8 <- padus7[padus7$Pub_Access != "XA",]

# exclude unknown access select land managers
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

# exclude select SMRA areas
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

# dissolve
tic("dissolve")
padus10_dis <- padus10 %>% st_union() %>% st_sf()
toc()

# make sure this is valid after dissolve
padus10_dis

# save to final data
st_write(padus10, "../3_data/padus_ar.gpkg")
padus10poly <- st_collection_extract(padus10, "POLYGON")
st_write(padus10poly, "../3_data/padus_ar.shp")
st_write(padus10_dis, "../3_data/padus_ar_dissolve.gpkg")
toc(log = T)

# ------------------------------------------------------------------------%
# Complete ----------------------------------------------------------------
# ------------------------------------------------------------------------%

beep(3) # fun sound for completion