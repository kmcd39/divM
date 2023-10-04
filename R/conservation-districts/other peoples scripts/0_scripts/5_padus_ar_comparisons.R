# Original Date: 2022-10-14
# Last updated : 2022-10-30 by MB
# M Browning
# Description: Compare PAD-US-AR park cover with sociodemographics, NDVI, and canopy cover
# Concept: Calculate descriptive statistics for PAD-US-AR and other relevant datasets
#          Calculate bivariate correlations between PAD-US-AR and other greenspace metrics
#          Calculate multivariate associations between PAD-US-AR and sociodemographics

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
  c(
    "tidyverse",
    "readr",
    "sf",
    "raster",
    "exactextractr",
    "tictoc",
    "beepr",
    "here",
    "dplyr",
    "stringr",
    "tigris",
    "sjmisc",
    "ggeasy",
    "gridExtra",
    "sjPlot",
    "lme4",
    "correlation"
  )
lapply(required_packages_1, installpkg)
invisible(lapply(required_packages_1, library, character.only = TRUE))

options(tigris_use_cache = TRUE) # so that you only download once

# !! if different values result check package versions !!
# Run with tidyverse 1.3.2, readr 2.1.1, sf 1.0-8, raster 3.5-9,
#          exactextractr 0.7.2, tictoc 1.1, beepr 1.3, here 1.0.1,
#          dplyr 1.0.7, stringr 1.4.0, tigris 1.6.1, sjmisc 2.8,9
#          ggeasy 0.1.3, gridExtra 2.3, sjPlot 2.8.10, lme4 1.1-27.1,
#          correlation 0.7.1

# ------------------------------------------------------------------------%
# census regions ----------------------------------------------------------
# ------------------------------------------------------------------------%

# source: https://data2.nhgis.org/
# 2019 Census Regions - downloaded 07 July 2022
regions <- st_read("../1_source_data/census_regions/regions.shp")

# select states in CONUS, no AK or HI, but add DC
sts <- c(state.abb[-c(2, 11)], "DC")
conus <-
  states(year = 2019) %>% filter(STUSPS %in% sts) %>% st_union() %>% st_sf()

# set CRS for CONUS
conus <- st_transform(conus, st_crs(regions))

NE.name <- c(
  "Connecticut",
  "Maine",
  "Massachusetts",
  "New Hampshire",
  "Rhode Island",
  "Vermont",
  "New Jersey",
  "New York",
  "Pennsylvania"
)
NE.abrv <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
NE.ref <- c(NE.name, NE.abrv)

MW.name <- c(
  "Indiana",
  "Illinois",
  "Michigan",
  "Ohio",
  "Wisconsin",
  "Iowa",
  "Kansas",
  "Minnesota",
  "Missouri",
  "Nebraska",
  "North Dakota",
  "South Dakota"
)
MW.abrv <- c("IN",
             "IL",
             "MI",
             "OH",
             "WI",
             "IA",
             "KS",
             "MN",
             "MO",
             "NE",
             "ND",
             "SD")
MW.ref <- c(MW.name, MW.abrv)

S.name <- c(
  "Delaware",
  "District of Columbia",
  "Florida",
  "Georgia",
  "Maryland",
  "North Carolina",
  "South Carolina",
  "Virginia",
  "West Virginia",
  "Alabama",
  "Kentucky",
  "Mississippi",
  "Tennessee",
  "Arkansas",
  "Louisiana",
  "Oklahoma",
  "Texas"
)
S.abrv <- c(
  "DE",
  "DC",
  "FL",
  "GA",
  "MD",
  "NC",
  "SC",
  "VA",
  "WV",
  "AL",
  "KY",
  "MS",
  "TN",
  "AR",
  "LA",
  "OK",
  "TX"
)
S.ref <- c(S.name, S.abrv)

W.name <- c(
  "Arizona",
  "Colorado",
  "Idaho",
  "New Mexico",
  "Montana",
  "Utah",
  "Nevada",
  "Wyoming",
  "Alaska",
  "California",
  "Hawaii",
  "Oregon",
  "Washington"
)
W.abrv <- c("AZ",
            "CO",
            "ID",
            "NM",
            "MT",
            "UT",
            "NV",
            "WY",
            "AK",
            "CA",
            "HI",
            "OR",
            "WA")
W.ref <- c(W.name, W.abrv)

region.list <- list(
  Northeast = NE.ref,
  Midwest = MW.ref,
  South = S.ref,
  West = W.ref
)

# --------------------------------------------------------------%
# counties -----------------------------------------------------
# --------------------------------------------------------------%

# counties
# source: https://data2.nhgis.org/
# 2019 Census Regions - downloaded 07 July 2022
county <- st_read("../1_source_data/counties/USA_Counties.shp")
county_witharea <-
  sts %>% map_df( ~ counties(state = .x, year = 2019))
county <- st_transform(county, st_crs(regions))
st_crs(county) == st_crs(regions) # should be true

# ------------------------------------------------------------%
# tracts -----------------------------------------------------
# ------------------------------------------------------------%

# tracts
tract2 <- st_read("../1_source_data/tracts/US_tract_2019.shp")
tract_witharea <-
  unique(county$STATEFP) %>% map_df( ~ tracts(state = .x, year = 2019))
tract2 <- st_transform(tract2, st_crs(regions))
st_crs(tract2) == st_crs(regions) # should be true
tract$LandAreakm2 <- tract$ALAND / 1000000

# ----------------------------------------------------------------------%
# park cover data -------------------------------------------------------
# ----------------------------------------------------------------------%

parkc <- read_csv("../3_data/padus_ar_cov_cnty.txt")
parkt <- read_csv("../3_data/padus_ar_cov_btrct.txt")

# ----------------------------------------------------------------------%
# canopy cover ----------------------------------------------------------
# ----------------------------------------------------------------------%

# source: https://www.mrlc.gov/data/nlcd-2016-usfs-tree-canopy-cover-conus
# 2019 release of 2016 tree canopy cover data from MLRC - downloaded 1 Oct 2022

# load data
tree_ras <-
  raster('../1_source_data/NLCD/nlcd_2016_treecanopy_2019_08_31.img')

# match CRS
#tree_ras <- st_transform(tree_ras, crs(regions))
st_crs(county) == st_crs(tree_ras) #should be true
st_crs(tract) == st_crs(tree_ras) #should be true

# focal stats
county$PcCanopy <- exact_extract(tree_ras, county, 'mean')
tract$PcCanopy <- exact_extract(tree_ras, tract, 'mean')
tract$PcCanopy <- tract$PcCanopy / 100

# ----------------------------------------------------------------------%
# annual NDVI -----------------------------------------------------------
# ----------------------------------------------------------------------%

# source: Google Earth Engine
# MODIS 250x250m2 NDVI annual composite from Jan 1 2015 to Dec 31 2020

# load data
NDVI_ras <- raster('../1_source_data/GEE/NDVI_15to20.tif')

# match CRS
st_crs(county) == st_crs(NDVI_ras) #should be true
st_crs(tract) == st_crs(NDVI_ras) #should be true
# ! if these are not true ! use command:
# NDVI_ras <- projectRaster(NDVI_ras, crs = crs(regions))

# focal stats
county$NDVI_annual <- exact_extract(NDVI_ras, county, 'mean')
county$NDVI_annual <- county$NDVI_annual / 10000
tract$NDVI_annual <- exact_extract(NDVI_ras, tract, 'mean')
tract$NDVI_annual <- tract$NDVI_annual / 10000

# ----------------------------------------------------------------------%
# summer high NDVI ------------------------------------------------------
# ----------------------------------------------------------------------%

# source: Google Earth Engine
# MODIS 250x250m2 NDVI annual composite from Jan 1 2015 to Dec 31 2020
# zonal statistics run on GEE
# code at https://code.earthengine.google.com/3984fd87248d4d24c76744fa32e2abeb
# provided CSV files with NDVI mean for counties and tracts

# ---- counties ----

# 2015
dc_gee15 <-
  read_csv("../1_source_data/GEE/NDVI_2015_summertime_counties.csv")
names(dc_gee15) <- paste0(names(dc_gee15), "_15")
dc_gee15$ndvi_mean_15 <- dc_gee15$ndvi_mean_15 / 10000
dc_gee15$GEOID_15 <- as.numeric(dc_gee15$GEOID_15)

# 2016
dc_gee16 <-
  read_csv("../1_source_data/GEE/NDVI_2016_summertime_counties.csv")
names(dc_gee16) <- paste0(names(dc_gee16), "_16")
dc_gee16$ndvi_mean_16 <- dc_gee16$ndvi_mean_16 / 10000
dc_gee16$GEOID_16 <- as.numeric(dc_gee16$GEOID_16)

# 2017
dc_gee17 <-
  read_csv("../1_source_data/GEE/NDVI_2017_summertime_counties.csv")
names(dc_gee17) <- paste0(names(dc_gee17), "_17")
dc_gee17$ndvi_mean_17 <- dc_gee17$ndvi_mean_17 / 10000
dc_gee17$GEOID_17 <- as.numeric(dc_gee17$GEOID_17)

# 2018
dc_gee18 <-
  read_csv("../1_source_data/GEE/NDVI_2018_summertime_counties.csv")
names(dc_gee18) <- paste0(names(dc_gee18), "_18")
dc_gee18$ndvi_mean_18 <- dc_gee18$ndvi_mean_18 / 10000
dc_gee18$GEOID_18 <- as.numeric(dc_gee18$GEOID_18)

# 2019
dc_gee19 <-
  read_csv("../1_source_data/GEE/NDVI_2019_summertime_counties.csv")
names(dc_gee19) <- paste0(names(dc_gee19), "_19")
dc_gee19$ndvi_mean_19 <- dc_gee19$ndvi_mean_19 / 10000
dc_gee19$GEOID_19 <- as.numeric(dc_gee19$GEOID_19)

# 2020
dc_gee20 <-
  read_csv("../1_source_data/GEE/NDVI_2020_summertime_counties.csv")
names(dc_gee20) <- paste0(names(dc_gee20), "_20")
dc_gee20$ndvi_mean_20 <- dc_gee20$ndvi_mean_20 / 10000
dc_gee20$GEOID_20 <- as.numeric(dc_gee20$GEOID_20)

# combine yrs
dc_gee15_16 <-
  sp::merge(dc_gee15, dc_gee16, by.x = 'GEOID_15', by.y = 'GEOID_16')
dc_gee15_16_17 <-
  sp::merge(dc_gee15_16, dc_gee17, by.x = 'GEOID_15', by.y = 'GEOID_17')
dc_gee15_16_17_18 <-
  sp::merge(dc_gee15_16_17, dc_gee18, by.x = 'GEOID_15', by.y = 'GEOID_18')
dc_gee15_16_17_18_19 <-
  sp::merge(dc_gee15_16_17_18, dc_gee19, by.x = 'GEOID_15', by.y = 'GEOID_19')
dc_gee15_16_17_18_19_20 <-
  sp::merge(dc_gee15_16_17_18_19,
            dc_gee20,
            by.x = 'GEOID_15',
            by.y = 'GEOID_20')

# change vars
dc_gee15_16_17_18_19_20$FIPS <-
  sprintf("%05d", dc_gee15_16_17_18_19_20$GEOID_15)
dc_gee15_16_17_18_19_20 <-
  dc_gee15_16_17_18_19_20 %>% dplyr::select(matches("FIPS|ndvi_mean"))
dc_gee15_16_17_18_19_20$NDVI_Sum <-
  (
    dc_gee15_16_17_18_19_20$ndvi_mean_15 + dc_gee15_16_17_18_19_20$ndvi_mean_16 +
      dc_gee15_16_17_18_19_20$ndvi_mean_17 + dc_gee15_16_17_18_19_20$ndvi_mean_18 +
      dc_gee15_16_17_18_19_20$ndvi_mean_19 + dc_gee15_16_17_18_19_20$ndvi_mean_20
  ) / 6

# merge
county <-
  sp::merge(county,
            dc_gee15_16_17_18_19_20,
            by.x = 'FIPS',
            by.y = 'FIPS')

# ---- tracts ----

# 2015
dt_gee15 <-
  read_csv("../1_source_data/GEE/NDVI_2015_summertime_tracts.csv")
names(dt_gee15) <- paste0(names(dt_gee15), "_15")
dt_gee15$ndvi_mean_15 <- dt_gee15$ndvi_mean_15 / 10000
dt_gee15$geoid10_15n <- as.numeric(dt_gee15$geoid10_15)

# 2016
dt_gee16 <-
  read_csv("../1_source_data/GEE/NDVI_2016_summertime_tracts.csv")
names(dt_gee16) <- paste0(names(dt_gee16), "_16")
dt_gee16$ndvi_mean_16 <- dt_gee16$ndvi_mean_16 / 10000
dt_gee16$geoid10_16n <- as.numeric(dt_gee16$geoid10_16)

# 2017
dt_gee17 <-
  read_csv("../1_source_data/GEE/NDVI_2017_summertime_tracts.csv")
names(dt_gee17) <- paste0(names(dt_gee17), "_17")
dt_gee17$ndvi_mean_17 <- dt_gee17$ndvi_mean_17 / 10000
dt_gee17$geoid10_17n <- as.numeric(dt_gee17$geoid10_17)

# 2018
dt_gee18 <-
  read_csv("../1_source_data/GEE/NDVI_2018_summertime_tracts.csv")
names(dt_gee18) <- paste0(names(dt_gee18), "_18")
dt_gee18$ndvi_mean_18 <- dt_gee18$ndvi_mean_18 / 10000
dt_gee18$geoid10_18n <- as.numeric(dt_gee18$geoid10_18)

# 2019
dt_gee19 <-
  read_csv("../1_source_data/GEE/NDVI_2019_summertime_tracts.csv")
names(dt_gee19) <- paste0(names(dt_gee19), "_19")
dt_gee19$ndvi_mean_19 <- dt_gee19$ndvi_mean_19 / 10000
dt_gee19$geoid10_19n <- as.numeric(dt_gee19$geoid10_19)

# 2020
dt_gee20 <-
  read_csv("../1_source_data/GEE/NDVI_2020_summertime_tracts.csv")
names(dt_gee20) <- paste0(names(dt_gee20), "_20")
dt_gee20$ndvi_mean_20 <- dt_gee20$ndvi_mean_20 / 10000
dt_gee20$geoid10_20n <- as.numeric(dt_gee20$geoid10_20)

# combine
dt_gee15_16 <-
  sp::merge(dt_gee15, dt_gee16, by.x = 'geoid10_15', by.y = 'geoid10_16')
dt_gee15_16_17 <-
  sp::merge(dt_gee15_16, dt_gee17, by.x = 'geoid10_15', by.y = 'geoid10_17')
dt_gee15_16_17_18 <-
  sp::merge(dt_gee15_16_17, dt_gee18, by.x = 'geoid10_15', by.y = 'geoid10_18')
dt_gee15_16_17_18_19 <-
  sp::merge(dt_gee15_16_17_18, dt_gee19, by.x = 'geoid10_15', by.y = 'geoid10_19')
dt_gee15_16_17_18_19_20 <-
  sp::merge(dt_gee15_16_17_18_19,
            dt_gee20,
            by.x = 'geoid10_15',
            by.y = 'geoid10_20')

# change vars
dt_gee15_16_17_18_19_20$FIPS <- dt_gee15_16_17_18_19_20$geoid10_15
dt_gee15_16_17_18_19_20 <-
  dt_gee15_16_17_18_19_20 %>% dplyr::select(matches("FIPS|ndvi_mean"))
dt_gee15_16_17_18_19_20$NDVI_Sum <-
  (
    dt_gee15_16_17_18_19_20$ndvi_mean_15 + dt_gee15_16_17_18_19_20$ndvi_mean_16 +
      dt_gee15_16_17_18_19_20$ndvi_mean_17 + dt_gee15_16_17_18_19_20$ndvi_mean_18 +
      dt_gee15_16_17_18_19_20$ndvi_mean_19 + dt_gee15_16_17_18_19_20$ndvi_mean_20
  ) / 6

# merge
tract <-
  sp::merge(tract,
            dt_gee15_16_17_18_19_20,
            by.x = 'GEOID',
            by.y = 'FIPS')

# ----------------------------------------------------------------------%
# sociodemographic data for counties ------------------------------------
# ----------------------------------------------------------------------%

# file 1
county_d1 <-
  read_csv("../1_source_data/NHGIS/nhgis0050_ds244_20195_county.csv") %>%
  rename(
    NHGIS_State = STUSAB,
    Tot = ALT0E001,
    Female = ALT0E026,
    NHWhite = ALUKE003,
    NHBlack = ALUKE004,
    NHAsian = ALUKE006,
    Hisp = ALUKE012,
    EducTot = ALWGE001,
    EducHS_1 = ALWGE017,
    EducHS_2 = ALWGE018,
    EducHS_3 = ALWGE019,
    EducHS_4 = ALWGE020,
    EducHS_5 = ALWGE021,
    EducColl_1 = ALWGE022,
    EducColl_2 = ALWGE023,
    EducColl_3 = ALWGE024,
    EducColl_4 = ALWGE025,
    PovTot = ALWYE001,
    Pov = ALWYE002,
    MedHHInc = ALW1E001,
    UnempTot = ALY3E001,
    Unemp = ALY3E007,
    OldAgeM_1 = ALT0E020,
    OldAgeM_2 = ALT0E021,
    OldAgeM_3 = ALT0E022,
    OldAgeM_4 = ALT0E023,
    OldAgeM_5 = ALT0E024,
    OldAgeM_6 = ALT0E025,
    OldAgeF_1 = ALT0E044,
    OldAgeF_2 = ALT0E045,
    OldAgeF_3 = ALT0E046,
    OldAgeF_4 = ALT0E047,
    OldAgeF_5 = ALT0E048,
    OldAgeF_6 = ALT0E049,
    MedHHValue = AL1HE001
  ) %>%
  dplyr::select(
    matches(
      "NHGIS|GEOID|ISJOIN|Tot|Female|NHWhite|NHBlack|NHAsian|Hisp|Educ|Pov|Med|Unemp|OldAge"
    )
  )

county_d1 <- county_d1[-1,]

numvars <-
  c(
    'Tot',
    'Female' ,
    'NHWhite',
    'NHBlack',
    'NHAsian',
    'Hisp',
    'EducTot',
    'EducHS_1',
    'EducHS_2',
    'EducHS_3',
    'EducHS_4',
    'EducHS_5',
    'EducColl_1',
    'EducColl_2',
    'EducColl_3',
    'EducColl_4',
    'PovTot',
    'Pov',
    'MedHHInc',
    'UnempTot',
    'Unemp',
    'OldAgeM_1',
    'OldAgeM_2',
    'OldAgeM_3',
    'OldAgeM_4',
    'OldAgeM_5',
    'OldAgeM_6',
    'OldAgeF_1',
    'OldAgeF_2',
    'OldAgeF_3',
    'OldAgeF_4',
    'OldAgeF_5',
    'OldAgeF_6',
    'MedHHValue'
  )
county_d1[, numvars] <- lapply(county_d1[, numvars], as.numeric)

county_d1$PercFemale <- county_d1$Female / county_d1$Tot
county_d1$PercNHWhite <- county_d1$NHWhite / county_d1$Tot
county_d1$PercNHWhite <- county_d1$NHWhite / county_d1$Tot
county_d1$PercNHBlack <- county_d1$NHBlack / county_d1$Tot
county_d1$PercNHAsian <- county_d1$NHAsian / county_d1$Tot
county_d1$PercHisp <- county_d1$Hisp / county_d1$Tot
county_d1$PercEducHS <-
  (
    county_d1$EducHS_1 + county_d1$EducHS_2 + county_d1$EducHS_3 + county_d1$EducHS_4 +
      county_d1$EducHS_5 + county_d1$EducColl_1 + county_d1$EducColl_2 + county_d1$EducColl_3 +
      county_d1$EducColl_4
  ) / county_d1$EducTot
county_d1$PercEducColl <-
  (
    county_d1$EducColl_1 + county_d1$EducColl_2 + county_d1$EducColl_3 + county_d1$EducColl_4
  ) / county_d1$EducTot
county_d1$PercPov <- county_d1$Pov / county_d1$PovTot
county_d1$PercUnemp <- county_d1$Unemp / county_d1$UnempTot
county_d1$Perc65Up <-
  (
    county_d1$OldAgeM_1 + county_d1$OldAgeM_2 + county_d1$OldAgeM_3 + county_d1$OldAgeM_4 +
      county_d1$OldAgeM_5 + county_d1$OldAgeM_6 + county_d1$OldAgeF_1 + county_d1$OldAgeF_2 +
      county_d1$OldAgeF_3 + county_d1$OldAgeF_4 + county_d1$OldAgeF_5 + county_d1$OldAgeF_6
  ) / county_d1$Tot

county_d1 <-
  county_d1 %>% dplyr::select(matches("Tot|NHGIS|GISJOIN|GEOID|Perc|Med"))
county_d1 <- county_d1 %>% filter(!is.na(county_d1$MedHHInc))

county_d1$FIPS <- str_sub(county_d1$GEOID, -5, -1)

# file 2
county_d2 <-
  read_csv("../1_source_data/NHGIS/nhgis0050_ds245_20195_county.csv") %>%
  rename(
    NHGIS_State = STUSAB,
    Gini = AMEME001,
    EmpTot = AMHRE001,
    EmpNR = AMHRE002
  ) %>%
  dplyr::select(matches("NHGIS_State|GEOID|GISJOIN|Gini|EmpTot|EmpNR"))

county_d2 <- county_d2[-1,]

numvars <- c('Gini' , 'EmpTot', 'EmpNR')
county_d2[, numvars] <- lapply(county_d2[, numvars], as.numeric)

county_d2$PercEmpNR <- county_d2$EmpNR / county_d2$EmpTot

county_d2 <-
  county_d2 %>% dplyr::select(matches("GISJOIN|NHGIS|GEOID|Gini|Perc"))
county_d2 <- county_d2 %>% filter(!is.na(county_d2$PercEmpNR))

county_d2$FIPS <- str_sub(county_d2$GEOID, -5, -1)

# ----------------------------------------------------------------------%
# sociodemographic data for tracts --------------------------------------
# ----------------------------------------------------------------------%

# file 1
tract_d1 <-
  read_csv("../1_source_data/NHGIS/nhgis0049_ds244_20195_tract.csv") %>%
  rename(
    NHGIS_State = STUSAB,
    TotPop = ALT0E001,
    Female = ALT0E026,
    NHWhite = ALUKE003,
    NHBlack = ALUKE004,
    NHAsian = ALUKE006,
    Hisp = ALUKE012,
    EducTot = ALWGE001,
    EducHS_1 = ALWGE017,
    EducHS_2 = ALWGE018,
    EducHS_3 = ALWGE019,
    EducHS_4 = ALWGE020,
    EducHS_5 = ALWGE021,
    EducColl_1 = ALWGE022,
    EducColl_2 = ALWGE023,
    EducColl_3 = ALWGE024,
    EducColl_4 = ALWGE025,
    PovTot = ALWYE001,
    Pov = ALWYE002,
    MedHHInc = ALW1E001,
    UnempTot = ALY3E001,
    Unemp = ALY3E007,
    OldAgeM_1 = ALT0E020,
    OldAgeM_2 = ALT0E021,
    OldAgeM_3 = ALT0E022,
    OldAgeM_4 = ALT0E023,
    OldAgeM_5 = ALT0E024,
    OldAgeM_6 = ALT0E025,
    OldAgeF_1 = ALT0E044,
    OldAgeF_2 = ALT0E045,
    OldAgeF_3 = ALT0E046,
    OldAgeF_4 = ALT0E047,
    OldAgeF_5 = ALT0E048,
    OldAgeF_6 = ALT0E049
  ) %>%
  dplyr::select(
    matches(
      "NHGIS|GISJOIN|Tot|Female|NHWhite|NHBlack|NHAsian|Hisp|Educ|Pov|Med|Unemp|OldAge"
    )
  )

tract_d1 <- tract_d1[-1,]
numvars <-
  c(
    'TotPop',
    'Female' ,
    'NHWhite',
    'NHBlack',
    'NHAsian',
    'Hisp',
    'EducTot',
    'EducHS_1',
    'EducHS_2',
    'EducHS_3',
    'EducHS_4',
    'EducHS_5',
    'EducColl_1',
    'EducColl_2',
    'EducColl_3',
    'EducColl_4',
    'PovTot',
    'Pov',
    'MedHHInc',
    'UnempTot',
    'Unemp',
    'OldAgeM_1',
    'OldAgeM_2',
    'OldAgeM_3',
    'OldAgeM_4',
    'OldAgeM_5',
    'OldAgeM_6',
    'OldAgeF_1',
    'OldAgeF_2',
    'OldAgeF_3',
    'OldAgeF_4',
    'OldAgeF_5',
    'OldAgeF_6'
  )
tract_d1[, numvars] <- lapply(tract_d1[, numvars], as.numeric)
tract_d1$PercFemale <- tract_d1$Female / tract_d1$TotPop
tract_d1$PercNHWhite <- tract_d1$NHWhite / tract_d1$TotPop
tract_d1$PercNHWhite <- tract_d1$NHWhite / tract_d1$TotPop
tract_d1$PercNHBlack <- tract_d1$NHBlack / tract_d1$TotPop
tract_d1$PercNHAsian <- tract_d1$NHAsian / tract_d1$TotPop
tract_d1$PercHisp <- tract_d1$Hisp / tract_d1$TotPop
tract_d1$PercEducHS <-
  (
    tract_d1$EducHS_1 + tract_d1$EducHS_2 + tract_d1$EducHS_3 + tract_d1$EducHS_4 +
      tract_d1$EducHS_5 + tract_d1$EducColl_1 + tract_d1$EducColl_2 + tract_d1$EducColl_3 +
      tract_d1$EducColl_4
  ) / tract_d1$EducTot
tract_d1$PercEducColl <-
  (
    tract_d1$EducColl_1 + tract_d1$EducColl_2 + tract_d1$EducColl_3 + tract_d1$EducColl_4
  ) / tract_d1$EducTot
tract_d1$PercPov <- tract_d1$Pov / tract_d1$PovTot
tract_d1$PercUnemp <- tract_d1$Unemp / tract_d1$UnempTot
tract_d1$Perc65Up <-
  (
    tract_d1$OldAgeM_1 + tract_d1$OldAgeM_2 + tract_d1$OldAgeM_3 + tract_d1$OldAgeM_4 +
      tract_d1$OldAgeM_5 + tract_d1$OldAgeM_6 + tract_d1$OldAgeF_1 + tract_d1$OldAgeF_2 +
      tract_d1$OldAgeF_3 + tract_d1$OldAgeF_4 + tract_d1$OldAgeF_5 + tract_d1$OldAgeF_6
  ) / tract_d1$TotPop

tract_d1 <-
  tract_d1 %>% dplyr::select(matches("NHGIS|GISJOIN|Perc|MedHHInc|TotPop"))
tract_d1 <- tract_d1 %>% filter(!is.na(tract_d1$MedHHInc))

# file 2
tract_d2 <-
  read_csv("../1_source_data/NHGIS/nhgis0049_ds245_20195_tract.csv") %>%
  rename(
    NHGIS_State = STUSAB,
    Gini = AMEME001,
    EmpTot = AMHRE001,
    EmpNR = AMHRE002
  ) %>%
  dplyr::select(matches("NHGIS_State|GISJOIN|Gini|EmpTot|EmpNR"))

tract_d2 <- tract_d2[-1,]

numvars <- c('Gini' , 'EmpTot', 'EmpNR')
tract_d2[, numvars] <- lapply(tract_d2[, numvars], as.numeric)

tract_d2$PercEmpNR <- tract_d2$EmpNR / tract_d2$EmpTot

tract_d2 <-
  tract_d2 %>% dplyr::select(matches("GISJOIN|NHGIS|Gini|Perc"))

tract_d2 <- tract_d2 %>% filter(!is.na(tract_d2$PercEmpNR))

# file 3
tract_d3 <-
  read_csv("../1_source_data/NHGIS/nhgis0051_ds244_20195_tract.csv") %>%
  rename(NHGIS_State = STUSAB,
         MedHHValue = AL1HE001) %>%
  dplyr::select(matches("GISJOIN|MedHHValue|GEOID"))

tract_d3$GEOID_11 <- str_sub(tract_d3$GEOID, -11, -1)

tract_d3 <- tract_d3[-1,]

numvars <- c('MedHHValue')
tract_d3[, numvars] <- lapply(tract_d3[, numvars], as.numeric)

tract_d3 <- tract_d3 %>% filter(!is.na(tract_d3$MedHHValue))

# ----------------------------------------------------------------------%
# merge data ------------------------------------------------------------
# ----------------------------------------------------------------------%

# ---- counties ----

# first transform shapefile with tree and NDVI values into dataframe
county_df <- as.data.frame(county)

# merges
county_d3 <-
  sp::merge(county_d1, county_d2, by.x = 'GISJOIN', by.y = 'GISJOIN') # add demographics
county_d4 <-
  sp::merge(county_d3,
            county_df,
            by.x = 'FIPS.y',
            by.y = 'FIPS',
            all.y = F) # add shapefile dataframe
county_d6 <-
  sp::merge(county_d4, parkc, by.x = "FIPS.y", by.y = "GEOID") # add PAD-US-AR cover

# rename long var names that will be truncated by GIS
county_d7 <-
  county_d6 %>% dplyr::select (-c(EducTot, PovTot, UnempTot)) %>%
  rename(
    TotPop = Tot,
    MedHInc = MedHHInc,
    MedHVal = MedHHValue,
    PcFem = PercFemale,
    PcWhite = PercNHWhite,
    PcBlack = PercNHBlack,
    PcAsian = PercNHAsian,
    PcHisp = PercHisp,
    PcHighSc = PercEducHS,
    PcColl = PercEducColl,
    PcPov = PercPov,
    PcUnemp = PercUnemp,
    Pc65Up = Perc65Up,
    PcEmpNR = PercEmpNR,
    State = NHGIS_State.x,
    FIPS = FIPS.y,
    PcPark = pc_park,
    NDVI_S15 = ndvi_mean_15,
    NDVI_S16 = ndvi_mean_16,
    NDVI_S17 = ndvi_mean_17,
    NDVI_S18 = ndvi_mean_18,
    NDVI_S19 = ndvi_mean_19,
    NDVI_S20 = ndvi_mean_20,
    NDVI_Sum = NDVI_Sum,
    NDVI_Ann = NDVI_annual
  )

# calculate pop density
county_d7$PopDens <- county_d7$POP_SQMI * .3861

# add region
county_d8 <- county_d7
county_d8$Region <- sapply(county_d8$State,
                           function(x)
                             names(region.list)[grep(x, region.list)])
table(county_d8$Region, county_d8$State)

# reorder variables
county_d9 <-
  county_d8[, c(
    "FIPS",
    "State",
    "Region",
    "TotPop",
    "PopDens",
    "PcFem",
    "PcWhite",
    "PcBlack",
    "PcAsian",
    "PcHisp",
    "Pc65Up",
    "MedHInc",
    "MedHVal",
    "PcPov",
    "Gini",
    "PcUnemp",
    "PcEmpNR",
    "PcHighSc",
    "PcColl",
    "NDVI_Sum",
    "NDVI_Ann",
    "PcCanopy",
    "PcPark"
  )]

# make park and canopy cover on same scale as NDVI
county_d9$PcPark <- county_d9$PcPark / 100
county_d9$PcCanopy <- county_d9$PcCanopy / 100

# name final dataset
county_F <- county_d9

# separate into regions
county_NE <- county_F[county_F$Region == "Northeast",]
county_MW <- county_F[county_F$Region == "Midwest",]
county_SO <- county_F[county_F$Region == "South",]
county_WT <- county_F[county_F$Region == "West",]
county_NEMW <- rbind(county_NE, county_MW)

# can add back to spatial data for visualizations
county_complete <-
  sp::merge(county, county_F, by.x = 'FIPS', by.y = 'FIPS')
write_sf(county_complete, "../3_data/CountyValues.shp")
county_complete_NE <-
  county_complete[county_complete$Region == "Northeast",]
write_sf(county_complete_NE, "../3_data/CountyValuesNE.shp")
county_complete_MW <-
  county_complete[county_complete$Region == "Midwest",]
write_sf(county_complete_MW, "../3_data/CountyValuesMW.shp")
county_complete_SO <-
  county_complete[county_complete$Region == "South",]
write_sf(county_complete_SO, "../3_data/CountyValuesSO.shp")
county_complete_WT <-
  county_complete[county_complete$Region == "West",]
write_sf(county_complete_WT, "../3_data/CountyValuesWT.shp")

# ---- tracts ----

# first transform shapefile with tree and NDVI values into dataframe
tract_df <- as.data.frame(tract)

# merges
tract_d4 <-
  sp::merge(tract_d1, tract_d2, by.x = 'GISJOIN', by.y = 'GISJOIN') # add demographics
tract_d5 <-
  sp::merge(tract_d3, tract_d4, by.x = 'GISJOIN', by.y = 'GISJOIN') # add more demographics
tract_d6 <-
  sp::merge(tract_d5, tract, by.x = 'GEOID_11', by.y = 'GEOID') # add tree and NDVI values
tract_d7 <-
  sp::merge(tract_d6, parkt, by.x = 'GEOID_11', by.y = "GEOID") # add PAD-US-AR cover

# rename long var names that will be truncated by GIS
tract_d8 <-
  tract_d7 %>%
  rename(
    GISJOIN = GISJOIN.x,
    MedHInc = MedHHInc,
    MedHVal = MedHHValue,
    PcFem = PercFemale,
    PcWhite = PercNHWhite,
    PcBlack = PercNHBlack,
    PcAsian = PercNHAsian,
    PcHisp = PercHisp,
    PcHighSc = PercEducHS,
    PcColl = PercEducColl,
    PcPov = PercPov,
    PcUnemp = PercUnemp,
    Pc65Up = Perc65Up,
    PcEmpNR = PercEmpNR,
    State = NHGIS_State.x,
    PcPark = pc_park,
    NDVI_S15 = ndvi_mean_15,
    NDVI_S16 = ndvi_mean_16,
    NDVI_S17 = ndvi_mean_17,
    NDVI_S18 = ndvi_mean_18,
    NDVI_S19 = ndvi_mean_19,
    NDVI_S20 = ndvi_mean_20,
    NDVI_Sum = NDVI_Sum,
    NDVI_Ann = NDVI_annual
  )

# calculate pop density
tract_d8$PopDens <- tract_d8$TotPop / tract_d8$LandAreakm2

# add region
tract_d8$Region <- sapply(tract_d8$State,
                          function(x)
                            names(region.list)[grep(x, region.list)])
table(tract_d8$Region, tract_d8$State)

# reorder variables
tract_d9 <-
  tract_d8[, c(
    "GEOID",
    "GEOID_11",
    "GISJOIN",
    "State",
    "Region",
    "TotPop",
    "PopDens",
    "PcFem",
    "PcWhite",
    "PcBlack",
    "PcAsian",
    "PcHisp",
    "Pc65Up",
    "MedHInc",
    "MedHVal",
    "PcPov",
    "Gini",
    "PcUnemp",
    "PcEmpNR",
    "PcHighSc",
    "PcColl",
    "NDVI_Sum",
    "NDVI_Ann",
    "PcCanopy",
    "PcPark"
  )]

# make park cover on same scale as NDVI and canopy cover
tract_d9$PcPark <- tract_d9$PcPark / 100

# name final dataset
tract_F <- tract_d9

# separate into regions
tract_NE <- tract_F[tract_F$Region == "Northeast",]
tract_MW <- tract_F[tract_F$Region == "Midwest",]
tract_SO <- tract_F[tract_F$Region == "South",]
tract_WT <- tract_F[tract_F$Region == "West",]

# can add back to spatial data for visualizations
tract_complete <-
  sp::merge(tract, tract_F, by.x = 'GEOID', by.y = 'GEOID_11')
write_sf(tract_complete, "../3_data/TractValues.shp")
tract_complete_NE <-
  tract_complete[tract_complete$Region == "Northeast",]
write_sf(tract_complete_NE, "../3_data/TractValuesNE.shp")
tract_complete_MW <-
  tract_complete[tract_complete$Region == "Midwest",]
write_sf(tract_complete_MW, "../3_data/TractValuesMW.shp")
tract_complete_SO <-
  tract_complete[tract_complete$Region == "South",]
write_sf(tract_complete_SO, "../3_data/TractValuesSO.shp")
tract_complete_WT <-
  tract_complete[tract_complete$Region == "West",]
write_sf(tract_complete_WT, "../3_data/TractValuesWT.shp")

# ----------------------------------------------------------------------%
# comparing nature metrics ----------------------------------------------
# ----------------------------------------------------------------------%

# ----- desc stats -----

library(dplyr)
#nationwide
county_F %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer",
        show = c("n", "md", "iqr", "range", "miss"))
tract_F %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

#Northeast
county_NE %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_NE %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

#Midwest
county_MW %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_MW %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

#South
county_SO %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_SO %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

#West
county_WT %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_WT %>% dplyr::select(PcPark, PcCanopy, NDVI_Ann, NDVI_Sum) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ----- Create ggplot histogram theme -----

hist_theme <- theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  ggeasy::easy_center_title() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# ----- PcPark histograms -----

hist_PcPark_c <- ggplot(county_F, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide counties") +
  xlim(0, 1) + hist_theme
hist_PcPark_c_NE <- ggplot(county_NE, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern counties") +
  xlim(0, 1) + hist_theme
hist_PcPark_c_MW <- ggplot(county_MW, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern counties") +
  xlim(0, 1) + hist_theme
hist_PcPark_c_SO <- ggplot(county_SO, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern counties") +
  xlim(0, 1) + hist_theme
hist_PcPark_c_WT <- ggplot(county_WT, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western counties") +
  xlim(0, 1) + hist_theme
hist_PcPark_t <- ggplot(tract_F, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide tracts") +
  xlim(0, 1) + hist_theme
hist_PcPark_t_NE <- ggplot(tract_NE, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern tracts") +
  xlim(0, 1) + hist_theme
hist_PcPark_t_MW <- ggplot(tract_MW, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern tracts") +
  xlim(0, 1) + hist_theme
hist_PcPark_t_SO <- ggplot(tract_SO, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern tracts") +
  xlim(0, 1) + hist_theme
hist_PcPark_t_WT <- ggplot(tract_WT, aes(PcPark)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western tracts") +
  xlim(0, 1) + hist_theme

hist_PcPark_all <- grid.arrange(
  hist_PcPark_c,
  hist_PcPark_t,
  hist_PcPark_c_NE,
  hist_PcPark_t_NE,
  hist_PcPark_c_MW,
  hist_PcPark_t_MW,
  hist_PcPark_c_SO,
  hist_PcPark_t_SO,
  hist_PcPark_c_WT,
  hist_PcPark_t_WT,
  nrow = 5
)

ggsave(
  "../PcPark_Hist.png",
  plot = hist_PcPark_all,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ----- NDVI_Ann histograms -----

hist_NDVI_Ann_c <- ggplot(county_F, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_c_NE <- ggplot(county_NE, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_c_MW <- ggplot(county_MW, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_c_SO <- ggplot(county_SO, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_c_WT <- ggplot(county_WT, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_t <- ggplot(tract_F, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_t_NE <- ggplot(tract_NE, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_t_MW <- ggplot(tract_MW, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_t_SO <- ggplot(tract_SO, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Ann_t_WT <- ggplot(tract_WT, aes(NDVI_Ann)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western tracts") +
  xlim(0, 1) + hist_theme

hist_NDVI_Ann_all <- grid.arrange(
  hist_NDVI_Ann_c,
  hist_NDVI_Ann_t,
  hist_NDVI_Ann_c_NE,
  hist_NDVI_Ann_t_NE,
  hist_NDVI_Ann_c_MW,
  hist_NDVI_Ann_t_MW,
  hist_NDVI_Ann_c_SO,
  hist_NDVI_Ann_t_SO,
  hist_NDVI_Ann_c_WT,
  hist_NDVI_Ann_t_WT,
  nrow = 5
)

ggsave(
  "../NDVI_Ann_Hist.png",
  plot = hist_NDVI_Ann_all,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ----- NDVI_Sum histograms -----

hist_NDVI_Sum_c <- ggplot(county_F, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_c_NE <- ggplot(county_NE, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_c_MW <- ggplot(county_MW, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_c_SO <- ggplot(county_SO, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_c_WT <- ggplot(county_WT, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western counties") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_t <- ggplot(tract_F, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_t_NE <- ggplot(tract_NE, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_t_MW <- ggplot(tract_MW, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_t_SO <- ggplot(tract_SO, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern tracts") +
  xlim(0, 1) + hist_theme
hist_NDVI_Sum_t_WT <- ggplot(tract_WT, aes(NDVI_Sum)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western tracts") +
  xlim(0, 1) + hist_theme

hist_NDVI_Sum_all <- grid.arrange(
  hist_NDVI_Sum_c,
  hist_NDVI_Sum_t,
  hist_NDVI_Sum_c_NE,
  hist_NDVI_Sum_t_NE,
  hist_NDVI_Sum_c_MW,
  hist_NDVI_Sum_t_MW,
  hist_NDVI_Sum_c_SO,
  hist_NDVI_Sum_t_SO,
  hist_NDVI_Sum_c_WT,
  hist_NDVI_Sum_t_WT,
  nrow = 5
)

ggsave(
  "../NDVI_Sum_Hist.png",
  plot = hist_NDVI_Sum_all,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ----- PcCanopy histograms -----

hist_PcCanopy_c <- ggplot(county_F, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide counties") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_c_NE <- ggplot(county_NE, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern counties") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_c_MW <- ggplot(county_MW, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern counties") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_c_SO <- ggplot(county_SO, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern counties") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_c_WT <- ggplot(county_WT, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western counties") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_t <- ggplot(tract_F, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#3B6AA8") + ggtitle("Nationwide tracts") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_t_NE <- ggplot(tract_NE, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#32A229") + ggtitle("Northeastern tracts") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_t_MW <- ggplot(tract_MW, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#EDA247") + ggtitle("Midwestern tracts") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_t_SO <- ggplot(tract_SO, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#DB4325") + ggtitle("Southern tracts") +
  xlim(0, 1) + hist_theme
hist_PcCanopy_t_WT <- ggplot(tract_WT, aes(PcCanopy)) +
  geom_histogram(color = NA, fill = "#F8DE27") + ggtitle("Western tracts") +
  xlim(0, 1) + hist_theme

hist_PcCanopy_all <- grid.arrange(
  hist_PcCanopy_c,
  hist_PcCanopy_t,
  hist_PcCanopy_c_NE,
  hist_PcCanopy_t_NE,
  hist_PcCanopy_c_MW,
  hist_PcCanopy_t_MW,
  hist_PcCanopy_c_SO,
  hist_PcCanopy_t_SO,
  hist_PcCanopy_c_WT,
  hist_PcCanopy_t_WT,
  nrow = 5
)

ggsave(
  "../PcCanopy_Hist.png",
  plot = hist_PcCanopy_all,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ----- correlations -----

county_Fn <-
  county_F %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
county_NEn <-
  county_NE %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
county_MWn <-
  county_MW %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
county_SOn <-
  county_SO %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
county_WTn <-
  county_WT %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
tract_Fn <-
  tract_F %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
tract_NEn <-
  tract_NE %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
tract_MWn <-
  tract_MW %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
tract_SOn <-
  tract_SO %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)
tract_WTn <-
  tract_WT %>% dplyr::select(PcPark, NDVI_Ann, NDVI_Sum, PcCanopy)

correlation::correlation(county_Fn, decimals = 2)
correlation::correlation(county_NEn, decimals = 2)
correlation::correlation(county_MWn, decimals = 2)
correlation::correlation(county_SOn, decimals = 2)
correlation::correlation(county_WTn, decimals = 2)
correlation::correlation(tract_Fn, decimals = 2)
correlation::correlation(tract_NEn, decimals = 2)
correlation::correlation(tract_MWn, decimals = 2)
correlation::correlation(tract_SOn, decimals = 2)
correlation::correlation(tract_WTn, decimals = 2)

# ----------------------------------------------------------------------%
# sociodemographic desc stats ------------------------------------------
# ----------------------------------------------------------------------%

# ----- nationwide -----

county_F %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_F %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ----- Northeast -----
county_NE %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_NE %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ----- Midwest -----
county_MW %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_MW %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ----- South -----
county_SO %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_SO %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ----- West -----
county_WT %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))
tract_WT %>% dplyr::select(
  PopDens,
  MedHInc,
  MedHVal,
  PcPov,
  Gini,
  PcHighSc,
  PcColl,
  PcUnemp,
  PcEmpNR,
  PcBlack,
  PcAsian,
  PcHisp,
  Pc65Up,
  PcFem,
  TotPop
) %>%
  descr(out = "viewer", show = c("n", "md", "iqr", "range"))

# ------------------------------------------------------%
# regressions ------------------------------------------
# ------------------------------------------------------%

# specify vif code
vif.lme <- function (fit) {
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ns <- sum(1 *
              (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v) ^ 0.5
  v <- diag(solve(v / (d %o% d)))
  names(v) <- nam
  v
}

# ----- main model -----
glm <-
  PcPark ~ PopDens + MedHVal + PcPov + Gini + PcHighSc + PcColl + PcUnemp + PcEmpNR +
  PcBlack + PcAsian + PcHisp + Pc65Up + PcFem + TotPop + (1 | State)

glm_Fc <- lme4::lmer(glm, data = county_F, REML = F)
glm_Ft <- lme4::lmer(glm, data = tract_F, REML = F)
glm_NEc <- lme4::lmer(glm, data = county_NE, REML = F)
glm_NEt <- lme4::lmer(glm, data = tract_NE, REML = F)
glm_MWc <- lme4::lmer(glm, data = county_MW, REML = F)
glm_MWt <- lme4::lmer(glm, data = tract_MW, REML = F)
glm_SOc <- lme4::lmer(glm, data = county_SO, REML = F)
glm_SOt <- lme4::lmer(glm, data = tract_SO, REML = F)
glm_WTc <- lme4::lmer(glm, data = county_WT, REML = F)
glm_WTt <- lme4::lmer(glm, data = tract_WT, REML = F)

# residuals
par(mfrow = c(2, 2))
plot(glm_Fc)
plot(glm_Ft)

# VIF
vif.lme(glm_Fc)
vif.lme(glm_Ft)

# report results
sjPlot::tab_model(
  glm_Fc,
  glm_Ft,
  glm_NEc,
  glm_NEt,
  glm_MWc,
  glm_MWt,
  glm_SOc,
  glm_SOt,
  glm_WTc,
  glm_WTt,
  dv.labels = c("Counties", "Tracts"),
  show.intercept = F,
  show.est = F,
  show.se = F,
  show.ci = F,
  show.std	= T,
  show.p = T,
  show.r2 = T,
  show.re.var = T,
  show.fstat = T,
  show.aic = T
)

# ----- alternative model -----
glm2 <-
  PcPark ~ PopDens + MedHInc + PcBlack + PcAsian + PcHisp + Pc65Up + PcFem + TotPop +
  (1 | State)

glm2_Fc <- lme4::lmer(glm2, data = county_F, REML = F)
glm2_Ft <- lme4::lmer(glm2, data = tract_F, REML = F)
glm2_NEc <- lme4::lmer(glm2, data = county_NE, REML = F)
glm2_NEt <- lme4::lmer(glm2, data = tract_NE, REML = F)
glm2_MWc <- lme4::lmer(glm2, data = county_MW, REML = F)
glm2_MWt <- lme4::lmer(glm2, data = tract_MW, REML = F)
glm2_SOc <- lme4::lmer(glm2, data = county_SO, REML = F)
glm2_SOt <- lme4::lmer(glm2, data = tract_SO, REML = F)
glm2_WTc <- lme4::lmer(glm2, data = county_WT, REML = F)
glm2_WTt <- lme4::lmer(glm2, data = tract_WT, REML = F)

# residuals
par(mfrow = c(2, 2))
plot(glm2_Fc)
plot(glm2_Ft)

# VIF
vif.lme(glm2_Fc)
vif.lme(glm2_Ft)

# report results
sjPlot::tab_model(
  glm2_Fc,
  glm2_Ft,
  glm2_NEc,
  glm2_NEt,
  glm2_MWc,
  glm2_MWt,
  glm2_SOc,
  glm2_SOt,
  glm2_WTc,
  glm2_WTt,
  dv.labels = c("Counties", "Tracts"),
  show.intercept = F,
  show.est = F,
  show.se = F,
  show.ci = F,
  show.std	= T,
  show.p = T,
  show.r2 = T,
  show.re.var = T,
  show.fstat = T,
  show.aic = T
)

# ----- urban model -----

# Testing population density cut-points
nrow(county_F[(county_F$PopDens >= 1000),]) # 45 TOO small for nationwide analyses

nrow(county_NE[(county_NE$PopDens >= 300),]) # 43 TOO small for Northeastern analyses
nrow(county_MW[(county_MW$PopDens >= 300),]) # 30 TOO small for Midwestern analyses
nrow(county_SO[(county_SO$PopDens >= 300),]) # 93 small for Southern analyses
nrow(county_WT[(county_WT$PopDens >= 300),]) # 16 TOO small for Western analyses

nrow(county_NE[(county_NE$PopDens >= 50),]) # 121 Northeastern counties
nrow(county_MW[(county_MW$PopDens >= 50),]) # 178 Midwestern counties
nrow(county_SO[(county_SO$PopDens >= 50),]) # 386 Southern counties
nrow(county_WT[(county_WT$PopDens >= 50),]) # 58 Western counties

# Limit counties and tracts
county_Fu <- county_F[(county_F$PopDens >= 50),]
county_NEu <- county_NE[(county_NE$PopDens >= 50),]
county_MWu <- county_MW[(county_MW$PopDens >= 50),]
county_SOu <- county_SO[(county_SO$PopDens >= 50),]
county_WTu <- county_WT[(county_WT$PopDens >= 50),]
tract_Fu <- tract_F[(tract_F$PopDens >= 1000),]
tract_NEu <- tract_NE[(tract_NE$PopDens >= 1000),]
tract_MWu <- tract_MW[(tract_MW$PopDens >= 1000),]
tract_SOu <- tract_SO[(tract_SO$PopDens >= 1000),]
tract_WTu <- tract_WT[(tract_WT$PopDens >= 1000),]

glm_Fcu <- lme4::lmer(glm, data = county_Fu, REML = F)
glm_Ftu <- lme4::lmer(glm, data = tract_Fu, REML = F)
glm_NEcu <- lme4::lmer(glm, data = county_NEu, REML = F)
glm_NEtu <- lme4::lmer(glm, data = tract_NEu, REML = F)
glm_MWcu <- lm(
  PcPark ~ PopDens + MedHVal + PcPov + Gini + PcHighSc + PcColl + PcUnemp + PcEmpNR +
    PcBlack + PcAsian + PcHisp + Pc65Up + PcFem + TotPop,
  data = county_MWu
) # small Ns in several Midwestern states required OLS rather than mixed models
glm_MWtu <- lme4::lmer(glm, data = tract_MWu, REML = F)
glm_SOcu <- lme4::lmer(glm, data = county_SOu, REML = F)
glm_SOtu <- lme4::lmer(glm, data = tract_SOu, REML = F)
glm_WTcu <- lme4::lmer(glm, data = county_WTu, REML = F)
glm_WTtu <- lme4::lmer(glm, data = tract_WTu, REML = F)

# report results
sjPlot::tab_model(
  glm_Fcu,
  glm_Ftu,
  glm_NEcu,
  glm_NEtu,
  glm_MWcu,
  glm_MWtu,
  glm_SOcu,
  glm_SOtu,
  glm_WTcu,
  glm_WTtu,
  dv.labels = c("Counties", "Tracts"),
  show.intercept = F,
  show.est = F,
  show.se = F,
  show.ci = F,
  show.std	= T,
  show.p = T,
  show.r2 = T,
  show.re.var = T,
  show.fstat = T,
  show.aic = T
)

# ----- Create ggplot theme for regression models -----
glm_theme <- theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18, face = "bold")
  )
theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black")
)

# ----- model plots for counties -----

glm_plots_counties <- plot_models(
  glm_WTc,
  glm_SOc,
  glm_MWc,
  glm_NEc,
  glm_Fc,
  std.est = TRUE,
  p.shape = TRUE,
  axis.labels = c(
    "Total population",
    "% female",
    "% 65+ years",
    "% Hispanic",
    "%NH Asian",
    "% NH Black",
    "% employed natural resources",
    "% unemployed",
    "% college degree",
    "% high school degree",
    "Gini index",
    "% poverty",
    "Median home value",
    "Population density"
  ),
  legend.title = "Regions",
  m.labels = c("West",
               "South",
               "Midwest",
               "Northeast",
               "Nationwide"),
  spacing = .6,
  dot.size = 3,
  line.size = .8,
  colors = c("#3B6AA8",
             "#32A229",
             "#EDA247",
             "#DB4325",
             "#F8DE27"),
  show.legend = T,
  grid = F
) +
  glm_theme +
  geom_hline(
    yintercept = 0,
    size = 1,
    linetype = "dashed",
    color = "black"
  ) +
  ylim(-.8, .8)

ggsave(
  "../glm_plots_counties.png",
  plot = glm_plots_counties,
  width = 8,
  height = 12,
  units = "in",
  dpi = 300
)

# ----- model plots for tracts -----

glm_plots_tracts <- plot_models(
  glm_WTt,
  glm_SOt,
  glm_MWt,
  glm_NEt,
  glm_Ft,
  std.est = TRUE,
  p.shape = TRUE,
  axis.labels = c(
    "Total population",
    "% female",
    "% 65+ years",
    "% Hispanic",
    "%NH Asian",
    "% NH Black",
    "% employed natural resources",
    "% unemployed",
    "% college degree",
    "% high school degree",
    "Gini index",
    "% poverty",
    "Median home value",
    "Population density"
  ),
  legend.title = "Regions",
  m.labels = c("West",
               "South",
               "Midwest",
               "Northeast",
               "Nationwide"),
  spacing = .6,
  dot.size = 3,
  line.size = .8,
  colors = c("#3B6AA8",
             "#32A229",
             "#EDA247",
             "#DB4325",
             "#F8DE27"),
  show.legend = T,
  grid = F
) +
  glm_theme +
  geom_hline(
    yintercept = 0,
    size = 1,
    linetype = "dashed",
    color = "black"
  ) +
  ylim(-.8, .8)

ggsave(
  "../glm_plots_tracts.png",
  plot = glm_plots_tracts,
  width = 8,
  height = 12,
  units = "in",
  dpi = 300
)

# ----- model plots for counties with income -----

glm2_plots_counties <- plot_models(
  glm2_WTc,
  glm2_SOc,
  glm2_MWc,
  glm2_NEc,
  glm2_Fc,
  std.est = TRUE,
  p.shape = TRUE,
  axis.labels = c(
    "Total population",
    "% female",
    "% 65+ years",
    "% Hispanic",
    "%NH Asian",
    "% NH Black",
    "Median household income",
    "Population density"
  ),
  legend.title = "Regions",
  m.labels = c("West",
               "South",
               "Midwest",
               "Northeast",
               "Nationwide"),
  spacing = .6,
  dot.size = 3,
  line.size = .8,
  colors = c("#3B6AA8",
             "#32A229",
             "#EDA247",
             "#DB4325",
             "#F8DE27"),
  show.legend = T,
  grid = F
) +
  glm_theme +
  geom_hline(
    yintercept = 0,
    size = 1,
    linetype = "dashed",
    color = "black"
  ) +
  ylim(-.8, .8)

ggsave(
  "../glm2_plots_counties_income.png",
  plot = glm2_plots_counties,
  width = 8,
  height = 10,
  units = "in",
  dpi = 300
)

# ----- model plots for tracts with income -----

glm2_plots_tracts <- plot_models(
  glm2_WTt,
  glm2_SOt,
  glm2_MWt,
  glm2_NEt,
  glm2_Ft,
  std.est = TRUE,
  p.shape = TRUE,
  axis.labels = c(
    "Total population",
    "% female",
    "% 65+ years",
    "% Hispanic",
    "%NH Asian",
    "% NH Black",
    "Median household income",
    "Population density"
  ),
  legend.title = "Regions",
  m.labels = c("West",
               "South",
               "Midwest",
               "Northeast",
               "Nationwide"),
  spacing = .6,
  dot.size = 3,
  line.size = .8,
  colors = c("#3B6AA8",
             "#32A229",
             "#EDA247",
             "#DB4325",
             "#F8DE27"),
  show.legend = T,
  grid = F
) +
  glm_theme +
  geom_hline(
    yintercept = 0,
    size = 1,
    linetype = "dashed",
    color = "black"
  ) +
  ylim(-.8, .8)

ggsave(
  "../glm2_plots_tracts_income.png",
  plot = glm2_plots_tracts,
  width = 8,
  height = 10,
  units = "in",
  dpi = 300
)

# ------------------------------------------------------------------------%
# Complete ----------------------------------------------------------------
# ------------------------------------------------------------------------%

beep(3) # fun sound for completion