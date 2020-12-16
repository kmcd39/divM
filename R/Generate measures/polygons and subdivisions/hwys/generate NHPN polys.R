# get data & ws and fcns --------------------------------------------------
rm(list=ls())
library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
devtools::load_all(export_all = F)

# get czs
czs <- divDat::czs %>% divM::region.reorg("cz")

# downloaded from
# https://catalog.data.gov/dataset/national-highway-planning-network-nhpn
shp.dir <- "~/R/shapefiles/"
hwys <- st_read(paste0(shp.dir, "National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))
hwys <- hwys %>%
  select(c(div.id = 1, div.name = LNAME,
           county = CTFIPS,
           SOURCE,  # data source
           F_SYSTEM, FCLASS, # addl hwy classification
           LRSKEY, # Uniquely identifies a route within a state
           SIGNT1, SIGNN1, SIGN1,
           MILES, KM, state = STFIPS, geometry))

# recode i80 bus route (I80 in name column but SIGN columns blank)
# This includes the bus route with interstates
hwys[grepl("I[0-9]+", hwys$div.name),]$SIGNT1 = "I"
hwys[grepl("I[0-9]+", hwys$div.name),]$SIGN1 = "I80 (bus route)"
hwys[grepl("I[0-9]+", hwys$div.name),]


# make uniform metered crs -----------------------------------------------
czs <- czs %>% divM::conic.transform()
hwys <- hwys %>% divM::conic.transform()

# Limited-access proxy ---------------------------------------
lac <- hwys %>% filter(FCLASS %in% divM::lac_codes |
                         SIGNT1 == "I")

# drop others that are still NA for SIGNT
lac <- lac %>% filter(!is.na(SIGNT1))

# get subset of CZs that have some intersection w/ interstates & lacs ------------
sbgp <- st_intersects(czs, filter(hwys, SIGNT1 == "I"))
int.eligible <- czs$region.id[lengths(sbgp) > 0]

sbgp <- st_intersects(czs, lac)
lac.eligible <- czs$region.id[lengths(sbgp) > 0]

# map thru & generate measures --------------------------------------------



# limited-access, gaps filled
lac.polys <- map_dfr(lac.eligible
                     , ~Polys.wrapper(czs[czs$region.id == ., ]
                                      , lac
                                      , always.include = NULL))


# just interstates, gaps filled
int.polys <- map_dfr(hwy.eligible
                     , ~Polys.wrapper(czs[czs$region.id == ., ]
                                      , hwys
                                      , always.include = c("I")))

write.csv(lac.polys, "intermediate saves/polys/lac-gapfilled v2.csv" )

write.csv(int.polys, "intermediate saves/polys/interstates-gapfilled v2.csv" )
