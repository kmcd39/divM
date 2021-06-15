# get data & ws and fcns --------------------------------------------------
rm(list=ls())
library(sf)
library(tidyverse)
library(mapview)
library(lwgeom)
devtools::load_all()

load(here::here("R/Generate-measures/ray-ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(czs) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

czs <- tibble(czs) %>% geoseg::region.reorg("cz") %>% st_sf()

czs <- czs %>% rename(region.name = cz_name)
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

# test run ----------------------------------------------------------------
tmp <-
  Polys.wrapper(
    czs %>% filter(grepl("Philadelphia", region.name))
    , lac
    , always.include = NULL
    , fill.gaps = T
    , return.sf = T )

tmp %>% mapview()


# wrapped fcn with options for variations: ---------------------------------
?Polys.wrapper

# map thru & generate measures --------------------------------------------

# limited-access, gaps filled
lac.polys <- map_dfr(lac.eligible
                     , ~Polys.wrapper(czs[czs$region.id == ., ]
                                      , lac
                                      , always.include = NULL))


# just interstates, gaps filled
int.polys <- map_dfr(int.eligible
                     , ~Polys.wrapper(czs[czs$region.id == ., ]
                                      , hwys
                                      , always.include = c("I")))


# write ------------------------------------------------------------------------


write.csv(lac.polys,
          "dividedness-measures/CZs/polys/lac-gapfilled-hwy-polys.csv" )

write.csv(int.polys,
          "dividedness-measures/interstates-gapfilled-hwy-polys.csv" )
