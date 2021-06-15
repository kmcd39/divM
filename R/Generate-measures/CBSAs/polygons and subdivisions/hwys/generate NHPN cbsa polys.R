# get data & ws and fcns --------------------------------------------------
rm(list=ls())
library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)

devtools::load_all()
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(nhpn) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(cbsas) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

# divm org
cbsas <- tibble(cbsas) %>% geoseg::region.reorg("cbsa") %>% st_sf()
cbsas <- cbsas %>% rename(region.name = cbsa_name)

# Limited-access proxy ---------------------------------------
lac <- nhpn %>% filter(FCLASS %in% divM::lac_codes |
                         SIGNT1 == "I")

# drop others that are still NA for SIGNT
lac <- lac %>% filter(!is.na(SIGNT1))

# get subset of CBSAs that have some intersection w/ interstates & lacs ------------

sbgp <- st_intersects(cbsas, filter(nhpn, SIGNT1 == "I"))
int.eligible <- cbsas$region.id[lengths(sbgp) > 0]

sbgp <- st_intersects(cbsas, lac)
lac.eligible <- cbsas$region.id[lengths(sbgp) > 0]

# test run ----------------------------------------------------------------
tmp <-
  Polys.wrapper(
    cbsas %>% filter(grepl("Philadelphia", region.name))
    , lac
    , always.include = NULL
    , fill.gaps = T
    , return.sf = T )

tmp %>% mapview()


# wrapped fcn with options for variations: ---------------------------------
?Polys.wrapper
Polys.wrapper(cbsas[1, ]
              , lac
              , always.include = NULL)

# map thru & generate measures --------------------------------------------

lac.eligible
# limited-access, gaps filled
lac.polys <- map_dfr(lac.eligible
                     , ~Polys.wrapper(cbsas[cbsas$region.id == ., ]
                                      , lac
                                      , always.include = NULL))


# just interstates, gaps filled
int.polys <- map_dfr(int.eligible
                     , ~Polys.wrapper(cbsas[cbsas$region.id == ., ]
                                      , nhpn
                                      , always.include = c("I")))


# write ------------------------------------------------------------------------

write.csv(lac.polys,
          "dividedness-measures/CBSAs/polys/lac-gapfilled-hwy-polys.csv" )

write.csv(int.polys,
          "dividedness-measures/CBSAs/interstates-gapfilled-hwy-polys.csv" )
