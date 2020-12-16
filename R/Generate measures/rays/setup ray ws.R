library(sf)
library(dplyr)
library(mapview)
library(lwgeom)
rm(list=ls())
# grab helper fcns --------------------------------------------------------
# will probably put all these into divFcns pckg later
#source("places & rays/places & rays fcns.R")
devtools::load_all()

czs <- divDat::czs %>% divM::region.reorg("cz")

plc <- divM::largest.plc.in.cz

shp.dir <- "~/R/shapefiles/"
hwys <- st_read(paste0(shp.dir
                       ,"National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))
hwys <- hwys %>%
  select(c(div.id = 1, div.name = LNAME,
           county = CTFIPS,
           SOURCE,  # data source
           F_SYSTEM, FCLASS, # addl hwy classification
           LRSKEY, # Uniquely identifies a route within a state
           SIGNT1, SIGNN1, SIGN1,
           MILES, KM, state = STFIPS, geometry))




# set identical metered crs -----------------------------------------------
czs <- czs %>% divM::conic.transform()
plc <- plc %>% divM::conic.transform()
hwys <- hwys %>% divM::conic.transform()

# build name-geoid place index --------------------------------------------
# plc <- filter(plc , STATEFP  == 42) %>% # (for test state)
plc.ids <- plc$geoid
names(plc.ids) <- plc$name


# save WS ----------------------------------------------------------------------

# .Rdata for portability -- relies on local NHPN data anyway
save.image(
  here::here("R/Generate measures/rays/ray ws.Rdata")
  )
