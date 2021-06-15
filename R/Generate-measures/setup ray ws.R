library(sf)
library(tidyverse)
library(mapview)
library(lwgeom)
rm(list=ls())

# grab helper fcns --------------------------------------------------------
devtools::load_all()

# form czs from counties  --------------------------------------------------------
counties <- tigris::counties( year = 2019)

counties <- counties %>%
  left_join(xwalks::co2cz,
            by = c("GEOID" = "countyfp",
                   "STATEFP" = "statefp"))

czs <- counties %>%
  divM::conic.transform() %>%
  group_by(cz, cz_name) %>%
  summarise(., do_union = T)

(czs <- czs %>% filter(!is.na(cz)))
# plcs w/ pkg -------------------------------------------------------------
plc <- divM::largest.plc.in.cz

plc <- plc %>% filter(!is.na(cz.id))


# cbsas from tigris -----------------------------------------------------

cbsas <- tigris::core_based_statistical_areas(year = 2019)
cbsas <- cbsas %>%
  select(cbsa = GEOID, cbsa_name = NAME,
         lsad=LSAD) %>%
  divM::conic.transform()

# hwys from local ---------------------------------------------------------

shp.dir <- #"~/R/shapefiles/"
   "/scratch/gpfs/km31/other-local-data/" #

nhpn <- st_read(paste0(shp.dir
                       ,"National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))
nhpn <- nhpn %>%
  select(c(div.id = 1, div.name = LNAME,
           county = CTFIPS,
           SOURCE,  # data source
           F_SYSTEM, FCLASS, # addl hwy classification
           LRSKEY, # Uniquely identifies a route within a state
           SIGNT1, SIGNN1, SIGN1,
           MILES, KM, state = STFIPS, geometry))

# recode the annoying i80 bus route
nhpn[grepl("I[0-9]+", nhpn$div.name),]$SIGNT1 = "I"
nhpn[grepl("I[0-9]+", nhpn$div.name),]$SIGN1 = "I80 (bus route)"

# set identical metered crs -----------------------------------------------
czs <- czs %>% divM::conic.transform()
plc <- plc %>% st_sf() %>%  divM::conic.transform()
cbsas <- cbsas %>% divM::conic.transform()
nhpn <- nhpn %>% divM::conic.transform()

# build name-geoid place index --------------------------------------------
# plc <- filter(plc , STATEFP  == 42) %>% # (for test state)

plc.ids <- plc$plc.id
names(plc.ids) <- plc$plc.name

# duplicate some colms for expected names
plc$geoid <- plc$plc.id
plc$name <- plc$plc.name

# save WS ----------------------------------------------------------------------

# .Rdata for portability -- relies on local NHPN data anyway
save.image(
  here::here("R/Generate-measures/ray-ws.Rdata")
  )
