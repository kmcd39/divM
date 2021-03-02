library(sf)
library(tidyverse)
library(mapview)
library(lwgeom)
rm(list=ls())

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

czs


# get plcs ----------------------------------------------------------------


# get NHPN hwy shpfile
shp.dir <- "~/R/shapefiles/"
nhpn <- st_read(paste0(shp.dir, "National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))
nhpn <- nhpn %>%
  select(c(div.id = 1, div.name = LNAME,
           county = CTFIPS,
           SOURCE,  # data source
           F_SYSTEM, FCLASS, # addl hwy classification
           LRSKEY, # Uniquely identifies a route within a state
           SIGNT1, SIGNN1, SIGN1, # hwy type & number ("I80")
           MILES, KM, state = STFIPS, geometry))

# recode the annoying i80 bus route
nhpn[grepl("I[0-9]+", nhpn$div.name),]$SIGNT1 = "I"
nhpn[grepl("I[0-9]+", nhpn$div.name),]$SIGN1 = "I80 (bus route)"

# set crs
nhpn <- nhpn %>% conic.transform()

lac_codes <- c(1, 2, 11, 12)
lac <- nhpn %>% filter(FCLASS %in% lac_codes |
                         SIGNT1 == "I") # all limited-access

# drop others that are still NA for SIGNT
lac <- lac %>% filter(!is.na(SIGNT1))


# get USGS water shpfile
gsw <- st_read(paste0(shp.dir, "/water/Lakes_and_Rivers_Shapefile_NA_Lakes_and_Rivers_data_hydrography_l_rivers_v2/hydrography_l_rivers_v2.shp"))
gsw <- st_transform(gsw, st_crs(czs))


# setup CTs for poly population trims -------------------------------------
cts <- st_read("~/R/shapefiles/2010 CTs/US_tract_2010.shp")
cts <- cts %>% select( c( state = 1
                          ,county = 2
                          ,geoid = 4
                          ,gisjoin = GISJOIN
                          ,aland = ALAND10) )

# get ct attr info
cattr = geoseg::cts
head(cattr)
cattr <- cattr %>% select(gisjoin = geoid,
                          cz,czname,cbsa_id,county_id,population,hh)

cts <- left_join(tibble(cts), cattr) %>%
  st_sf()

rm(cattr)

# load rails and prep -----------------------------------------------------

# bureau of transit statistics rails
raildir <- "~/R/shapefiles/rails/"
bts <- st_read( paste0(raildir,
                       "BTS rail lines/cleaned BTS/cleaned-bts.shp") )

bts <- conic.transform(bts)
bts.nona = bts %>% filter(!is.na(DIRECTION))


# ensure identical metered crs -----------------------------------------------
czs <- czs %>% conic.transform()
plc <- plc %>% conic.transform()
cts <- conic.transform(cts)
gsw <- conic.transform(gsw)


# build name-geoid place index --------------------------------------------
# plc <- filter(plc , STATEFP  == 42) %>% # (for test state)
plc.ids <- plc$GEOID
names(plc.ids) <- plc$NAME

save.image("polys ws.RData")
