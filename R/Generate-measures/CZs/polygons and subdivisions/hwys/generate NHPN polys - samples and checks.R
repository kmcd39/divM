# get data & ws and fcns --------------------------------------------------
rm(list=ls())
library(tidyverse)
library(sf)
library(lwgeom)
library(mapview)

devtools::load_all(export_all = F)

# option setting
sf_use_s2(F)
options(tigris_use_cache = TRUE)

# get czs
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(nhpn) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(czs) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

czs <- tibble(czs) %>% geox::region.reorg('cz', abvcols = F)

czs <- czs %>% rename(region.name = cz_name)
czsf <- st_sf(czs)
# Limited-access proxy ---------------------------------------
lac <- nhpn %>% filter(FCLASS %in% divM::lac_codes |
                         SIGNT1 == "I")

# drop others that are still NA for SIGNT
lac <- lac %>% filter(!is.na(SIGNT1))

# get subset of CZs that have some intersection w/ interstates & lacs ------------
sbgp <- st_intersects(czsf, filter(nhpn, SIGNT1 == "I"))
int.eligible <- czs$region.id[lengths(sbgp) > 0]

sbgp <- st_intersects(czsf, lac)
lac.eligible <- czs$region.id[lengths(sbgp) > 0]

# spot checks -------------------------------------------------------------
which(lac.eligible=="30702")
which(lac.eligible=="00700")

?Polys.wrapper

phl.polys <- Polys.wrapper(  region = czsf[czs$region.name == "Philadelphia", ]
                             , div.sf = lac
                             , fill.gaps = T
                             , div.identifier.column = "SIGNT1"
                             , always.include = NULL
                             , include.intersecting = F
                             , remove.NA.divs = T
                             , negative.buffer = 100
                             , min.size = 5e5
                             , min.population.count = 1
                             , min.population.perc = NULL
                             , return.sf = T)

phl.polys %>% mapview(zcol= "id")

czs[czs$region.id %in% lac.eligible[1:8], ]


' # test run
lac.polys_test <-
  map_dfr(lac.eligible[1:8]
          , ~Polys.wrapper(  czs[czs$region.id == ., ]
                             , div.sf = lac
                             , fill.gaps = T
                             , div.ientifier.column = "SIGNT1"
                             , always.include = NULL
                             , include.intersecting = F
                             , remove.NA.divs = T
                             , negative.buffer = 100
                             , min.size = 5e5
                             , min.population.count = 100
                             , min.population.perc = NULL
                             , return.sf = T)
  )

lac.polys_test
'

# spot checks ------------------------------------------------------------
lac.polys[lac.polys$region.name=="Philadelphia",]
lac.polys[lac.polys$n.polys <= 1,] # %>% nrow()


geox::rx %>%
  filter(grepl("Louis",
               cz_name))
# checking some
tmp <- czs[czs$region.id == 38602,] # Colville (international border issue --- solved at 80m negative buffer)
tmp <- czs[czs$region.id == 10200,] # Albany, GA (lac hwys running through but not bisecting)
tmp <- czs[czs$region.id == 18600,] # Albany, NY
tmp <- czs[czs$region.id == 32000,] # Houston, TX
tmp <- czs[czs$region.id == 33100,] # Dallas, TX
tmp <- czs[czs$region.id == 19700,] # PHilly, pa
tmp <- czs[czs$region.id == 10102,] # Hastings
tmp  <- czs[czs$region.id == 24701,] # stL


# generating polys and checking map
hwtmpy <- Polys.wrapper(
  region =  st_sf(tmp)
  ,div.sf = nhpn
  , div.identifier.column = "SIGNT1"
  ,always.include = "I"
  ,include.intersecting = F
  , return.sf = T)

hwtmpy

# versus just HWYs
hwtm <- visaux::get.NHPN(sfx =
                           st_transform(st_sf(tmp)
                                        ,4326))


hwtmpy
hwtm <- hwtm %>% st_transform(st_crs(st_sf(tmp)))
hwttm <- hwtm %>%
  select(matches('^sign'), fclass)
mapview(hwtmpy
        ,zcol = 'id') +
  mapview(hwttm
          ,zcol = "sign1"
          ,lwd=4.2)



# i64 is missing from dataset?? --------------------------------------------
if_any
#isf <-
  nhpn %>%
  filter(
    if_any

    rowAny(across(matches('^SIGNT[1-9]')
                  , ~.x == 'U'))
    )
  select(matches('^SIGNT')) %>%
  filter
  filter(any(vars(matches("^SIGNT[1-9]")) %in% "I64"))

#vars(matches("^SIGN[1-9]"))
                , ~.x == "I64")
isf
