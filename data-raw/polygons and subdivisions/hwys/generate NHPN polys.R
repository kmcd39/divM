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

# group & denode ----------------------------------------------------------
# mandatory if using "flexi" polys fcns ( i think time to delete this)
# div <- div %>% group_by_at(vars(all_of(grouping.cols))) %>% summarise(., do_union=T)

# spot checks -------------------------------------------------------------
which(lac.eligible=="30702")
which(lac.eligible=="00700")

?Polys.wrapper
phl.polys <- Polys.wrapper(  region = czs[czs$region.name == "Philadelphia", ]
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

phl.polys %>% mapview(zcol= "id")

czs[czs$region.id %in% lac.eligible[1:8], ]

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



# map thru & generate measures --------------------------------------------


?Polys.wrapper
'
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
'
'
write.csv(lac.polys, "intermediate saves/polys/lac-gapfilled v2.csv" )

write.csv(int.polys, "intermediate saves/polys/interstates-gapfilled v2.csv" )
'




# quick checks ------------------------------------------------------------
lac.polys[lac.polys$region.name=="Philadelphia",]
lac.polys[lac.polys$n.polys <= 1,] # %>% nrow()

czs[czs$region.id == 18600,]

# checking some
tmp <- czs[czs$region.id == 38602,] # Colville (international border issue --- solved at 80m negative buffer)
tmp <- czs[czs$region.id == 10200,] # Albany, GA (lac hwys running through but not bisecting)
tmp <- czs[czs$region.id == 18600,] # Albany, NY
tmp <- czs[czs$region.id == 32000,] # Houston, TX
tmp <- czs[czs$region.id == 33100,] # Dallas, TX
tmp <- czs[czs$region.id == 19700,] # PHilly, pa
tmp <- czs[czs$region.id == 10102,] # Hastings



# visual check w/ hwys
#tmph <- subset.polys.hwys(tmp, lac)
tmph <- st_intersection(tmp, hwys)
tmph <- denode.lines(tmph, group.cols = c("SIGNT1", "SIGN1", "FCLASS"))
tmph
mapview(st_boundary(
  st_buffer(tmp, -20) ), color ="#800020") + mapview(tmph, lwd = 3
                                                     , zcol = "FCLASS")

# generating polys and checking map
bmap <- Polys.wrapper( tmp
                       , lac, return.map = T
) %>% mapview(zcol = "id")


bmap + mapview(tmph, zcol="FCLASS")

Polys.wrapper( tmp
               , lac ) #80)


Polys.wrapper( tmp
               , lac
               , negative.buffer = 10) #80)

Polys.wrapper( tmp
               , lac, return.map = T
               ,negative.buffer=10) %>% mapview(zcol = "id")

Polys.wrapper( tmp
               , lac, return.map = T
               ,negative.buffer=100) %>% mapview(zcol = "id")

# idea of population-weighting hwy polygons?

# organize variations -----------------------------------------------------
list(lac.polys, int.polys, czs) %>% map(nrow)

lac.polys %>%
  rename(limited.access.polys = n.polys) %>%
  left_join()

int

lac.polys$gaps_filled <- T
lac.polys$hwy_subset <- "lac"
Polys.wrapper(phl["geometry"], lac, always.include =NULL, include.intersecting = F) # lac only

Polys.wrapper(phl, hwys, always.include ="I", include.intersecting = F) # interstates only


Polys.wrapper(czs[3, ]
              , lac
              , always.include = NULL)
