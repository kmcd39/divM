library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
library(divFcns)
rm(list=ls())

# get data & ws and fcns --------------------------------------------------
cbsas <- tigris::core_based_statistical_areas(year = 2019)

con <-
  dblinkr::db.connect(
    Sys.getenv("PRINCETON_LOGIN"),
    Sys.getenv("PRINCETON_PW")
    )
dblinkr::tbls.in.schema(con, "divs")
bts <- dblinkr::q_db2sf(con, "divs.rails_bts")
st_crs(bts) = 4326
bts <- bts %>% divM::conic.transform()

bts %>% purrr::map(~sum(is.na(.)))
bts.nona <- bts %>% filter(!is.na(DIRECTION))
bts.nona

# spot checks -------------------------------------------------------------
tmpcz = cbsas[cbsas$region.name == "Minneapolis", ]

tmp.divs = st_intersection(bts,
                           st_union(tmpcz))

rail.mn = Polys.wrapper(tmpcz,
                        tmp.divs
                        , fill.gaps = F
                        , div.identifier.column = "DIRECTION"
                        , negative.buffer = 100
                        , min.size = 0
                        , min.population.count = 1000
                        , min.population.perc = NULL
                        , return.sf = T)

rail.mn.nona = Polys.wrapper(tmpcz,
                        filter(tmp.divs, !is.na(DIRECTION))
                        , fill.gaps = F
                        , div.identifier.column = "DIRECTION"
                        , negative.buffer = 100
                        , min.size = 0
                        , min.population.count = 1000
                        , min.population.perc = NULL
                        , return.sf = T)
rail.mn


# get elligible areas -----------------------------------------------------
sbgp <- st_intersects(cbsas, bts.nona)
rr.eligible <- cbsas$region.id[lengths(sbgp) > 0]


# map thru and generate ---------------------------------------------------

rail.polys <-
  map_dfr( rr.eligible,
           ~Polys.wrapper(cbsas[cbsas$region.id == ., ]
                          , bts.nona
                         , fill.gaps = F
                         , div.identifier.column = NULL
                         , negative.buffer = 100
                         , min.size = 5e5
                         , min.population.count = 250
                         , min.population.perc = NULL
                         , return.sf = F)
  )


write.csv(rail.polys, ".local-measures/CBSAs/rail_polys_bts.csv")
'

# ending w some maps ------------------------------------------------------

# detroit
tmp= Polys.wrapper(czs[czs$region.id == 11600, ]
              , bts.nona
              , fill.gaps = F
              , div.identifier.column = NULL
              , negative.buffer = 100
              , min.size = 5e5
              , min.population.count = 250
              , min.population.perc = NULL
              , return.sf = TRUE)

mapview(tmp, zcol = "id") +
  mapview(st_crop(bts.nona,
                  st_bbox(tmp)))
