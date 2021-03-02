# setup ws ----------------------------------------------------------------
rm(list=ls())
source("polygons and subdivisions/polys fcns.R")

library(sf)
library(dplyr)
library(purrr)
library(mapview)

#library(dblinkr)
# con <- princeton.db.connect()
#czs <- st_read(con, query = "select * from regions.czs1990;") # i gotta fix czs crs in db
czs <- divDat::geo.list$cz %>% select(region.id, region.name, region.type, geometry)
# czs <- czs %>% rename(id = region.id, name = region.name)

# get usgs water shpfile
gsw <- st_read("~/R/shapefiles/water/Lakes_and_Rivers_Shapefile_NA_Lakes_and_Rivers_data_hydrography_l_rivers_v2/hydrography_l_rivers_v2.shp")
gsw <- st_transform(gsw, st_crs(czs))


# get subset of CZs that have some intersection w/ water lines ------------
sbgp <- st_intersects(czs,gsw)
wtr.eligible <- czs$region.id[lengths(sbgp) > 0]

# spot checks -------------------------------------------------------------
mn <- czs[czs$region.name=="Minneapolis",] 
phl <- czs[czs$region.name == "Philadelphia", ]

polygonal.div(
  st_intersection(
    st_buffer(mn, -500), gsw),
   mn
  ,return.map = T
  ,min.size = 2e7)

tmp <- st_intersection(mn, gsw)

split <- st_split(st_buffer(mn, -500), tmp)
split <- split %>% rmapshaper::ms_explode()
area.floor <- 1e7
out <- split %>% st_transform(4326) %>%
  mutate(area = lwgeom::st_geod_area(.$geometry)) %>%
  filter(as.numeric(area) > area.floor) %>%
  mutate(id = 1:nrow(.))
out %>% mapview(zcol = "id")
Polys.wrapper( mn
              , gsw
              , always.include = NULL)

lac.polys_test <- map_dfr(lac.eligible[1:8]
                     , ~Polys.wrapper(czs[czs$region.id == ., ]
                                      , lac
                                      , always.include = NULL))
gsw

# ad hoc usgs water polys fcn ---------------------------------------------

get.usgs.water.polys <- function(region, neg.buffer = 500
                                 , area.min = 2e7, return.map = F) {
  
  rwtr <- st_intersection(region, gsw)
  split <- st_split(st_buffer(region, -neg.buffer)
                    , rwtr) %>%
    rmapshaper::ms_explode()
  
  out <- split %>% st_transform(4326) %>%
    mutate(area = lwgeom::st_geod_area(.$geometry)) %>%
    filter(as.numeric(area) > area.min) %>%
    mutate(id = 1:nrow(.))
  
  if(return.map) 
    return(out)
  else
    return(
      data.frame(  region.id = region$region.id
                  ,region.name = region$region.name
                  ,region.type = region$region.type
                  ,n.water.polys = max(out$id)  )
    )
}

get.usgs.water.polys(mn
                     ,return.map=T) %>% mapview(zcol = "id")

get.usgs.water.polys(phl #mn
                     ,return.map=T) %>% mapview(zcol = "id")



### okay just do a markdown on these considerations....
# interpolation vs messiness of linear plan. Using population raster as a means to filter non-useful polygons is good approach.

# map thru & generate measures --------------------------------------------

usgs.water.polys <- map_dfr( wtr.eligible
                             , ~get.usgs.water.polys(czs[czs$region.id == ., ]) )

usgs.water.polys %>% arrange(desc(n.water.polys))

polygonal.div()

# scratch -----------------------------------------------------------------



tmp <- st_intersection(phl, gsw)

split <- st_split(st_buffer(mn, -500), tmp)
split <- split %>% rmapshaper::ms_explode()
area.floor <- 1e7
out <- split %>% st_transform(4326) %>%
  mutate(area = lwgeom::st_geod_area(.$geometry)) %>%
  filter(as.numeric(area) > area.floor) %>%
  mutate(id = 1:nrow(.))
out %>% mapview(zcol = "id")
Polys.wrapper( mn
               , gsw
               , always.include = NULL)

lac.polys_test <- map_dfr(lac.eligible[1:8]
                          , ~Polys.wrapper(czs[czs$region.id == ., ]
                                           , lac
                                           , always.include = NULL))
gsw