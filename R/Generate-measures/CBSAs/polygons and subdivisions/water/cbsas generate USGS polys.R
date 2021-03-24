# setup ws ----------------------------------------------------------------
rm(list=ls())

library(sf)
library(lwgeom)
library(dplyr)
library(purrr)
library(mapview)
devtools::load_all()

# get ray WS for czs and other conveninces
#load("R/Generate-measures/ray-ws.Rdata")

# get cbsas
cbsas <- divDat::cbsas # tigris::core_based_statistical_areas(year = 2019)
cbsas <- tibble(cbsas) %>% geoseg::region.reorg("cbsa") %>% rename(region.name= cbsa_name)
cbsas <- st_sf(cbsas) %>% divM::conic.transform()
# get local hydrography files from USGS - https://www.sciencebase.gov/catalog/item/4fb55df0e4b04cb937751e02
gsw <- st_read("~/R/shapefiles/water/Lakes_and_Rivers_Shapefile_NA_Lakes_and_Rivers_data_hydrography_l_rivers_v2/hydrography_l_rivers_v2.shp")
gsw <- st_transform(gsw, st_crs(cbsas))


# get subset of CZs that have some intersection w/ water lines ------------
#wtr.eligible <- st_intersects(cbsas, gsw)
#wtr.eligible <- cbsas[lengths(wtr.eligible) > 0,]


# fcn --------------------------------------------------------------------------

?Polys.wrapper


# test run ---------------------------------------------------------------------
devtools::load_all()
#cbsas[cbsas$region.id == "12120", ] %>%
cbsas[grepl("New York", cbsas$region.name), ] %>%
  Polys.wrapper(gsw,
                fill.gaps = F,
                min.population.perc = 0.02,
                return.sf = T) %>%
  mapview()

# map thru regions --------------------------------------------------------

usgs.water.polys <-
  cbsas$region.id %>%
  map_dfr( ~Polys.wrapper(region = cbsas[cbsas$region.id == ., ]
                          ,gsw
                          ,fill.gaps = F
                          ,min.population.perc = 0.02
                          ,return.sf = F)
           )
usgs.water.polys <- usgs.water.polys %>% rename("water.polys" = n.polys)
write.csv(usgs.water.polys,
          file = "cbsa- usgs water-polys.csv")



# do czs -----------------------------------------------------------------------

# get ray WS for czs and other conveninces
load("R/Generate-measures/ray-ws.Rdata")

czs <- tibble(czs) %>% geoseg::region.reorg("cz") %>% rename(region.name= cz_name)
czs <- st_sf(czs) %>% divM::conic.transform()


usgs.water.polys <-
  czs$region.id %>%
  map_dfr( ~Polys.wrapper(region = czs[czs$region.id == ., ]
                          ,gsw
                          ,fill.gaps = F
                          ,min.population.perc = 0.02
                          ,return.sf = F)
  )


usgs.water.polys <- usgs.water.polys %>% rename("water.polys" = n.polys)
write.csv(usgs.water.polys,
          file = "cz- usgs water-polys.csv")


# format better to save --------------------------------------------------------
library(tidyverse)
fns <- list.files(pattern = "\\.csv")
wp <- map(fns, vroom::vroom)
wp <- wp %>% map( ~select(., -any_of("...1")) )

names(wp) <- map(wp, ~pull(., region.type)[1])

wp$cbsa <- wp$cbsa %>% select(cbsa = region.id,
                              cbsa.name = region.name,
                              water.polys)
wp$cz <- wp$cz %>% select(cz = region.id,
                              cz.name = region.name,
                              water.polys)
wp$cbsa %>%
  write.csv(
    file = "cbsa- usgs water-polys.csv",
    row.names = F)

wp$cz %>%
  write.csv(
    file = "cz- usgs water-polys.csv",
    row.names = F)


# spot checks -------------------------------------------------------------





