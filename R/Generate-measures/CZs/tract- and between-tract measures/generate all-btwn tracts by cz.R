# setup ws ---------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(sf)
devtools::load_all()

# get all cts
cts <- xwalks::ctx


# get all hwys
hwys <- st_read("~/R/shapefiles/National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp")

hwys <- st_transform(hwys, st_crs(st_sf(ctsf)))

hwys <- st_intersection(hwys,
                        st_union(ctsf))

ints <- hwys %>%
  filter(SIGNT1 %in% "I")



