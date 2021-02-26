# setup ws ----------------------------------------------------------------
rm(list=ls())
source("polygons and subdivisions/polys fcns.R")

library(sf)
library(dplyr)
library(dbplyr)
library(dblinkr)

# set test areas
czs <- divDat::geo.list$cz %>% select(region.id, region.name, region.type, geometry) %>% st_transform(4326)
phl <- czs[czs$region.name=="Philadelphia",]
mn <- czs[czs$region.name=="Minneapolis",]
con <- princeton.db.connect() # use this one for schemas to work



# over test area ----------------------------------------------------------

mnwO <- query.division(con, mn, "divs.water_tigris")
st_crs(mnwO) <- 4326
mnw <- st_intersection(mn,mnwO)
mnw$is.named <- !is.na(mnw$FULLNAME)

isn <- mnw %>% filter(is.named)
nn <- mnw %>% filter(!is.named)

sgbp <- st_intersects(nn, isn)
touches.named <- nn[ lengths(sgbp) > 0, ]

#library(ggplot2)
#ggplot() +
#  geom_sf(data=isn, fill="#008080",color=NA) + 
#  geom_sf(data=touches.named, fill="#880030",color=NA)

mnw <- rbind(isn, touches.named)
# mapview(mnw, color = "#008080") + mapview(st_boundary(mn))
# mapview(mnwO, color = "#008080") + mapview(st_boundary(mn))

# water.trimmed
mn.wt <- st_difference(mn, st_union(mnw))
area.floor <- 2e7
out <- mn.wt %>%
  rmapshaper::ms_explode() %>%
  mutate(area = as.numeric(lwgeom::st_geod_area(.$geometry))) %>%
  filter(area > area.floor) %>%
  mutate(id = 1:nrow(.))
  
out %>% mapview(zcol = "id")
  

nyz = tigris::zctas(state = 36)
   