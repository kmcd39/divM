library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)

plcs <- divDat::plc
plcs <- plcs %>% divM::conic.transform()

czs <- divDat::czs
#maczs <- czs %>% filter(cz %in% maczs)

# visualize sample
plcs %>%
  filter(STATEFP=="25") %>%
  mapview(zcol="NAME") +
  mapview(st_boundary(maczs)
          , color = "red")

# generate xwalk
plc2cz <- xwalks::generate.coterminous.xwalk(smaller.geo = plcs,
                                             larger.geo = czs)
plc.polys <- plc2cz %>%
  count(cz,cz_name) %>%
  rename(place.polys = n)

# quick check
# first 2 sld be equal; 3rd shld be # of czs
nrow(plcs)
nrow(plc2cz)
nrow(plc.polys)

plc.polys



# write ------------------------------------------------------------------------
write.csv(plc.polys, "data-raw/polygons and subdivisions/.output/place_polys.csv")


# to map sample area:
plc2cz %>%
  filter(cz_name == "Minneapolis") %>%
  left_join(plcs['GEOID']) %>%
  st_sf() %>%
  mapview(zcol= "NAMELSAD")
