library(sf)
library(dplyr)
library(purrr)
rm(list = ls())


# get all places:
plcs <- st_read("~/R/shapefiles/places/places.shp")
# (to retrieve file: )
'plcs <-
  map(state.list$state
      , ~ tigris::places(state = ., class = "sf", year = 2018))
plcs <- do.call("rbind", plcs)
st_write(plcs, "~/R/shapefiles/places/places.shp")'

head(plcs)
## trim columns
colnames(plcs)
plcs <- plcs %>% select(GEOID, NAME, geometry)

## get czs & set plc crs
czs <- divDat::czs %>% group_by(cz, cz_name)
plcs <- st_transform(plcs, st_crs(czs))

# get plc2cz xwalk
plc2cz = xwalks::generate.coterminous.xwalk(select(plcs
                                                   ,plcid=GEOID
                                                   ,plc_name = NAME)
                                            , czs)
# add containing czs to plc data
plcs <- left_join(plcs[,c("GEOID", "geometry")],
                  plc2cz,
                  by = c("GEOID" = "plcid"))

# visualize sample over TX -- shows places matched to cz (cz's represented w/ red
# boundary)
'
library(mapview)
tx_czs = xwalks::co2cz %>% filter(state == "TX") %>% pull(cz)
tx_czs = czs %>% filter(cz %in% tx_czs)
plcs[ , c("GEOID", "NAME")] %>%
  left_join(plc2cz
            , by = c("GEOID"="plcid")) %>%
  filter(cz %in% tx_czs$cz) %>%
  mapview(zcol="cz_name", legend=F) +
  mapview(st_boundary(tx_czs)
          ,color = "red", lwd=3)
'

# add population to spatial data ---------------------------------------------------------

# get population from census tables
# uses 5-yr aggregations for 2018 population estimates
plc.pop <-
  tidycensus::get_acs(geography = "place",
                      year = 2018,
                      variables = "B01003_001") # Total population
plc.pop <- plc.pop %>%
  select(c(1, population = estimate))

# join population to spatial data
head(plc.pop)
plcs <- left_join(  plcs,
                    plc.pop,
                    by = c("GEOID"))

# filter to largest centers in cz's --------------------------------------------------------------

(largest.plc.in.cz <- plcs %>%
  group_by(cz, cz_name) %>%
  filter(population == max(population)))

(largest.plc.in.cz <- largest.plc.in.cz %>%
  select(geoid = GEOID, name = plc_name, population, geometry))



# write data to pkg -----------------------------------------------------------------
usethis::use_data(largest.plc.in.cz, overwrite = TRUE)
