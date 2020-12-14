library(sf)
library(dplyr)

# get "central cities"
# two possibilities:
# probably I use "minor civil divisions" mcds
# maybe i use "places" -- but filter out census designated places(?)
?tigris::county_subdivisions # this contains MCDs and CCDs
#xwalks::full.xwalks[,c(1,2)] %>% unique()
mcds <- tigris::county_subdivisions(state = 25, class = "sf", year = 2019)
mcdwa <- tigris::county_subdivisions(state = 53, class = "sf", year = 2019)

head(mcds)
# places
plcs <- tigris::places(state = 25, class = "sf", year = 2019)
plcwa <- tigris::places(state = 53, class = "sf", year = 2019)
head(plcs)


# class codes - 
# note they all incorporate incorporated places- and supplement with other types
# https://www.census.gov/library/reference/code-lists/class-codes.html
mcds %>%
  tibble() %>%
  count(CLASSFP)

mcdwa %>%
  tibble() %>%
  count(CLASSFP)


plcs %>%
  tibble() %>%
  count(CLASSFP)


plcwa %>%
  tibble() %>%
  count(CLASSFP)



# helper infra to add hwys to leaflet ---------------------------------------
add.statewide.hwys <- function(map, state.fp) {
  to.add <- div %>%
    filter(state == state.fp &
             region.type == "county") %>%
    st_transform(4326)
  pal <- colorFactor(wesanderson::wes_palettes$Moonrise3
                     ,unique(to.add$SIGNT1))
  map %>% 
    addPolylines( data = to.add
               ,color = ~pal(to.add$SIGNT1)
               ,label = to.add$SIGNT1)
  
}

# map each w hwys ---------------------------------------------------------
library(divFcns)
div<- divDat::div
geo.list <- divDat::geo.list
# MA
quick.leaflet(mcds, "CLASSFP") %>%
  add.statewide.hwys(25)
  
quick.leaflet(plcs, "CLASSFP") %>%
  add.statewide.hwys(25)

# WA
quick.leaflet(mcdwa, "CLASSFP") %>%
  add.statewide.hwys(53)
# interestingly, hwys often act ~as~ the divisions between CCDs here

quick.leaflet(plcwa, "CLASSFP") %>%
  add.statewide.hwys(53)

# joins each w/ population ------------------------------------------------

# get natl data for each
state.list <- xwalks::full.xwalks %>% select(1,2) %>% unique()
library(purrr)
'mcds <- 
  map(state.list$state
      , possibly( ~ tigris::county_subdivisions(state = ., class = "sf", year = 2018)
                  , otherwise = NA)) 
# weirdly, nyc and nm are missing from state-by-state query for 2019, so i rolled back to 2018

mcds <- do.call("rbind", mcds)


tigris::county_subdivisions(state = "NM", class = "sf", year = 2019)

  
# places
plcs <-
  map(state.list$state
      , ~ tigris::places(state = ., class = "sf", year = 2018)) 
  

plcs <- do.call("rbind", plcs)


# save shpfiles -----------------------------------------------------------
st_write(plcs, "~/R/shapefiles/places/places.shp")
st_write(mcds, "~/R/shapefiles/county-subdivisions/csds.shp")'


# link with cts -----------------------------------------------------------
# use pop-weighted centroids b/c they're faster + easier than polygons
cts <- divDat::ctpts

colnames(cts)
# trial state MA
macts <- cts %>%
  filter(state == 25) %>%
  st_transform(4326)

mapl <- plcs %>%
  filter(STATEFP == 25) %>%
  st_transform(4326)
macds <- mcds %>%
  filter(STATEFP == 25) %>%
  st_transform(4326) # st_crs(macts))

# `calculate population by aggregating CTs and rejoining to place geometries.
# checked against https://data.census.gov/ aggregated to place directly
place.pops <- mapl %>%
  select(place.geoid = GEOID,
         place.name = NAME,
         place.class = CLASSFP) %>%
  st_join(macts,
          ., ) %>%
  data.frame() %>%
  group_by(place.geoid,
           place.name) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  ungroup() %>%
  left_join(mapl[,c("GEOID", "CLASSFP", "geometry")],
            by = c("place.geoid" = "GEOID")) %>% 
  st_sf()

place.pops
divFcns::quick.leaflet(place.pops, "population")


csds <- macds %>%
  select(csd.geoid = GEOID,
        csd.name = NAME,
        csd.class = CLASSFP) %>%
  st_join(macts,
          ., ) %>%
  data.frame() %>%
  group_by(csd.geoid,
           csd.name) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  ungroup() %>%
  left_join(macds[,c("GEOID", "CLASSFP", "geometry")],
            by = c("csd.geoid" = "GEOID")) %>% 
  st_sf()

csds
divFcns::quick.leaflet(csds, "population")

# filter to largest x in CZ -----------------------------------------------
maczs <- divDat::geo.list$cz %>%
  filter(state == 25) %>%
  st_transform(4326) %>%
  select( cz = region.id
         ,cz_name = region.name
         ,cz.pop = population
         ,geometry)

city.centers.csds <- 
  st_join(csds,
        maczs) %>%
  group_by(cz, cz_name, cz.pop) %>%
  filter(population == max(population, na.rm = T))

# remove NA relics (water areas)
city.centers.csds <- city.centers.csds %>%
  filter(population != 0)
# map to double check
divFcns::quick.leaflet(city.centers.csds, "population")
# looks good.

# repeat w/ places
city.centers.plcs <- 
  st_join(place.pops,
          maczs) %>%
  group_by(cz, cz_name, cz.pop) %>%
  filter(population == max(population, na.rm = T))

city.centers.plcs <- city.centers.plcs %>%
  filter(!is.na(place.geoid))

city.centers.csds %>%
  divFcns::quick.leaflet("population")



# check if census tracts are coterminous with CSDs ------------------------
csds <- st_read("~/R/shapefiles/county-subdivisions/csds.shp") %>% st_transform(4326)

tracts <- tigris::tracts(state = 1, year = 2018, class = "sf")
# add in population
minimal.cts <- divFcns::abv_out(cts)[ , c("GISJOIN", "state", "population")]

# the insane way those ct codes started as... (from opp insights!)
minimal.cts$GISJOIN %>% head()
tracts$GEOID %>% sort() %>% head()
# extra 0's between state/county/tract for GISJOIN
tracts<-tracts %>%
  mutate(GISJOIN = paste0("G",
                          stringr::str_pad(pull(tracts, STATEFP), width=2, side="left", pad="0"),
                          stringr::str_pad(pull(tracts, COUNTYFP), width=4, side="left", pad="0"),
                          stringr::str_pad(pull(tracts, TRACTCE), width=7, side="left", pad="0")))

  
minimal.cts$GISJOIN %>% head()
tracts$GISJOIN %>% sort() %>% head()

tracp <- tracts %>%
  left_join(minimal.cts
            , by = c("GISJOIN"))

quick.leaflet(tracp, "COUNTYFP",weight=2) %>%
  addPolylines( data = st_boundary(filter(csds
                                          ,as.character(STATEFP)=="01"))
                ,color = "#fcad03"
                ,opacity=.4)

st_join(tracp, )

csds