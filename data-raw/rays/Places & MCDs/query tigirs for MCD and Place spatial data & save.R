library(sf)
library(dplyr)
# get natl data for each
state.list <- xwalks::full.xwalks %>% select(1,2) %>% unique()
library(purrr)
tigris:
mcds <- 
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
st_write(mcds, "~/R/shapefiles/county-subdivisions/csds.shp")