
# setup ws ---------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(sf)
devtools::load_all()

# get all hwys
hwys <- st_read("~/R/shapefiles/National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp")

hwys <- hwys %>% divM::conic.transform()

ints <- hwys %>%
  filter(SIGNT1 %in% "I")


# sample run -------------------------------------------------------------------

# 24701 # st louis
#21501 # minneapolis
# "09100" # atlanta
# "19700" # philly
tmpcz <- "09100"
(tmpr <- get.region.identifiers(cz = "09100"))
xwalks::co2cz %>% filter( cz == "09100")
tigris::tracts(13,
               "063",
               year = 2019)

atd <-
  Wrapper_gen.tract.div.measures(cz = tmpcz,
                                 divs =
                                   list(int = ints,
                                        hwy = hwys),
                                 year = 2019
  )


# checking and visualizing output ----------------------------------------------

atl.cts <- tracts.from.region(tmpr)[c("geoid", "geometry")]
atl.cts <- atl.cts %>% conic.transform()
atl.ints <- st_intersection(ints, st_union(atl.cts))

ggplot() +
  geom_sf(data = st_boundary(st_union(atl.cts))) +
  geom_sf(data = atl.ints,
          aes(color = SIGNN1)) +
  theme_dark()

atd <-
  atd %>%
  left_join(
    atl.cts
  )

atd[2:6] %>%
  st_sf() %>%
  plot()

library(mapview)
mapview(st_sf(atd),
        zcol = "int.poly") +
  mapview(atl.ints,
          color = "#d12e6a")
