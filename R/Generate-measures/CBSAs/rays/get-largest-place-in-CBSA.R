# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

# 2018 instead of 2019 b/c temporary issue w/ census server, but seems no
# changes.
cbsas <-
  tigris::core_based_statistical_areas(year = 2018)

state_list <- xwalks::co2cbsa$statefp %>% unique()

plcs <- map_dfr( state_list,
                 ~tigris::places(state = .,
                                 year = 2019)
                 )


plcs$plc.id <-  plcs$GEOID
cbsas$cbsa.id <- cbsas$GEOID

# generate xwalk
plc2cbsa <-
  xwalks::get.spatial.overlap(sf1 = plcs,
                              sf2 = cbsas,
                              "plc.id",
                              "cbsa.id")
plc2cbsa <- plc2cbsa %>%
  filter(perc.area >= .10) %>%
  tibble()



# add population ----------------------------------------------------------


#devtools::install_github("https://github.com/spatial-ineq/geoseg.git")
pops <-
  map_dfr(state_list,
          ~tidycensus::get_acs(
            geography = "place",
            state = .,
            variable =
              "B01001_001",
            geometry = T,
            year = 2019
            )
          )


pops <- pops %>%
  select(GEOID,
         plc.name = NAME,
         pop = estimate)

plc2cbsa <- plc2cbsa %>%
  left_join(pops,
            by = c("plc.id" =  "GEOID")
            )

# largest place in cbsa ---------------------------------------------------

cbsas %>% nrow()

largest.plc.in.cbsa <- plc2cbsa %>%
  group_by(cbsa.id) %>%
  filter(pop == max(pop,
                    na.rm = T)) %>%
  ungroup()

# trim cols
largest.plc.in.cbsa <-
  select(largest.plc.in.cbsa,
         plc.id, cbsa.id, plc.name)

class(largest.plc.in.cbsa)

# re-attach place geometries
largest.plc.in.cbsa <- left_join(largest.plc.in.cbsa,
          pops[, c("GEOID", "geometry")],
          by = c("plc.id" = "GEOID"))

largest.plc.in.cbsa %>% st_sf() %>%  st_is_empty() %>% sum()
# write -------------------------------------------------------------------

saveRDS(largest.plc.in.cbsa,
        here::here("R/Generate-measures/CBSAs/rays/largest.plc.in.cbsa.rds")
        )

