library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)

state_list <- xwalks::co2cbsa$statefp %>% unique()

plcs <- map_dfr( state_list,
                 ~tigris::places(state = .,
                                 year = 2019)
                 )

cbsas <- tigris::core_based_statistical_areas( year = 2019)


plcs$plc.id <-  plcs$GEOID
cbsas$cbsa.id <- cbsas$GEOID

# generate xwalk
plc2cbsa <-
  xwalks::get.spatial.overlap(sf1 = plcs,
                              sf2 = cbsas,
                              "plc.id",
                              "cbsa.id")


nrow(plc2cbsa)
plc2cbsa$perc.area %>% summary()
plc2cbsa$perc.area %>% quantile(seq(0,1,.05))
plc2cbsa %>% arrange(perc.area)


# keep >10% in area for now
cbsa.plc.polys <- plc2cbsa %>%
  filter(perc.area >= .10) %>%
  tibble() %>%
  count(cbsa.id) %>%
  rename(place.polys = n)

cbsa.plc.polys <-
  cbsa.plc.polys %>%
  left_join(
    select(tibble(cbsas),
           cbsa.name = NAME,
           cbsa.id)
  )

cbsa.plc.polys

# quick check
# first 2 sld be equal; 3rd shld be # of cbsas
nrow(plcs)
nrow(plc2cbsa)
nrow(cbsa.plc.polys)
cbsa.plc.polys$place.polys %>% sum()
plc.polys


# write ------------------------------------------------------------------------
write.csv(cbsa.plc.polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/polys/place-polys.csv")


# to map sample area:
