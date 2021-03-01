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

# form czs from counties
counties <- tigris::counties( year = 2019)

counties <- counties %>%
  left_join(xwalks::co2cz,
            by = c("GEOID" = "countyfp",
                   "STATEFP" = "statefp"))

czs <- counties %>%
  divM::conic.transform() %>%
  group_by(cz, cz_name) %>%
  summarise(., do_union = T)

czs

plcs$plc.id <-  plcs$GEOID


# generate xwalk
plc2cz <-
  xwalks::get.spatial.overlap(sf1 = plcs,
                              sf2 = czs,
                              "plc.id",
                              "cz")


nrow(plc2cz)
plc2cz$perc.area %>% summary()
plc2cz$perc.area %>% quantile(seq(0,1,.05))
plc2cz %>% arrange(perc.area)


# keep >10% in area for now
cz.plc.polys <- plc2cz %>%
  filter(perc.area >= .10) %>%
  tibble() %>%
  count(cz) %>%
  rename(place.polys = n)

cz.plc.polys <-
  cz.plc.polys %>%
  left_join(
    select(tibble(czs),
           1:2)
  )

cz.plc.polys

# quick check
# first 2 sld be ~equal; 3rd shld be # of czs
nrow(plcs)
nrow(plc2cz)
nrow(cz.plc.polys)
cz.plc.polys$place.polys %>% sum()
cz.plc.polys


# write ------------------------------------------------------------------------
write.csv(cz.plc.polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/polys/place-polys.csv")

