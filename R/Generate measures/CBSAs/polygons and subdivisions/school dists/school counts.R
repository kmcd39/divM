library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
rm(list=ls())

state_list <- xwalks::co2cbsa$statefp %>% unique()

school.dists <- map_dfr( state_list,
                 ~tigris::school_districts(state = .,
                                           year = 2019)
                 )

cbsas <- tigris::core_based_statistical_areas( year = 2019)


school.dists$sd.id <- school.dists$GEOID
cbsas$cbsa.id <- cbsas$GEOID

# generate xwalk
sd2cbsa <-
  xwalks::get.spatial.overlap(sf1 = school.dists,
                              sf2 = cbsas,
                              "sd.id",
                              "cbsa.id")


nrow(sd2cbsa)
sd2cbsa$perc.area %>% summary()
sd2cbsa$perc.area %>% quantile(seq(0,1,.05))
sd2cbsa %>% arrange(perc.area)

# keep >10% in area for now
cbsa.sd.polys <- sd2cbsa %>%
  filter(perc.area >= .10) %>%
  tibble() %>%
  count(cbsa.id) %>%
  rename(school.dist.polys = n)

cbsa.sd.polys <-
  cbsa.sd.polys %>%
  left_join(
    select(tibble(cbsas),
           cbsa.name = NAME,
           cbsa.id)
  )

cbsa.sd.polys

# quick check
# first 2 sld be ~equal; 3rd shld be # of cbsas
nrow(school.dists)
nrow(sd2cbsa)
nrow(cbsa.sd.polys)
cbsa.sd.polys$school.dist.polys %>% sum()
cbsa.sd.polys


# write ------------------------------------------------------------------------
write.csv(cbsa.sd.polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/polys/school-dist-polys.csv")


