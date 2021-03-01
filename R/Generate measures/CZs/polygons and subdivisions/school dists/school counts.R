library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
rm(list=ls())

state_list <- xwalks::co2cz$statefp %>% unique()

school.dists <- map_dfr( state_list,
                         ~tigris::school_districts(state = .,
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


school.dists$sd.id <- school.dists$GEOID

# generate xwalk
sd2cz <-
  xwalks::get.spatial.overlap(sf1 = school.dists,
                              sf2 = czs,
                              "sd.id",
                              "cz")


nrow(sd2cz)
sd2cz$perc.area %>% summary()
sd2cz$perc.area %>% quantile(seq(0,1,.05))
sd2cz %>% arrange(perc.area)

# keep >10% in area for now
cz.sd.polys <- sd2cz %>%
  filter(perc.area >= .10) %>%
  tibble() %>%
  count(cz) %>%
  rename(school.dist.polys = n)

cz.sd.polys <-
  cz.sd.polys %>%
  left_join(
    select(tibble(czs),
           1:2)
  )

cz.sd.polys

# quick check
# first 2 sld be ~equal; 3rd shld be # of czs
nrow(school.dists)
nrow(sd2cz)
nrow(cz.sd.polys)
cz.sd.polys$school.dist.polys %>% sum()
cz.sd.polys


# write ------------------------------------------------------------------------
write.csv(cz.sd.polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/polys/school-dist-polys.csv")


