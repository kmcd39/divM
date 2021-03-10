library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
rm(list=ls())

# get xwalk
co2cz = xwalks::co2cz

# count # counties w/in CZ
county_polys <- co2cz %>%
  count(cz, cz_name) %>%
  ungroup() %>%
  rename(county_polys = n)

# that's all

write.csv(county_polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/polys/county-polys.csv",
          row.names = F)


# to visualize ----------------
co2cz %>%
  left_join(divDat::counties
            ,by = c("countyfp" = "geoid")) %>%
  filter(cz_name == "Houston") %>%
  st_sf() %>%
  select(countyfp, name, cz, cz_name) %>%
  mapview(zcol = "name")
