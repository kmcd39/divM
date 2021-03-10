library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
rm(list=ls())

# get xwalk
co2cbsa <- xwalks::co2cbsa

# count # counties w/in CZ
county_polys <- co2cbsa %>%
  count(cbsa, cbsa_name) %>%
  ungroup() %>%
  rename(county_polys = n)

# that's all
write.csv(county_polys,
          "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/polys/county-polys.csv")


# to visualize ----------------
co2cz %>%
  left_join(divDat::counties
            ,by = c("countyfp" = "geoid")) %>%
  filter(cz_name == "Houston") %>%
  st_sf() %>%
  select(countyfp, name, cz, cz_name) %>%
  mapview(zcol = "name")
