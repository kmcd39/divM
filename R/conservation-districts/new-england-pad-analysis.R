library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

rm(list = ls())

# RI wktf
statsf <- tigris::states(year = 2021) %>%
  rename_with(tolower)

ri <- statsf %>% filter(statefp == '09')
riwktf <- ri %>%
  st_transform(5070) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_text()

riwktf


# tests on local computer -------------------------------------------------

devtools::load_all()

ri.test <- Wrapper_pad.area.by.nbhd(
    statefp = 44,
    pad.dir = '~/R/local-data/usgs/PADUS3_0Geodatabase/',
    category.colms = c( 'des_tp' ),
    geo.query = tigris::tracts,
    geo.yr = 2021,
    simplify.geos = F
)
