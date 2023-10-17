library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

rm(list = ls())

# # RI wktf
# statsf <- tigris::states(year = 2021) %>%
#   rename_with(tolower)
#
# ri <- statsf %>% filter(statefp == '44')
# riwktf <- ri %>%
#   st_transform(5070) %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_as_text()
#
# riwktf


# tests on local computer -------------------------------------------------

devtools::load_all()

ri.test <-
  local.Wrapper_pad.area.by.nbhd(
    statefips = 44,
    nbhds.fcn = tigris::tracts
    ,simplify.geos = T
  )

ri.test


# gen locally for initial states --------------------------------------------------------------

#' from patrick: "To start up any of NJ, CT, or MA would be interesting, tho
#' ultimately I'd like to do this nationally."
statesf <- tigris::states()
to.gen <- statesf %>% tibble() %>% select(STATEFP, NAME, STUSPS) %>%
  filter(grepl('NJ|CT|MA|RI', STUSPS)) %>%
  .$STATEFP

to.gen

devtools::load_all()

# local.Wrapper_pad.area.by.nbhd(
#   # statefips = '25' # MA
#   statefips = '09' # CT
#   ,simplify.geos = T
# )

for(fips in to.gen) {
  cat(fips, '\n')
  # generate for initial states
  local.Wrapper_pad.area.by.nbhd(
      statefips = fips
     ,simplify.geos = T
  )
}

#' for CT(09), got an error at the collection extract... then at the
#' st_difference? Error in (function (msg) : TopologyException: side location
#' conflict at 1866690 2348091. This can occur if the input geometry is invalid.
#'
#' Trying again with geo.simplification

# gen for a next batch of states ------------------------------------------

measure.dir <- '~/R/divM/genereated-measures/fall-2023-conservation-areas/'


already.generated <-
  measure.dir %>%
  list.files(pattern = '\\d{2}.csv$') %>%
  str_extract('\\d{2}')

# what's left
statefps <- geox::rx$statefp %>% unique()
togen <- setdiff(statefps, already.generated)

# "send" a batch
togen[1:5] %>%
  map(
    local.Wrapper_pad.area.by.nbhd
  )

# checking what states are there ------------------------------------------

