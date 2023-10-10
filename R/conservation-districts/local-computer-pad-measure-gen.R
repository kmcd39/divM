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

# ri.test <- Wrapper_pad.area.by.nbhd(
#     statefp = 44,
#     pad.dir = '~/R/local-data/usgs/PADUS3_0Geodatabase/',
#     category.colms = c( 'des_tp' ),
#     geo.query = tigris::tracts,
#     geo.yr = 2021,
#     simplify.geos = F
# )



# just run locally --------------------------------------------------------

# i am having Della issues...so just run locally

#' note arguments passed onto divM::wrapper --- I AM SIMPLIFYING POLYGONS for
#' now
pad.by.tract.della.wrapper <- function(
    statefp,
    save.dir =
      # '/scratch/gpfs/km31/protected-areas/generated-measures/'  # for della
      '~/R/local-data/usgs/generated-PAD-measurues/'
    )
{

  require(tidyverse)
  require(sf)

  # just using equal-area projection that USGS is in
  #sf_use_s2(F)

  pad.measures <-
    Wrapper_pad.area.by.nbhd(
      statefp,
      pad.dir =
        # '/scratch/gpfs/km31/protected-areas/usgs-data/PADUS3_0Geodatabase/'
          '~/R/local-data/usgs/PADUS3_0Geodatabase/'
      ,category.colms = c( 'featclass'
                          ,'own_type', 'own_name'
                          ,'mang_type', 'mang_name'
                          ,'des_tp'
                          ,'gap_sts'),
      geo.query = tigris::tracts,
      geo.yr = 2021,
      simplify.geos = T)

  # write
  save.path <- paste0(save.dir, 'statefp-', statefp, '.csv')
  write.csv(
    pad.measures
    ,file = save.path
    ,row.names = F
  )

  return(pad.measures)

}


# gen for NE --------------------------------------------------------------

devtools::load_all()
# wrapped function:
divM::Wrapper_pad.area.by.nbhd

# "send" job
pad.measures <- map(
  c('09', '25', '44') ,
  Wrapper_pad.area.by.nbhd
)


# gen for a next batch of states ------------------------------------------

already.generated <-
  '~/R/local-data/usgs/generated-PAD-measurues/' %>%
  list.files(pattern = '\\d{2}.csv$') %>%
  str_extract('\\d{2}')

# what's left
statefps <- geox::rx$statefp %>% unique()
togen <- setdiff(statefps, already.generated)

# send a batch
togen[1:5] %>%
  map(
    pad.by.tract.della.wrapper
  )

# checking what states are there ------------------------------------------

