library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

rm(list = ls())


library(mapview)


# data loads --------------------------------------------------------------

# states for crop areas
statesf <- tigris::states(year = 2021) %>%
  rename_with(tolower) %>%
  st_transform(4326)

## national districts ------------------------------------------------------

ddir <- '~/R/local-data/historic-places/'

gdpth <- list.dirs(
  paste0(ddir,'gis/'),
  recursive = T
  #,pattern = 'gdb$'
  ,full.names = T)

gdpth <- gdpth[grepl('gdb$', gdpth)]

lyrs <- gdpth %>% st_layers()
lyrs

# # use wkt spatial filter to read only for a specific area and do data checks:
# # bounding box:
# fcos <-
#   tigris::counties(state = 'la'
#                    , year = 2021) %>%
#   rename_with(tolower)
#
# #fcos[1:2] %>% plot()
#
# bbx <- fcos %>%
#   st_bbox() %>%
#   st_as_sfc()
#
# # wkt filter:
# wktf <- st_as_text(bbx)


# # nrcrs: national register "cultural resources" -- term they use.
# # reading w or w/o spatial filter:
# ncrs <-
#   lyrs$name[1:10] %>%
#   map(
#     ~st_read(
#       gdpth
#       ,layer = .x
#       #,wkt_filter = wktf
#     )
#   ) %>%
#   set_names(
#     lyrs$name[1:10]
#   )
#
# ncrs <- ncrs %>%
#   map( tibble ) %>%
#   map( ~rename_with(.x, tolower) ) %>%
#   map( ~rename(.x, geometry = shape) ) %>%
#   map( ~rename_with(.x, tolower) )
#
# # drop empty layers
# ncrs <- ncrs[lengths(ncrs) > 0]
# names(ncrs)
# ncrs$crdist_py %>% glimpse()
#
#
# # just keep resnames, property ids, and layer names, bind to single sf object
# ncr <- ncrs %>%
#   imap_dfr( ~{.x %>%
#       select(resname, nr_propertyid, geometry) %>%
#       mutate(lyr = .y , .before = geometry)
#   })
#
# # only polygon version of natl register places
# #ncr %>% tibble( ) %>% count(lyr)
# ncrpy <- ncr %>%
#   filter(grepl('py$', lyr))
#
# ncrpy <- ncrpy %>% st_sf() %>% st_make_valid()



# natrional register districts, separate from others
#nrdists <- ncrs$crdist_py  %>% st_sf() %>% st_make_valid()

# just read crdistspy
nrdists <- st_read(
  gdpth
  ,layer = 'crdist_py'
  ) %>%
  rename(geometry = SHAPE) %>%
  rename_with(tolower) %>%
  st_sf() %>%
  st_make_valid()

# trim cols
nrdists <- nrdists %>% select(resname, nr_propertyid, geometry)
nrdists <- nrdists %>% st_transform(4326)

## Baton Rouge hdist load local data ---------------------------------------------------------------

# about the districts: https://www.brla.gov/2577/Historic-Districts

# open data shpfile: https://data.brla.gov/Culture-and-Recreation/Local-District/p8r8-2ium

# pull using their api
lhdst <- st_read('https://data.brla.gov/resource/p8r8-2ium.geojson')

# lhdst %>% mapview()


# map together ------------------------------------------------------------

nrcropt <- nrdists %>%
  st_crop( filter(statesf, stusps == 'LA'))

brplc <- tigris::places(state = 'LA', year = 2021) %>%
  rename_with(tolower) %>%
  st_transform(4326)
brplc <- brplc %>%
  filter(grepl('^Baton Rouge', name))

mapview( st_boundary(nrcropt)
        ,color = visaux::jewel.pal()[5]
        ,layer.name = 'national register districts'
        ,lwd = 4
        ) +
  mapview( lhdst
           ,col.regions = visaux::jewel.pal()[4]
           ,layer.name = 'baton rouge local districts'
           ) +
  mapview( st_boundary(brplc)
           ,color = visaux::jewel.pal()[1]
           ,lwd = 1
           ,layer.name = 'Baton Rouge city limits')


# st louis? ---------------------------------------------------------------

# city of st louis open data: https://www.stlouis-mo.gov/data/datasets/dataset.cfm?id=73

# but city also made their own map doing the same comparison:  https://www.stlouis-mo.gov/government/departments/planning/planning/adopted-plans/strategic-land-use/histmap.cfm

# LA ----------------------------------------------------------------------

# not geoJson api.. just json whichi s a pain.. just gna download shpfile.

library(rjson)
#install.packages('RSocrata')

#https://data.lacity.org/Housing-and-Real-Estate/Historic-Preservation-Overlay-Zone/upvx-bixm

# hilariously they dont have geojson as an option (just json), but changing
# extention to geojson seems to work:
apiendpt <- 'https://data.lacity.org/resource/hnms-4szp.geojson'

lahdsts <- st_read(apiendpt)

# lahdsts %>% mapview()

# get LA city limites too
laplc <- tigris::places(state = 'CA', year = 2021) %>%
  rename_with(tolower) %>%
  st_transform(4326)
laplc <- laplc %>%
  filter(grepl('^Los Angeles', name))

# LA and NR map
nrcropt <- nrdists %>%
  st_crop( filter(statesf, stusps == 'CA'))

mapview( st_boundary(nrcropt)
         ,color = visaux::jewel.pal()[5]
         ,layer.name = 'national register districts'
         ,lwd = 4
) +
  mapview( lahdsts
           ,col.regions = visaux::jewel.pal()[4]
           ,layer.name = 'LA local districts'
  ) +
  mapview( st_boundary(laplc)
           ,color = visaux::jewel.pal()[1]
           ,lwd = 1
           ,layer.name = 'LA city limits')



# chicago -----------------------------------------------------------------

# link: https://data.cityofchicago.org/Historic-Preservation/Landmark-Districts/zidz-sdfj

# chicago is annoying their GIS geos arent available from api.
