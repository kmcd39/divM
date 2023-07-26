library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

rm(list = ls())


# notes -------------------------------------------------------------------

#' note that dif levels of government may have different historic district
#' designations. So there may be state, natl, and local historic districts that
#' we wouldn't expect to all be the same and that which have different
#' signifance.
#'
#' One helpful primer on local historic districts vs. those in the national
#' register:
#'
#' https://gadnr.org/sites/default/files/hpd/pdf/NR%20Historic%20District%20vs.%20Local%20Historic%20District.pdf

# load data ---------------------------------------------------------------

ddir <- '~/R/local-data/historic-places/'
list.files(ddir)


## state lvl ---------------------------------------------------------------

# state-lvl directory
sddir <- paste0(ddir, 'gis/state-level/')

# state abbriviations to load:
states2load <- c('nj', 'vt')
#states2load <- c('nj')

sdirs <- paste0(sddir, states2load)

# state-level historical districts, sldsts
sldsts <- sdirs %>%
  list.files(
     recursive = T
    ,pattern = 'shp$'
    ,full.names = T
  ) %>%
  map( st_read ) %>%
  set_names( states2load  )

sldsts <- sldsts %>%
  map( ~rename_with(.x, tolower) )

#sldsts$nj


## natl spatial data -----------------------------------------------------------

# geodatabase path
# was downloaded from https://irma.nps.gov/DataStore/Reference/Profile/2297306
gdpth <- list.dirs(
  paste0(ddir,'gis/'),
  recursive = T
  #,pattern = 'gdb$'
  ,full.names = T)

gdpth <- gdpth[grepl('gdb$', gdpth)]

lyrs <- gdpth %>% st_layers()
lyrs

# use wkt spatial filter to read only for a specific area and do data checks:
# bounding box:
fcos <-
  map_dfr(
    #c('NY', 'PA')
    states2load
    ,
    ~tigris::counties(state = .x
                      , year = 2019)
  ) %>%
  rename_with(tolower)

#fcos[1:2] %>% plot()

# filter to a subset
#fcos <- fcos %>%  filter(statefp == 50)
#filter(grepl('Kings$|Queens$|^New York$|^Bronx$|^Richmond$', name))

bbx <- fcos %>%
  st_bbox() %>%
  st_as_sfc()

# wkt filter:
wktf <- st_as_text(bbx)

# nrcrs: national register "cultural resources" -- term they use.
# reading w or w/o spatial filter:
ncrs <-
  map(
    lyrs$name[1:10]
    , ~st_read(
      gdpth
      ,layer = .x
      ,wkt_filter = wktf
    )
  ) %>%
  set_names(
    lyrs$name[1:10]
  )

ncrs <- ncrs %>%
  map( tibble ) %>%
  map( ~rename_with(.x, tolower) ) %>%
  map( ~rename(.x, geometry = shape) )

ncrs <- ncrs %>%
  map( ~rename_with(.x, tolower) )

# drop empty layers
ncrs <- ncrs[lengths(ncrs) > 0]
ncrs$crdist_py%>% glimpse()
# just keep resnames, property ids, and layer names, bind to single sf object
ncr <- ncrs %>%
  imap_dfr( ~{.x %>%
      select(resname, nr_propertyid, geometry) %>%
      mutate(lyr = .y , .before = geometry)
  })

# only polygon version of natl register places
#ncr %>% tibble( ) %>% count(lyr)
ncrpy <- ncr %>%
  filter(grepl('py$', lyr))


# natl attributes data ----------------------------------------------------

# this is what has the # of bldgs affected by historic designation
attrs <- st_read(
    gdpth
    ,layer = lyrs$name[11])

attrs <- attrs %>% tibble() %>% rename_with(tolower)

attrs %>% glimpse()
attrs

# map together ------------------------------------------------------------

library(mapview)

# statelvl
#sldsts[[1]] %>% mapview()

# clean state-lvl data ----------------------------------------------------

# (relatively generizable script at this point; remind what state youre looking
# at:)
states2load

tmp <- sldsts[[1]]

# drop columns where all values identical.
tmp <- tmp %>%
  select( where( ~length(unique(.x)) > 1 ))

tmp %>% tibble() %>% glimpse()
nrow(tmp)

tmp %>%
  tibble() %>%
  taux::sum.NAs(T)



# Jersey-specific analysis / map comparison  ---------------------------------

#' reminder: metadata is useful.
#' https://www.arcgis.com/sharing/rest/content/items/7b83f7b8348d473bb6c610a8ef57808d/info/metadata/metadata.xml?format=default&output=html
#'
#' Some colms: STATUS is important. Values:
#'
#' NHL - National Historic Landmark
#'
#' LISTED - Listed in the New Jersey and/or National Registers of Historic
#' Places
#'
#' ELIGIBLE - "Formally determined eligible for listing in the New Jersey and/or
#' National Registers of Historic Places* (SHPO Opinion, COE, DOE)"
#'
#' *Note difference between NHL and National Register Historic Places:
#' https://www.nps.gov/subjects/nationalhistoriclandmarks/faqs.htm#:~:text=All%20National%20Historic%20Landmarks%20are,of%20state%20and%20local%20significance.
#' All NHL landmarks are in the national register, but it's a more significant
#' designation.
#'
#' LOCALLY_DESIGNATED - Designated as an historic district by municipal
#' ordinance.
#'
#' DELISTED - Removed from NJ/National Registers
#'
#' NOT_ELIGIBLE -  Formally determined not-elibile for listing in the New Jersey
#' and/or Natonal Registers of Historic Places --- ONLY FOR RESOURCES FORMERLY
#' OPINIONED AS ELIGIBLE NOW FORMALLY DE-OPINIONED

njdsts <- sldsts$nj
njdsts <- njdsts %>% select( where( ~length(unique(.x)) > 1 ))
njdsts

# njdsts %>%
#   tibble() %>%
#   select(-matches('id$|name|shape|geometry|coord')) %>%
#   taux::count.across()

# simplify
#install.packages('rmapshaper')
stmp <- njdsts %>% rmapshaper::ms_simplify()
#stmp <- stmp %>% st_transform(st_crs(st_sf(ncrpy)))

statesf <- tigris::states() %>%
  rename_with(tolower) %>%
  st_transform(st_crs(st_sf(ncrpy)))

njstate <- statesf %>%
  filter(grepl('NJ', stusps) )

# filter NatReg data to jersye better
njcrspy <- ncrpy %>%
  filter( lyr =='crdist_py' ) %>%
  st_sf() %>%
  st_filter(njstate)

# add attrs with # of contributing x
tmp <- attrs  %>%
  select(property_id, status, status_date
         ,matches('^number_of'))
tmp

njcrspy <- njcrspy %>%
  left_join(tmp
            ,by = c('nr_propertyid' =
                      'property_id' ))

# create map that does Jersey along with national
njdsts %>%
  select( -matches('note')) %>%
  filter( !grepl('DELISTED|NOT_ELIGIBLE', status)) %>%
  mapview( .
           ,zcol = 'status'
           ,layer.name = 'Jersey state-level data') +
  ( mapview( #njcrspy
             st_boundary(njcrspy)
             #,zcol = 'lyr'
             ,lwd = 4
             #,col.regions = 'grey90'
             ,color = '#FF7F50'
             ,layer.name = 'National Register data')) +
  mapview(st_boundary(njstate)
          ,color = 'grey35'
          ,layer.name = 'state boundaries')



tmp %>%
  head(300) %>%
  mapview(zcol ='status')


# Vermont-specific analysis  -----------------------------------------------

#' vt link:
#' https://geodata.vermont.gov/datasets/ee5cdb1b9c094139ad00f7f02785d2b2_21/about
#'
#' metadata:
#' https://www.arcgis.com/sharing/rest/content/items/ee5cdb1b9c094139ad00f7f02785d2b2/info/metadata/metadata.xml?format=default&output=html


# state-level historical districts, sldsts
names(sldsts)
vtdsts <- sldsts$vt %>% rename_with(tolower)


vtdsts %>% glimpse()
# rpc is which "regional planning commission"
vtdsts %>% tibble() %>% count(rpc)
njdsts %>% glimpse()


## load natl register for VT -----------------------------------------------

vtstate <- statesf %>%
  filter(grepl('VT$', stusps))



# filter NatReg data to jersye better
vtcrspy <- ncrpy %>%
  filter( lyr =='crdist_py' ) %>%
  st_sf() %>%
  st_make_valid() %>%
  st_crop(vtstate) %>%
  st_filter(vtstate)

# add attrs with # of contributing x
tmp <- attrs %>%
  select(property_id, status, status_date
         ,matches('^number_of'))
tmp

vtcrspy <- vtcrspy %>%
  left_join(tmp
            ,by = c('nr_propertyid' =
                      'property_id' ))


# create map that does Vermont along with national
mapview( vtdsts
           #,zcol = 'status'
         ,color = visaux::jewel.pal()[1]
           ,layer.name = 'Vermont state-level data') +
  ( mapview( #njcrspy
    st_boundary(vtcrspy)
    #,zcol = 'lyr'
    ,lwd = 4
    #,col.regions = 'grey90'
    ,color = '#FF7F50'
    ,layer.name = 'National Register data')) +
  mapview(st_boundary(vtstate)
          ,color = 'grey35'
          ,layer.name = 'state boundaries')
