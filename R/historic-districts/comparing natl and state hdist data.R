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

ncrpy <- ncrpy %>% st_sf() %>% st_make_valid()

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
#'
#' Major flag in the datasource
#' (https://njogis-newjersey.opendata.arcgis.com/datasets/njdep::historic-districts-of-new-jersey/about):
#'
#' "HPO is still in the process of comprehensive digitizing for categories 4 and
#' 5. Inclusion in this dataset does not preclude the existence of other
#' historic districts as yet unidentified, unrecorded, or undocumented."

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
library(mapview)

njdsts %>%
  select( -matches('note')) %>%
  #filter( !grepl('DELISTED|NOT_ELIGIBLE', status)) %>%
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



# tmp %>%
#   head(300) %>%
#   mapview(zcol ='status')



## only nat'l register data over NJ ----------------------------------------

ncrs$crdist_py %>% glimpse()
attrs %>% filter(property_id %in% ncrs$crdist_py$nr_propertyid) %>% glimpse()
attrs %>% count(status)

njcrspy %>%
  tibble() %>%
  select(matches('^number')) %>%
  map( summary )

njcrspy %>%
  mapview(zcol = 'number_of_contributing_buildings')


## data questions ----------------------------------------------------------

#' When a district is both national -and- local, how does it show up in the
#' data? If `Status` column is "LISTED," could it be local as well as State or
#' National?
#'
#' Note the content form the state DEP
#' (https://www.mounttabornj.org/wp-content/uploads/2015/04/State_National_Register_facts.pdf):
#'
#' "Local landmarks and historic district regulations that may affect private
#' property owner actions are completely separate from New Jersey and National
#' Register regulations. The New Jersey and National Registers provide a degree
#' of review and protection from public actions only."
#'
#' -> state listing is pretty similar to nat'l in that it has lower restrictiveness.
#'
#' Metadata says: "LISTED" means "Listed in the New Jersey and/or National
#' Registers of Historic Places"
#'
#' while "LOCALLY_DESIGNATED" means "Designated as an historic district by
#' municipal ordinance."
#'
#' Comparing the NR and NJ data, shows that sometimes local distrcts can also be
#' national, hopefully it's not also vice-versa..

njdsts %>% class()
njdsts <- njdsts %>% tibble()

njdsts %>% glimpse()


njdsts %>%
  count( in.nr = !is.na(nris_id)
        ,status)

# LISTED or NHL districts have non-NA designation dates for State/Local
# designations (with a few exceptions); local ones have NAs for those. That seems to imply
njdsts %>%
  count( natl.or.state =
           !is.na(nrdate) |
           !is.na(srdate)
         ,status)
# altho 1 exception: a LISTED district that doesn't have the s/r designation
# dates:
njdsts %>%
  filter(status == 'LISTED') %>%
  filter( is.na(nrdate) &
           is.na(srdate)
         ) #%>% st_sf() %>% mapview()

# checking locally designated ones..

# njdsts %>%
#   filter(grepl('LOCALLY_', status) ) %>%
#   select(matches('^nr|^local|^sr')) %>% View()

# some that are not locally designated but have a local designation date..?
njdsts %>%
  filter( !grepl('LOCALLY_', status) ) %>%
  filter( !is.na(localdate) ) %>% View()
  count( is.na(localdate))



njdsts %>% count(status)
njdsts %>% taux::sum.NAs(T)


# Vermont-specific analysis  -----------------------------------------------

#' vt link:
#' https://geodata.vermont.gov/datasets/ee5cdb1b9c094139ad00f7f02785d2b2_21/about
#'
#' metadata:
#' https://www.arcgis.com/sharing/rest/content/items/ee5cdb1b9c094139ad00f7f02785d2b2/info/metadata/metadata.xml?format=default&output=html
#'
#' Details on historic preservation in VT:
#' http://vpic.info/Publications/Reports/Implementation/Historic.pdf
#'
#' From above: section on Historic Districts and Design Control Districts -- the
#' zoning overlays that mandate design decisions are "local regulations" --
#' "Local regulations of this kind are by far the best way to ensure protection
#' of historic resources in a community"


# state-level historical districts, sldsts
names(sldsts)
vtdsts <- sldsts$vt %>% rename_with(tolower)


vtdsts %>% glimpse()
# rpc is which "regional planning commission"
vtdsts %>% tibble() %>% count(rpc)
vtdsts %>% glimpse()


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
    ,lwd = 6
    #,col.regions = 'grey90'
    ,color = '#FF7F50'
    ,layer.name = 'National Register data')) +
  mapview(st_boundary(vtstate)
          ,color = 'grey35'
          ,layer.name = 'state boundaries')
