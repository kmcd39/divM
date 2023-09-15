library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

rm(list = ls())


# load NJ places ----------------------------------------------------------

# acs.vars <- censusrx::pull.acs.metadata(2021)
# acs.vars %>%  write.csv(file = '~/R/local-data/acs-vars.csv')


plcpops <-
  tidycensus::get_acs(
     geography = 'place'
    ,variables = c('pop' = 'B01001_001')
    ,year = 2021
    ,state = 'nj'
    ,geometry = T
  ) %>%
  rename_with(tolower)

plcpops$estimate %>% quantile(seq(0,1,.2))


# hdist load data ---------------------------------------------------------------

ddir <- '~/R/local-data/historic-places/'
list.files(ddir)

## state lvl ---------------------------------------------------------------

# state-lvl directory
sddir <- paste0(ddir, 'gis/state-level/nj/')


# state-level historical districts, sldsts
njdsts <- sddir %>%
  list.files(
    recursive = T
    ,pattern = 'shp$'
    ,full.names = T
  ) %>%
  st_read() %>%
  rename_with(tolower)

njdsts

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
  tigris::counties(state = 'nj'
                      , year = 2021) %>%
  rename_with(tolower)

#fcos[1:2] %>% plot()

bbx <- fcos %>%
  st_bbox() %>%
  st_as_sfc()

# wkt filter:
wktf <- st_as_text(bbx)

# nrcrs: national register "cultural resources" -- term they use.
# reading w or w/o spatial filter:
ncrs <-
  lyrs$name[1:10] %>%
  map(
    ~st_read(
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
  map( ~rename(.x, geometry = shape) ) %>%
  map( ~rename_with(.x, tolower) )

# drop empty layers
ncrs <- ncrs[lengths(ncrs) > 0]
names(ncrs)
ncrs$crdist_py %>% glimpse()

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

### natl attributes data ----------------------------------------------------

# this is what has the # of bldgs affected by historic designation
attrs <- st_read(
  gdpth
  ,layer = lyrs$name[11])

attrs <- attrs %>% tibble() %>% rename_with(tolower)

attrs %>% glimpse()
attrs


## local level data --------------------------------------------------------

library(mapview)

# look at higher population NJ cities to get a sense of where i should maybe
# pull some local-level historic districts:
plcpops %>% arrange(desc(estimate))
plcpops %>% mapview(zcol = 'estimate')

#' Paterson has historic preservation elements through their planning process:
#'
#' https://www.patersonnj.gov/egov/documents/1395859208_441833.pdf
#'
#' it has 4 preservation Districts -- 1 of which is a canal and the other 3 are
#' built areas. It seems they are working on an opendata portal that is not yet
#' live, so maybe will manually check those 3 for now.
#'
#' Elizabeth doesn't have them online either; i doubt smaller places would
#'
#' State-wide document -- it's dated but lists some municipalities that do local
#' hdists: https://www.nj.gov/dep/hpo/hpo_article.pdf -- notes that state
#' designations don't mean local designations but 'coordination should be
#' encouraged.'
#'


### load local lvl data -----------------------------------------------------

ddir <- '~/R/local-data/historic-places/local-districts/nj/'

ldcities <-
  list.dirs(ddir
            ,full.names = F
            ,recursive = F)

pths <-
  list.files(
   ddir
  ,full.names = T
  ,recursive = T
  ,pattern = 'shp$'
)

pths %>% str_extract(paste(ldcities, collapse = '|'))


localdists <- pths %>%
  set_names( ldcities ) %>%
  imap( ~st_read(.x) ) %>%
  map( ~rename_with(.x, tolower) )

# localdists$camden %>% mapview()
# localdists %>% map(st_crs)

localdists <-
  localdists %>%
  imap_dfr(
    ~select(.x, name, geometry) %>%
          mutate(city = .y
                 ,.before = everything() ) %>%
      st_transform(4326) )

localdists



# review statewide data categories ----------------------------------------

#' analysis in "comparing natl and state hdist data," Jersey-specific analysis.
#'
#'
njdsts <- njdsts %>% st_sf() %>%  st_transform(4326)
njdsts <- njdsts %>% tibble()

njdsts %>% glimpse()
njdsts %>% select(matches('status|date'))

njdsts %>% count(status) %>% arrange(desc(n))

njdsts %>% filter(!is.na(localdate)) %>%  count(status) %>% arrange(desc(n))

#njdsts %>% filter(is.na(srdate) & is.na(nrdate)) %>%  count(status) %>% arrange(desc(n))

# get a selection of local districts (or plausibly local districts?) and tighter colm selection
seldsts <-
  njdsts %>%
  filter(!is.na(localdate)) %>%
  st_sf() %>%
  select(objectid, nris_id,
         name, status, demolished,
         nhl, 'bound_qual',
         matches('date'), geometry)


seldsts %>% mapview(zcol = 'status')

# map of local dists and statewide ----------------------------------------


# Places (cities) for which i loaded data
plcregx <- paste0('^', ldcities) %>% paste0(collapse = '|')
plcdata <-
  plcpops %>%
  filter(
    grepl(
       plcregx
      ,name
      ,ignore.case = T )
    ) %>%
  st_transform(4326)

# remove Camden for now
plcdata <- plcdata %>% filter(!grepl('Camden', name))


# scales::show_col(visaux::jewel.pal())

mapview(localdists
        ,col.regions = visaux::jewel.pal()[4]
        ,layer.name = 'local data'
        ) +
  mapview(st_boundary(seldsts)
          #,zcol = 'status'
          ,color = visaux::jewel.pal()[5]
          ,layer.name = 'state data'
          ,lwd = 4) +
  mapview(  st_boundary(plcdata)
            ,layer.name = 'city boundaries'
            )


# districts in state data w/o filtering based on local designation date or status...

seldsts2 <- njdsts %>%
  select(objectid, nris_id,
         name, status, demolished,
         nhl, 'bound_qual',
         matches('date'), geometry) %>%
  st_sf() %>%
  st_make_valid() %>%
  st_filter(
    st_as_sfc(
      st_bbox(plcdata) )
    )

# based on this, it seems like LISTED also counts as local district, even when
# there's no local desgination date:
mapview(localdist
        ,col.regions = visaux::jewel.pal()[4]
        ,layer.name = 'local data'
) +
  mapview(st_boundary(seldsts2)
          #,zcol = 'status'
          ,color = visaux::jewel.pal()[5]
          ,layer.name = 'state data'
          ,lwd = 4) +
  mapview(  st_boundary(plcdata)
            ,layer.name = 'city boundaries'
  )


# map of local dists and natl --------------------------------------------

# natl dists w/in those cities:
selnatl <- st_sf(ncrs$crdist_py) %>%
  st_transform(4326)
#%>%
#  st_filter(plcdata)

# trim colms
selnatl %>% glimpse()
selnatl <- selnatl %>%
  select(cr_id, resname, source)


# version with string filtering
selnatl %>%
  select(resname, source)
  #filter(grepl('Historic District', resname))

selnatlf <- selnatl %>%
  #filter(grepl('Historic District', resname))
  filter(grepl('District', resname))

# map
mapview(localdists
        ,col.regions = visaux::jewel.pal()[4]
        ,layer.name = 'local data'
        ) +
  mapview(st_boundary(
    selnatlf)
    #,zcol = 'status'
    ,layer.name = 'national register data'
    ,color = visaux::jewel.pal()[5]
    ,lwd = 4) +
  mapview( st_boundary(plcdata)
           ,layer.name = 'city boundaries'
           )


# gosh lol. It seems there is a BETTER DIFFERENT source for NJ state level data

# https://njdep.maps.arcgis.com/apps/webappviewer/

