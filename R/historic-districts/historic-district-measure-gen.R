library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

rm(list = ls())


# notes -------------------------------------------------------------------

#' desired measures (from p sharkey):
#'
#' 1) I'd like to develop a file that includes every *historic district* in the
#' country (not every historic building), including the following information:
#' land area / zip / city or town (possibly census place) / tract / county /
#' state in which it is located / year it was created
#'
#' 2) and then I'm hoping to turn it around and develop measures at the level of
#' tracts, places, counties, etc. e.g. what percentage of land at each level is
#' part of a historic district (and perhaps the year the largest district was
#' formed).
#'
#'

#' workflow:
#'
#' load the geodatabase, link to the excel file for the NPS national archives
#' (NARA). Linking will happen by way of NR_Main in the NPH geodatabase because
#' different IDs are used in the different files, but that file has both.
#'
#' Filter to districts by string matching "Historic District" OR
#' category.of.property in NARA is a district.
#'
#' Remove duplicates by propertyid, filtering to more recent record for areal
#' measures and earliest record for "when it was created."
#'
#'


# create historic districts data set --------------------------------------

ddir <- '~/R/local-data/historic-places/'
list.files(ddir)

## load NARA excel file ----------------------------------------------------

nara <- list.files(ddir
                   ,recursive = T
                   ,pattern = 'xlsx$'
                   ,full.names = T) %>%
  readxl::read_xlsx()

nara <- nara %>%
  rename_with( tolower ) %>%
  rename_with( make.names )

nara

## load geodatabase ---------------------------------------------------------

# geodatabase path
# was downloaded from https://irma.nps.gov/DataStore/Reference/Profile/2297306
gdpth <- list.dirs(ddir,
                   recursive = T
                   #,pattern = 'gdb$'
                   ,full.names = T)
gdpth <- gdpth[grepl('gdb$', gdpth)]

lyrs <- gdpth %>% st_layers()
lyrs

# separate out layer that doesn't have geometries

# (below doesnt work; may open issue w sf that it doesn't...) Instead just use indexing.
#attrlyr <- lyrs %>% filter(is.na(geometry_type))

# use wkt spatial filter to read only for a specific area and do data checks:
# bounding box:
fcos <-
  map_dfr(
    c('NY', 'PA'),
    ~tigris::counties(state = .x
                      , year = 2019)
  ) %>%
  rename_with(tolower) %>%
  filter(grepl('Philadelphia|Kings$|Queens$|^New York$|^Bronx$|^Richmond$', name))

#filter(grepl('Kings$|Queens$|^New York$|^Bronx$|^Richmond$', name))
fcos
bbx <- st_bbox(fcos) %>% st_as_sfc()
# wkt filter:
wktf <- st_as_text(bbx)

# reading w or w/o spatial filter:
ncrs <-
  map(
    lyrs$name[1:10]
    , ~st_read(
      gdpth
      ,layer = .x
      #,wkt_filter = wktf
      )
  ) %>%
  set_names(
    lyrs$name[1:10]
  )

ncrs <- ncrs %>%
  map( tibble ) %>%
  map( ~rename_with(.x, tolower) ) %>%
  map( ~rename(.x, geometry = shape) )

# the layer that lacks geometries
attrs <-
  st_read(
    gdpth
    ,layer = lyrs$name[11])

attrs <- attrs %>% tibble() %>% rename_with(tolower)
#(these are not the same as the NARA file, but have potentially useful info on #
#of bldgs affected by historic designation, for example.) )



# initial trims of spatial data -------------------------------------------

ncrs$crbldg_py

ncrs <- ncrs %>%
  map( ~rename_with(.x, tolower) )

# # check columns where all values are identical or NA:
ncrs %>%
  map(
    ~{.x %>%
        tibble() %>%
        select( where( ~length(unique(.x)) == 1 )) %>%
        colnames()
      })

# compared to columns w multiple distinct, non-missing values:
ncrs %>%
  map(
    ~{.x %>%
        tibble() %>%
        select( where( ~length(unique(.x)) > 1 )) %>%
        colnames()
    })


# drop empty layers
ncrs <- ncrs[lengths(ncrs) > 0]

# just keep resnames, property ids, and layer names, bind to single sf object
ncr <- ncrs %>%
  imap_dfr( ~{.x %>%
      select(resname, nr_propertyid, geometry) %>%
      mutate(lyr = .y , .before = geometry)
  })




# add attribute info ------------------------------------------------------

# note the "reference #" is nearly but not always equal to property id. The NARA
# csv only has the ref number it seems, while the GIS data only has the prop id.
attrs %>% filter(ref_ != property_id)


# drop attrs where is says removed..
attrs <- attrs %>%
  filter(
    !grepl('Removed', status, ignore.case = T)
         )
# filter to most recent (or earlist) record chagne per district/property
max(attrs$status_date) # most recent

## merge attrs from geodatabase to NARA records ---------------------------

# confusing how these are mostly but not always the same... I wish this were
# better documented.
attrs %>%
  select(ref_, property_id) %>%
  filter(ref_ != property_id)

nara$reference.number

mergd <- nara %>%
  mutate(
    reference.number =
      gsub('^_', '', reference.number)
  ) %>%
  full_join(attrs
            ,by = c('reference.number' = 'ref_'))

mergd %>% select(matches('x$|y$'))
# associated counties/cities occassionally change, probably from boundary shifts
# over time?
mergd %>%
  select(reference.number, matches('x$|y$')) %>%
  filter(tolower(state.x) != tolower(state.y) |
           tolower(county.x) != tolower(county.y) |
           tolower(city.x) != tolower(city.y) |
           tolower(status.x) != tolower(status.y)
         )
# also note (_ is from attrs)
mergd %>%
  filter(property.name != property_name) %>%
  select(property.name, property_name)

# redefine merged attributes using only colnames from the NARA csv, where they'd
# be in both places.

# first, rename attrs from geodatabase to be consistent w/ _s or .s as spaces.
attrs <- attrs %>%
  rename_with( ~gsub('_', '.', .x))

mergd <- nara %>%
  mutate(
    reference.number =
      gsub('^_', '', reference.number)
  ) %>%
  full_join( select(attrs, -any_of(colnames(nara)))
            ,by = c('reference.number' = 'ref.'))

nrow(mergd)
nrow(nara)

ncr <- ncr %>%
  left_join( mergd
            , by = c('nr_propertyid' = 'property.id'))

ncr
# filter to only districts
mergd %>% count(category.of.property)
mergd %>% count(area.of.significance)
mergd %>% filter( !is.na(park.name))
ncr %>% select(matches('^number')) %>% taux::sum.NAs(T)
ncr %>% select(matches('^number')) %>% summarise(across(everything(), ~sum(.x, na.rm = T)) )

ncr %>% select(resname, property.name) %>% filter(resname != property.name)

# filter to districts
dists <- ncr %>%
  filter(tolower(category.of.property) == 'district' |
           grepl('historic district', property.name, ignore.case = T) |
           grepl('historic district', resname, ignore.case = T)
           )

# (this is not a reliable way to filter out parks it seems, unfortunately)
# ncr %>% filter( !is.na(park.name)) %>% st_sf() %>% mapview()

dists <- dists %>%
  filter( !grepl('Park$', resname, ignore.case = T) )


dists %>%
# peek --------------------------------------------------------------------

dists$number.of.contributing.objects
dists %>% count
library(mapview)
dists %>% st_sf() %>% mapview()

# still huge data quality issues... look at the Downtown providence Historic
# district for example... one of the vertices was accidentally added in the next
# state... or the many areas in the middle of the ocean.

#' can maybe manually drop polygons w mistaken vertices where it's an issue:
#'
#' i see:
#'
#' downtown pvd.. also note how it seems like the " First Universalist Church"
#' (in other names) was mistakenly entered as the Downtown pvd boundary
#' increase..
#'
#' Owosso Downtown Historic District (nr_propertyid - 14000126)
#'
#' ...there's tbh a lot

# # to move to earliest/latest record per property:
# latest.ncr <- ncr %>%
#   group_by(nr_propertyid) %>%
#   filter(status_date ==
#            max(status_date) ) %>%
#   ungroup()
#
#
# earliest.ncr <- ncr %>%
#   group_by(nr_propertyid) %>%
#   filter(status_date ==
#            min(status_date) ) %>%
#   ungroup()
#
# # some duplicate property ids still:
# latest.ncr %>% filter(nr_propertyid %in% nr_propertyid[duplicated(nr_propertyid)]) %>%
#   arrange(nr_propertyid)

# merge to NARA records



ncr <-





# scratch -----------------------------------------------------------------


bbx <- bbx %>%  st_transform(st_crs(st_sf(ncr)))







