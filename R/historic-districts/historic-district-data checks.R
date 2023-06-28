library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

rm(list = ls())

# load data ---------------------------------------------------------------

ddir <- '~/R/local-data/historic-places/'
list.files(ddir)

## csv ---------------------------------------------------------------------

# nara <- list.files(ddir,
#                    recursive = T
#                    ,pattern = 'xlsx$'
#                    ,full.names = T) %>%
#   readxl::read_xlsx()
#
# nara <- nara %>%
#   rename_with( tolower ) %>%
#   rename_with( make.names )
#
# nara


## gdb ---------------------------------------------------------------------

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

#attrlyr <- lyrs %>% filter(is.na(geometry_type))  # may open issue w sf that this doesn't work...


# # to read all:
# ncrs <-
#   map( #lyrs$name[1:2]
#         lyrs$name
#       , ~st_read(gdpth
#                  ,layer = .x)
#        ) %>%
#   set_names(lyrs$name#[1:2]
#             )


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

# reading w spatial filter:
ncrs <-
  map( #lyrs$name[1:2]
    lyrs$name[1:10]
    , ~st_read(
      gdpth
      ,layer = .x
      ,wkt_filter = wktf)
  ) %>%
  set_names(
    lyrs$name[1:10]
  )

# the layer that lacks geometries
attrs <-
  st_read(
  gdpth
  ,layer = lyrs$name[11])

attrs <- attrs %>% tibble() %>% rename_with(tolower)
# (i suspect identical to the NARA excel file? -- no, it's not quite identical.)


# check files -------------------------------------------------------------

names(ncrs)

ncrs <- ncrs %>%
  map( ~rename_with(.x, tolower) )

map_dbl(ncrs, ncol)

ncrs$crbldg_py %>%
  colnames()

# # check columns where all values are identical or NA:
# ncrs$crbldg_py %>%
#   tibble() %>%
#   select( where( ~length(unique(.x)) == 1 )) %>%
#   distinct() %>%
#   View()


# to drop those columns from all layers:
ncrs <- ncrs %>%
  map( ~{.x %>%
    tibble() %>%
      select( -where( ~!length(unique(.x)) > 1 ))
  })


# just keep property ids and layer names, bind to single sf object
ncrs <- ncrs[lengths(ncrs) > 0]
ncr <- ncrs %>%
  imap_dfr( ~{.x %>%
      select(resname, nr_propertyid, geometry = shape) %>%
      mutate(lyr = .y)
    })


# exploratory plots -------------------------------------------------------

#install.packages('mapview')
library(mapview)

ncr %>% st_sf() %>% mapview(zcol = 'lyr')

## checking what could be added from attrs ---------------------------------------

attrs
attrs %>% glimpse()

attrs %>%

# are there duplicated property ids?
attrs$property_id %>% duplicated() %>% sum()

# # is it b/c the status changed or smthng?
# attrs %>%
#   filter(property_id %in%
#            attrs$property_id[duplicated(attrs$property_id)]
#          ) %>%
#   arrange(property_id) %>%
#   select(property_id, everything()) %>%
#   View()

# seems to be from "boundary increases" , or "addtional documentation" or similar.

attrs %>%
  taux::count.across(matches('request.type|status$'))

# how many / proportion of NAs for the "number of contributing" etcs columns?
attrs %>%
  select(matches('^number')) %>%
  taux::sum.NAs(perc = T)

# attrs$name_of_multiple_property_listing


# join attr information to geometries
ncra <- ncr %>%
  left_join(attrs
            ,by = c('nr_propertyid' = 'property_id'))

# plot w attrs
ncra %>%  st_sf() %>% mapview(zcol = 'lyr')


# notes on identifying historic districts ---------------------------------

#' they seem to be only really identified by "District" in the resname /
#' property name columns.
#'
#' I think all in crdist_py layer of the original geodatabase.
#'
#' Most historic districts end with "Historic District"; however, some, llike
#' Southwark or Fairview in Philly/Camden only end in "District".
#'
#' Additionally -- b/c districts may be duplicated for boundary increases, I
#' should remove duplicated by property_id by filtering to most recent status_dates.
#'
#' More analysis on separating out historic districts:

# ncra$status_date[ncra$status_date == max(ncra$status_date)]

# look at duplicates
ncra %>%
  filter(nr_propertyid %in%
           nr_propertyid[duplicated(nr_propertyid)]
  ) %>%
  arrange(nr_propertyid) %>%
  select(nr_propertyid, everything()) %>%
  View()

# check duplicates by resname? above seems to not quite catch everything,
# although that could be fine.
ncra %>%
  filter(resname %in%
           resname[duplicated(resname)]
  ) %>%
  arrange(resname) %>%
  select(nr_propertyid, resname, everything()) %>%
  View()

# removing duplicates
ncrar <- ncra %>%
  group_by(nr_propertyid) %>%
  #group_by(resname) %>%
  filter(status_date ==
           max(status_date)) %>%
  ungroup()

ncrar
ncra

# separating out districts
dists <- ncrar %>%
  filter(grepl('District'
               , property_name))

# what layers do districts tend to be in?
dists %>% count(lyr)

# check completeness for "number of contributing etc." columns from attrs.
dists %>% select(matches('^number')) %>%
  taux::sum.NAs(perc = T)

# other peeks
dists %>% count(status)
dists %>% count(request_type)

dists %>% st_sf() %>% mapview(zcol = 'number_of_contributing_buildings')

# i should also check how many properties in the csv don't match the GIS data -- which
# would reflect degree of incompleteness of gis data (noted in FAQ/below)


# other important notes ---------------------------------------------------

#' data has some important issues. see the NR_FAQ:
#'
#' "Is the data from NPS complete and up-to-date?
#'
#' "The data made available to the public in both GIS (geodatabase) format and
#' via data services through nps.gov are NEITHER up-to-date nor complete with
#' regards to the current state of the National Register of Historic Places
#' (FEBRUARY 2020). More detail can be found in this document. Much of the data
#' was first generated in early 2012 and a small update was added in 2014.
#' However, from 2014- 2017, the Nation Register’s database was taken offline to
#' be modernized and, in the process, it was impossible to attempt additional
#' data updates. In late 2017, an effort was made to try to again update the
#' data. However, due to many of the issues described below, particularly with
#' respect to accuracy of polygon data, only some of the information recorded as
#' points was included. Efforts to continue to update this data using the
#' National Register’s database have been temporarily discontinued until CRGIS
#' and the NR Office can develop a robust plus to address the overall content
#' and accuracy issues of the data in a sustainable manner."
#'
#' -in changelog, additional places were added in 2020. It seems the data would
#' be relatively complete for places added up to 2012, with ~some~ additional
#' places that have been added since then.
#'
#' -Also the polygons are "rough envelopes", reflecting the poor GIS technology of
#' the legacy system this was built from....

#' Also in the FAQ:
#'
#' "However, the number of problems related to the polygon data (in particular)
#' exists that goes beyond the resources of our staff to address them.
#' Additionally, it may not be fully worth the effort as we believe the more
#' appropriate thing to do would be to invest the resources into creating GIS
#' data that displays the intended legal boundaries of the properties. We are
#' attempting to work with selected partners and other trusted sources to begin
#' attempts to correct some of the data. We currently have no timeline on when
#' this will be complete or what the final product will contain but we hope to
#' have updates on our status soon."
#'
#' -the "street___number" for the historic districts gives a verbal description
#' of its boundaries that are more accurate and precise than the polygons in the
#' spatial data.. which is unfortunate. Maybe some kind of possibilities to make
#' measures more precise using natural language processing to parse those verbal
#' descriptions and generate better polygons--- but that'd be a lift.


#' On why some districts are points:
#'
#' "Typically, if a property is less than 10 acres in size, nominators were only
#' required to submit point coordinates for the primary resource. CRGIS hopes to
#' eventually create polygons for these properties using the legal, verbal
#' boundary descriptions as guidance."




# comparing NARA excel file and attributes from the gdb -------------------

nara <- list.files(ddir,
                   recursive = T
                   ,pattern = 'xlsx$'
                   ,full.names = T) %>%
  readxl::read_xlsx()

nara <- nara %>%
  rename_with( tolower ) %>%
  rename_with( make.names )

nara

attrs

# interesting, attrs actually has more records.
nara %>% glimpse()

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

mergd %>% View()
mergd %>% count(category.of.property)

# how many with the "district" property category in the excel file don't have
# "District in the name" -- (there are some! Most of them are just large single
# properties, like farms or plantations, others are historic districts in our
# understanding: like German Village, Ohio.)
mergd %>%
  filter(
    tolower(category.of.property) == 'district'
  ) %>%
  filter(!grepl('District', property.name)) %>% View()


# check the reverse: how many non-distrcts by category have the word in their
# name?
mergd %>%
  filter(
    tolower(category.of.property) != 'district'
  ) %>%
  filter(grepl('District', property.name)) %>%
  select(category.of.property, everything()) %>% View()
# oooh, the searching by district will get a bunch of thinks with "District of
# Columbia" in the names too.

# how many rows with missing properties may be historic districts?
mergd %>%
  filter(
    is.na(category.of.property)
  ) %>%
  filter(grepl('District', property.name)) %>%
  select(category.of.property, everything()) %>% View()

nara %>% glimpse()
mergd %>% filter(grepl('Meeker Historic District', property.name))

# km summary of data quality issues/possibilities ---------------------------------------


# issues:

#' polygons are coarse, approximate, flawed. The FAQ talks about reasons for
#' this, including poor gis technology that undergirded legacy systems, typos,
#' undocumented 3rd-party data cleaning processes, etc.
#'
#' The data is not complete. In particular, historic districts added since 2012
#' will have spotty representation.
#'
#' Some historic districts are represented as points, but "typically" only those
#' that are <10 acres large. Point data will not have a calculable area.
#'
#'

# possibilities:

#' There is data on "number of buildings affected" that seems to be in
#' potentially better shape than the polygons/areas defining historic districts.
#' These could be used to generate and alternate/supplemental set of measures.
#'
#' Historic districts also have verbal descriptions of their boundaries in the
#' "street_number" column. But it'd be very difficult to turn these into
#' cleaner, workable versions of the data.




