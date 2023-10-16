library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

rm(list = ls())

#' notes:
#'
#' rename this script, original pad-analysis became measure gen; it's there
#' becuase i'm just doing it locally...
#'
#' Also remember source for additional metadata:
#' https://www.protectedlands.net/help/
#'
#' The "Technical How-Tos" in that site has "Detailed Descriptions of Layer
#' Files" which is helpful, as well as "Tips for GIS Users"; in particular ---
#'
#' "A: Manager-based mapping is highly recommended in PAD-US, as the manager
#' field (Mang_Name) is more completely filled in than the owner field (work is
#' underway on the owner field). The best approach here may be to use one of the
#' pre-made PAD-US layer files: “Mid Agency Level” and “Fine Agency Level” layer
#' files have individual agency names for Federal managers and generic names for
#' other managers; “General Agency Level” has generic level names for all
#' managers (Federal, State, etc.). You can of course choose “Mang_Name” as the
#' field to categorize in any GIS software and define your own colors."
#'




# load data ------------------------------------------------------

## area geos ---------------------------------------------------------------

statefips <- '44' # RI


statesf <- tigris::states(year = 2021)
statesf <- statesf %>%
  rename_with(tolower) %>%
  filter(statefp == statefips)

# see https://github.com/pauldzy/USGS_Albers_Equal_Area_Projections for EPSG.
bbx <- statesf %>%
  st_transform(5070) %>%
  st_bbox()

st_crs(5070)

wktf <- bbx %>% st_as_sfc() %>% st_as_text()

# cts as well
fcts <- tigris::tracts(
  state = statefips
  ,year = 2021
) %>%
  rename_with(tolower) %>%
  st_transform(5070) %>%
  select(1:4, aland, awater, geometry)

# remove only-water CTs
fcts <- fcts %>%
  filter(aland > 0 )

## PAD ---------------------------------------------------------------------

ddir <- '~/R/local-data/usgs/PADUS3_0Geodatabase/'
gdb <- ddir %>% list.files(pattern = 'gdb$', full.names = T)

lyrs <- gdb %>% st_layers()
lyrs

# read Pad fees & easements
pfe <-
  map(lyrs$name[c( 11, 13 )]
      , ~st_read(
        gdb
        , layer = .x
        , wkt_filter = wktf # over sample area
      )
  ) %>%
  set_names( lyrs$name[c( 11, 13 )] )
# i get a warning/GDAL error reading..

pfe <- pfe %>%
  map( tibble ) %>%
  map( ~rename_with(., tolower) ) %>%
  map( ~rename(., geometry = shape) )


# interestingly, the columns are different than in the combined file.
pfe %>% map(colnames)
# pfe$PADUS3_0Fee %>% count(featclass)
# pfe$PADUS3_0Easement %>% count(featclass)
#
# pfe$PADUS3_0Fee %>%
#   filter(category != 'Fee') %>% View()


## bring in metadata -------------------------------------------------------

# map column name in the data to metadata name; can skip 8, which is just state
# names. Note Agency name and type match either Owner or Manager in the spatial
# data.
metadata2colm <- tibble(
  metadata.nm = lyrs$name[1:7]
  ,colm.nm = Hmisc::Cs(pub_access, featclass, des_tp, gap_sts, iucn_cat, agency_name, agency_type)
)

# create an organized list of dataframes for every metadata layer
metadata <- lyrs$name[1:7] %>%
  map2( metadata2colm$colm.nm
        , ~{st_read(gdb, layer = .x) %>%
            select(!!.y := 1, !!.x := 2)
        }
  ) %>%
  set_names(lyrs$name[1:7])

metadata$Agency_Name
metadata$Agency_Type

# data cleans and preps ---------------------------------------------------

## trim colms and rbind ----------------------------------------------------

keep.cols <- Hmisc::Cs(featclass, mang_type,
                      loc_mang, loc_ds,
                      unit_nm,
                      src_date, date_est,
                      gis_acres,
                      pub_access, gap_sts,
                      des_tp, geometry )

pfe <- pfe %>%
  map_dfr( ~select(  tibble(.x)
                   , all_of(keep.cols)) )

# spatial cleans ----------------------------------------------------------

#' cast to polygons (get rid of multisurface)
#'
#' set precision seems to help avoid invalid geometries resulting from the
#' difference.
geo.precision <- units::set_units(1, 'meters')

pfe <- pfe %>%
  st_sf() %>%
  st_transform(5070) %>%
  st_cast('MULTIPOLYGON') %>%
  st_set_precision(
    geo.precision ) %>%
  st_make_valid()


# pfe <- pfe %>% map( st_simplify )


## removing overlaps -------------------------------------------------------

pfed <- pfe %>%
  arrange(desc(featclass)) %>%
  st_difference()

### for sample area -------------------------------------------------------------

#' library(mapview)
#'
#' # use a sample area to visualize over
#' fcos <- tigris::counties(year = 2021
#'                          ,state = statefips) %>%
#'   rename_with(tolower) %>%
#'   st_transform(5070)
#'
#' # fcos %>% mapview(zcol = 'name')
#'
#' crop.bbx <- fcos %>%
#'   filter(name == 'Washington') %>%
#'   st_bbox()
#'
#' tmp <- pfe %>%
#'   st_sf() %>%
#'   st_crop(crop.bbx) %>%
#'   st_make_valid()
#'
#' # call st_difference
#'
#' #' from fcn documentation:
#' #'
#' #' "When st_difference is called with a single argument, overlapping areas are
#' #' erased from geometries that are indexed at greater numbers in the argument to
#' #' x; geometries that are empty or contained fully inside geometries with higher
#' #' priority are removed entirely. The st_difference.sfc method with a single
#' #' argument returns an object with an "idx" attribute with the orginal index for
#' #' returned geometries."
#' tmpd <- tmp %>%
#'   arrange(desc(featclass)) %>%
#'   st_difference()
#'
#' # compare total rows and n acres (We want easements to decrease, and fees to
#' # likely decrease by a smaller amount)
#' tmp %>%
#'   tibble() %>%
#'   group_by(featclass) %>%
#'   summarise(n = n()
#'             ,acres = sum(gis_acres))
#'
#' tmpd %>%
#'   tibble() %>%
#'   group_by(featclass) %>%
#'   summarise(n = n()
#'             ,acres = sum(gis_acres))

# # visual checks -- tmpd should have no overlaps.
# #install.packages('leaflet.extras2')
# library(leaflet.extras2)
# mapview(tmp, zcol = 'featclass') |
#   mapview( tmpd, zcol = 'featclass')



## remove water from tracts/nbhds ------------------------------------------

cofps <- fcts$countyfp %>% unique()

wtr <-
  map_dfr(
    cofps,
    ~tigris::area_water(state = statefips,
                        county = .x,
                        year = 2021) %>%
      rename_with(tolower)
  )

# wtr %>% mapview(zcol = 'mtfcc')

wtr <- wtr %>%
  mutate(extracted.type =
           map_chr(str_split(fullname,
                             ' ')
                   ,~tail(.,1))
         ,.after = fullname
           )

# thinking thru trimming water in way that would work nationwide..

# tibble(wtr) %>% select(mtfcc, extracted.type) %>% distinct() %>% View()

# wtr %>%
#   tibble() %>%
#   group_by(mtfcc) %>%
#   summarise(n = n()
#             ,nacres = sum(awater)) %>%
#   arrange(desc(nacres))
# wtr %>%
#   tibble() %>%
#   group_by(extracted.type) %>%
#   summarise(n = n()
#             ,nacres = sum(awater)) %>%
#   arrange(desc(nacres))
# arrange(desc(nacres))
# # wtr  %>% mapview(zcol = 'extracted.type')

# will keep larger features but drop smaller ponds/lakes... could not worry
# about dropping any for Della but would just make  littile more computer work.
wtr <- wtr %>%
  filter(mtfcc %in%
           c('H2051', 'H2053', 'H3010', 'H2040') |
           !is.na(extracted.type) |
           awater >= 1e5
         )

#wtr %>% mapview(zcol = 'mtfcc')

wtr <- wtr %>%
  st_transform(5070)

tmp <- fcts %>%
  st_difference( st_union( wtr ) ) %>%
  st_set_precision(
    geo.precision) %>%
  st_make_valid()

tmp %>% mapview(zcol = 'geoid')

#devtools::install_github('kmcd39/geox')

# union by relevant columns -----------------------------------------------

pfed <- pfed %>%
  group_by(mang_type, des_tp) %>%
  summarise(., do_union = T)

# add index/id
pfed <- pfed %>%
  ungroup() %>%
  mutate(id = 1:nrow(.)
         ,.before = everything())


# get overlap by tract ----------------------------------------------------

#' long by manager and designation type

perc.by.tract <-
  geox::get.spatial.overlap(
    fcts,
    pfed,
    'geoid',
    'id',
    filter.threshold = 0.005 # half a percent overlap.
  )

# add full list of tracts and make 0s explicit
perc.by.tract <- perc.by.tract %>%
  full_join(tibble(fcts)['geoid']) %>%
  mutate(perc.area =
           if_else( is.na(perc.area),
                    0, perc.area)
         )

# add des/mgnr type back
perc.by.tract <- perc.by.tract %>%
  left_join(tibble(pfed)[c('id', 'mang_type', 'des_tp' )]
            ,by = 'id') %>%
  select(-id)



# map by tracts -----------------------------------------------------------

# perc.by.tract_1m.precision <- perc.by.tract %>%
#   group_by(geoid) %>%
#   summarise(perc.area = sum(perc.area)
#             ) %>%
#   left_join(fcts) %>%
#   st_sf()
# perc.by.tract_1m.precision %>%
#   mapview( zcol = 'perc.area')

# for eyeballing accuracy
# perc.by.tract_1m.precision +
#   mapview(pfed, zcol = 'des_tp')

perc.by.tract

# Tigris queries for della -----------------------------------------------------------------

# (run on della to prep shapes -- no internet access)

# BGS
statefps <- geox::rx$statefp %>% unique()
bgs <- map_dfr(statefps
        ,~tigris::block_groups(state = .x
                                ,year = 2021)
        )
st_write(bgs
         ,'/scratch/gpfs/km31/census-geos/bgs/block-groups.shp')


# WATER
cos <- tigris::counties(year = 2021) %>%
  rename_with(tolower)


statefips <-  cos$statefp %>% unique()

for(fips in statefips) {

  svdir <- paste0('/scratch/gpfs/km31/census-geos/water/state-', fips, '/')

  dir.create( svdir )

  wtr <- map_dfr(
    cos$countyfp,
    ~{tigris::area_water(
      state = fips
      ,county = .x
      ,year = 2021
    ) %>%
        rename_with(tolower)
    }
  )

  st_write(wtr
          ,paste0(svdir, 'water.shp')
           )
}

cos %>% nrow()
