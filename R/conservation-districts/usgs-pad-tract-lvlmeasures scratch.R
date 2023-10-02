library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

rm(list = ls())

# load data ----------------------------------------------------------------

# downloaded from https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
ddir <- '~/R/local-data/usgs/PADUS3_0Geodatabase/'
gdb <- ddir %>% list.files(pattern = 'gdb$', full.names = T)

lyrs <- gdb %>% st_layers()
lyrs

# define a bbx/wkt for reading subarea ------------------------------------

statesf <- tigris::states(year = 2021)
statesf <- statesf %>%
  rename_with(tolower)

# see https://github.com/pauldzy/USGS_Albers_Equal_Area_Projections for EPSG.
bbx <- statesf %>%
  filter(stusps %in%
           c('NJ'#, "PA"
           )) %>%
  st_transform(5070) %>%
  st_bbox()

st_crs(5070)

wktf <- bbx %>% st_as_sfc() %>% st_as_text()
wktf


# load & preprocess data -------------------------------------------------------

#' remember, layer 9 was combined all data; layers 1-8 was metadata.
lyrs$name

# load spatial data
pad <- st_read(
  gdb
  , layer = lyrs$name[9]
  , wkt_filter = wktf # over sample area
)
# i get a warning/GDAL error reading..

# cast to polygons (get rid of multisurface), then turn to tibble for
# non-spatial processing
pad <- pad %>%
  rename(geometry = SHAPE) %>%
  rename_with(tolower) %>%
  st_cast("MULTIPOLYGON") %>%
  tibble()

pad


## add plain-language columns from metadata --------------------------------------

#' remembering what the columns say:
lyrs$name[c(1:8)]
pad$pub_access # maps to layer1, PublicAccess
pad$featclass # maps to layer 2, Category (Fee/easement/Other/Unknown...)
pad$des_tp # maps to layer 3, designation type -- many categories, potentially very useful.
pad$gap_sts # maps to layer 4, GAP status -- (is there biodiversity mandate?)
pad$iucn_cat # layer 5; IUCN category (maybe worse than GAP status?) IUCN -- international Union for Conservation of Nature
pad$mang_name # (along with own_name), maps to layer 6; Agency name.
pad$own_name
pad$mang_type # # (along with own_type), maps to layer 7; Agency type.
pad$own_name


# map column name in the data to metadata name; can skip 8, which is just state
# names
metadata2colm <- tibble(
  metadata.nm = lyrs$name[1:7]
  ,colm.nm = Hmisc::Cs(pub_access, featclass, des_tp, gap_sts, iucn_cat, mang_name, mang_type)
)

# create an organized list of dataframes for every metadata layer
metadata <- metadata2colm$metadata.nm %>%
  map2( metadata2colm$colm.nm
        , ~{st_read(gdb, layer = .x) %>%
            select(!!.y := 1, !!.x := 2)
        }
  ) %>%
  set_names(lyrs$name[1:7])

metadata


# consider what i want to add to data -------------------------------------

#' this site has good breakdown on what the Categories mean (fee, easement,
#' designation...), as well as how to interpret owner/manager/agency columns...
#'
#' https://www.protectedlands.net/what-to-know-before-using-pad-us-version-2-0/
#'
#' & from Psharkey:
#'
#' "for designation type, agency type and category seem most important. for
#' agency type I'm most interested in local, NGO and private for designation
#' type I'm most interested in the conservation types. that said, I wouldn't
#' really want to exclude any of these in developing a core set of measures."
#'


#' more details from the PAD how-tos...
#'
#' Q: How can I map all the managers of land?
#'
#' A: Manager-based mapping is highly recommended in PAD-US, as the manager
#' field (Mang_Name) is more completely filled in than the owner field (work is
#' underway on the owner field). The best approach here may be to use one of the
#' pre-made PAD-US layer files: “Mid Agency Level” and “Fine Agency Level” layer
#' files have individual agency names for Federal managers and generic names for
#' other managers; “General Agency Level” has generic level names for all
#' managers (Federal, State, etc.). You can of course choose “Mang_Name” as the
#' field to categorize in any GIS software and define your own colors.

#


#' so we can play with this later, but for now, I'll just try and do a Total,
#' and by:
#'
#' Category, manager/owner type, and designation type. I'll save merging in the
#' metadata for later.

metadata2colm

metadata$Category


# also, featclass seems a neater version of Category, and values already
# plain-language. So can drop category from both data and metadata


## more and more colm peeks ------------------------------------------------

# locown
pad %>% count(eholdtyp)
pad %>% count(loc_ds) # local designation type -- not standardized
#pad %>% count(source_paid)
pad %>% glimpse()

pad %>% count(own_type, own_name, mang_type, mang_name) %>% arrange(desc(n))

metadata$Agency_Type
metadata$Agency_Name

## finally just trim columns -----------------------------------------------

pad <- pad %>%
  select(featclass
         ,own_type, own_name
         ,mang_type, mang_name
         ,des_tp
         ,gap_sts, pub_access
         ,gis_acres
         ,geometry
         )



# work out code for just smaller area ---------------------------------------

# 1-2 counties
stfp <-
cofps <- c( '017','013'
            #, '003'#, '039'
            )

fco <- tigris::counties(year = 2021
                       ,state ='NJ' #stfp is 34
                       ) %>%
  rename_with(tolower) %>%
  filter( countyfp %in%
            cofps )

trimbx <- fco %>%
  st_transform(5070) %>%
  st_bbox()

# it's equal area projectection so can just keep 5070
st_crs(5070)

tmp <- pad %>%
  st_sf() %>%
  st_crop(trimbx)

# get CTs for that areas
fcts <- tigris::tracts(year = 2021
                       ,state = 'NJ'
                       ,county = cofps) %>%
  rename_with(tolower)


fcts <- fcts %>%
  select(1:4, aland, awater, geometry) %>%
  st_transform(5070)

fcts


# get areal overlap -------------------------------------------------------

#' ..it's easy to do without the breakdowns..
#'
#' I think to do it with the breakdowns, i need to basically do a group_by %>%
#' union for each column. And for no breakdown (all protected area), we just
#' union, not by group.



# simplify just for scratching out code/ running locally
library(lwgeom)
tmp <- tmp %>% st_simplify()

# for no bkdwn:
tmp2 <- tmp %>%
  st_sf() %>%
  group_by(NULL) %>%
  #group_by(mang_type) %>%
  summarise(., do_union = T) %>%
  mutate(id = 1:nrow(.))

out <-
  geox::get.spatial.overlap(fcts,
                            tmp2,
                             'geoid',
                            'id'
                             )
# duplicated tracts?
out$geoid %>% duplicated() %>% sum()

# # visual check
scales::show_col(visaux::jewel.pal())
# library(mapview)
# out %>% full_join(fcts) %>%
#   mutate(perc.area = if_else(is.na(perc.area), 0, perc.area)) %>%
#   st_sf() %>% mapview(zcol = 'perc.area') +
#   (mapview(tmp2, col.regions = visaux::jewel.pal()[1]))

# write function to do this for all the types of protected areas.

#' protected.area.by.type.by.tract
#'
#' @param pad data for protected areas
#' @param cts data for census tracts. Will assume geoid column
#' @param category.colm string for the category by which to get protected area
#'   purpose
#'
protected.area.by.type.by.tract <-
  function(pad,
           cts,
           category.colm = NULL) {

    #browser()

    pad <- pad %>% st_sf() %>% st_transform(5070)
    cts <- cts %>% st_sf() %>% st_transform(5070)

    # group pad and union by the category
    gpad <- pad %>%
      group_by( !!rlang::sym(category.colm) ) %>%
      summarise(., do_union = T) %>%
      mutate(id = 1:nrow(.))

    if(is.null(category.colm)) {
      category.colm <- 'id'
    }

    out <-
      geox::get.spatial.overlap(cts,
                                gpad,
                                'geoid',
                                category.colm,
                                filter.threshold = 0.005 # half a percent overlap.
      )

    # add back to full list of CTs and make 0s explicit
    out <- cts %>%
      tibble() %>%
      select(geoid) %>%
      left_join(out) %>%
      mutate(perc.area =
               if_else( is.na(perc.area )
                        ,0 , perc.area ))

    return(out)
}


check <- protected.area.by.type.by.tract(tmp, fcts,
                                'des_tp')

# add geos
check <- check %>% left_join( fcts['geoid'] )

# we will expect duplicated tracts iff different designation types fall on the
# same area.. looks like the spatial.overlap fcn handles this well, grouping by
# both identifiers to calc spatial overlap.
check %>%
  filter(geoid %in%
           geoid[duplicated(geoid)]) %>%
  arrange(geoid)

# mapview( st_sf(check)[check$geoid == '34013006400',] ) +
#   mapview(tmp[tmp$des_tp %in% c('LOTH', 'LP'),], zcol = 'des_tp')


## iterate through the columns we're interested in. ----------------------

# category columns that we'll want to have the tract-level measures by:
cat.colms <- Hmisc::Cs(featclass
                       ,own_type, own_name
                       ,mang_type, mang_name
                       ,des_tp
                       ,gap_sts)

combined <- cat.colms %>%
  map(
    ~protected.area.by.type.by.tract(
      tmp, fcts, .x
    )
  ) %>%
  set_names(cat.colms)

# then bind to long dataframe.
combined <- combined %>%
  imap_dfr(
    ~mutate(.x,
          protected.area.descriptor =  .y
          ,.after = geoid) %>%
      rename( protected.area.value = !!.y
              ,tract.perc.area = perc.area)
)
combined

# great! just gotta set this up as a Della script.
