library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

rm(list = ls())

#' notes from patrick:
#'
#' If you're able: here is the site where data on "protected areas" for the
#' whole country is available:
#' https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
#' And here is a link with replication files that were used to create a national
#' dataset just focusing on parks and recreation land: https://osf.io/pwdsg/
#'
#' I'd like to see if we can create a file down to the census tract that
#' includes measures of % acreage restricted, and then separate measures of
#' restrictions for specific purposes if possible. can you take a look and see
#' if you can begin to work with the geodatabase here?
#'


# load data ----------------------------------------------------------------

# downloaded from https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download

ddir <- '~/R/local-data/usgs/PADUS3_0Geodatabase/'
gdb <- ddir %>% list.files(pattern = 'gdb$', full.names = T)

lyrs <- gdb %>% st_layers()


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


# quickly peek through the layers -----------------------------------------

# note: don't use wkt filter when there's no geometry! it causes a crash.
lyrs

#' layers 1-8 and 15 have no geometries; layers 9-14 do have geos.

lyrs$name
lyrs[!is.na(lyrs$geomtype)]
st_read(gdb, layer = lyrs$name[1])
st_read(gdb, layer = lyrs$name[15])

# Layers 1-8 are parts of a data dictionary; layers 15 are sources data was
# compiled from.

# Seems layer 9,
# "PADUS3_0Combined_Proclamation_Marine_Fee_Designation_Easement," combines all
# the other layers.
lyrs$features[9]
lyrs$features[10:14] %>% sum()

# i can likely just use layer 9 and maybe reference 1-8 for metadata.


# load layer 9 through sample area ----------------------------------------

usgs <- st_read(
  gdb
  , layer = lyrs$name[9]
  #,crs = 4326
  , wkt_filter = wktf # over sample area
)
# i get a warning/GDAL error reading..

usgs <- usgs %>%
  rename(geometry = SHAPE) %>%
  rename_with(tolower)

usgs <- tibble(usgs)

# usgs %>% glimpse()
# usgs %>% colnames()
#
# usgs %>%
#   taux::count.across(
#     matches(
#       'featclass|category|own|type|name|tp'
#     )
#   )
#
# usgs %>%
#   taux::count.across(
#     matches('iucn') )
#
# usgs %>%
#   taux::count.across(
#     matches('gap') )
#

# thinking through columns more. ----------------------------------------------

#' I think all the columns that have associated metadata could be interesting
#' actually:
lyrs$name[c(1:8)]
usgs$pub_access # maps to layer1, PublicAccess
usgs$featclass # maps to layer 2, Category (Fee/easement/Other/Unknown...)
usgs$des_tp # maps to layer 3, designation type (Critical environmental concern/ conservation easment/ local historic or cultural area, etc.)
usgs$gap_sts # maps to layer 4, GAP status
usgs$iucn_cat # layer 5; IUCN category (maybe worse than GAP status?) IUCN -- international Union for Conservation of Nature
usgs$mang_name # maps to layer 6; Agency/manager name.
usgs$mang_type # maps to layer 7; Agency/manager type.

usgs %>%
  taux::count.across(
    matches('pub_access|featclass|des_tp|gap_sts|iucn.cat|mang.type')
  )

st_read(gdb, layer = lyrs$name[6]) %>%
  arrange(Code)

usgs %>% count(own_type) # ownership type.. not seeing data dictionary, but many are self evident. But seems a worse-documented version of Manager Type.

st_read(gdb, layer = lyrs$name[3]) %>% arrange(Code)

usgs %>% count(loc_own)


# trimming some columns ---------------------------------------------------

usgs
usgs %>% count(loc_mang)
usgs %>% glimpse()
usgs %>% count(gapcdsrc)
usgs %>% select(matches('^'))
usgs

usgs <- usgs %>%
  select(-matches('^loc|src$|^unit|wdpa|date|source|comments|^shape'))

# adding metadata columns -------------------------------------------------

#' map column name in the data to metadata name.
#'
#' can skip 8, which is just state names
metadata2colm <- tibble(
   metadata.nm = lyrs$name[1:7]
  ,colm.nm = Hmisc::Cs(pub_access, featclass, des_tp, gap_sts, iucn_cat, mang_name, mang_type)
)

metadata <- metadata2colm$metadata.nm %>%
  map2( metadata2colm$colm.nm
        , ~{st_read(gdb, layer = .x) %>%
            select(!!.y := 1, !!.x := 2)
          }
       ) %>%
  set_names(lyrs$name[1:7])

metadata

# remember how to do this...
tmp <- c(list(head(usgs, 100)), metadata) %>%
  purrr::reduce(left_join)

usgs2 <- c(list(usgs), metadata) %>%
  purrr::reduce(left_join)

# quick categorical correllogram.. ------------------------------------------------------

# ?model.matrix
# #install.packages('ggcorrplot')
# metadata2colm
# corr.vars <- metadata2colm %>%
#   filter(metadata.nm %in%
#            c('Category', 'GAP_Status'))
#
# library(ggcorrplot)
# model.matrix(~0+.,
#              data =
#                select(usgs2
#                       ,all_of(
#                         #lyrs$name[1:7]
#                         corr.vars$metadata.nm
#                         ))
#              ) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2
#              ,digits = 1, tl.cex = 9)


# get some useful descriptives of data possibilities.. --------------------

# distibution of the different categories:
metadata2colm
metadata$Agency_Type
count(usgs, own_type)

cats.of.interest <-
  #c(metadata2colm$metadata.nm[c(1:4,7)] )
  metadata2colm$metadata.nm[c(1:4,6:7)]

# Count number of instances of each column of interest and bind to single
# dataframe
tmp <- usgs2 %>%
  taux::count.across(all_of( cats.of.interest )) %>%
  imap( ~ mutate(.x, variable = .y, .before = everything()
                 )) %>%
  imap_dfr( ~select(.x, variable, value = 2, count = n)) %>%
  arrange(variable)

tmp

# select only top 8 by category, group others to "other"
tmp <- tmp %>%
  group_by(variable) %>%
  mutate(value =
           if_else( row_number() > 8
                    ,'recoded to other'
                    ,value
                    )) %>%
  group_by(variable, value) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  arrange(variable, desc(count))

duped.levls <- tmp %>%
  count(value) %>%
  arrange(desc(n)) %>%
  filter(n > 1)

tmp <- tmp %>%
  mutate(value = factor(value,
                        levels =
                          c( .$value[! .$value %in% duped.levls$value]
                            , duped.levls$value)
                        ))
metadata2colm

tmp %>%
  ggplot(
    aes( y = value
        ,x = count
        ,fill = variable)
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(visaux::jewel.pal(), 'grey35')
    ,guide = 'none'
  ) +
  scale_y_discrete(
    name = NULL
    ,limits = rev
  ) +
  scale_x_continuous(
    labels = scales::comma
  ) +
  facet_wrap(vars(variable)
             ,scales = 'free_y'
             ,ncol = 1) +
  theme_bw() +
  labs(
    title = 'Distibution of Different categories in the USGS Protected Areas Data, NJ Sample'
    ,subtitle =
      str_wrap(width = 80,
               'Different protected areas have different biodiversity mandates; different public access statuses; or different owners or managing entities (levels of government, federal agencies, NGO conservation groups, etc.). Some may also have mining or military uses or similiar')
  ) +
  theme(text = element_text(size = 10)
        ,axis.text.y = element_text(angle = 15)
        )

# visaux::ragg.wrapper(
#   'protected-areas/nj-sample-categories in data counts'
#   ,res = 300
#   ,height = 10
#   ,width = 9.1
# )


# JUST designation type
tmp <- usgs2 %>%
  group_by(Designation_Type) %>%
  summarise( n.areas = n()
            ,n.acres = sum(gis_acres)) %>%
  arrange(desc(n.acres)) %>%
  mutate(Designation_Type =
           factor(Designation_Type
                  ,levels = .$Designation_Type)) %>%
  pivot_longer(matches('^n'))

tmp %>%
  filter(Designation_Type !=
            'Outer Continental Shelf Area') %>%
  filter(name == 'n.acres') %>%
  ggplot(
    aes( y = Designation_Type
         ,x = value / 1e3
         ,fill = name)
  ) +
  geom_col(
    position = 'dodge'
  ) +
  scale_fill_manual(
    values = c(visaux::jewel.pal()[4], 'grey35')
    ,guide = 'none'
  ) +
  scale_y_discrete(
    name = NULL
    ,limits = rev
  ) +
  scale_x_continuous(
    name = 'acres (thousands)'
    ,labels = scales::comma
  ) +
  # facet_wrap(vars(name)
  #            ,scales = 'free_x'
  #            ,ncol = 1) +
  theme_bw() +
  labs(
    title = 'Land area distibution of designation typesin the USGS Protected Areas Data, NJ Sample'
    ,subtitle =
      str_wrap(width = 100,
               'Looks in particular at the "Designation Type" column, and counts acreage by designation. The Designation Type seems to offer a lot of detail that may allow us to drill down to pretty specific types of conservation areas. ')
  ) +
  theme(text = element_text(size = 10))



# mapping -----------------------------------------------------------------

library(mapview)

usgs$Designation_Type

# create a subset to map
# when agnecy type is
# tmp <- usgs2 %>%
#   filter(
#     grepl('Local|Historic|Boundary|Agricultural Easement|Resource Management|Private|Unkown'
#           ,Designation_Type)
#     )

# install.packages('gdalUtilities')
# library(gdalUtilities)

# get NJ counties
njcos <- tigris::counties(year = 2021
                          ,state ='NJ') %>%
  rename_with(tolower)

#njcos %>% mapview()
selcos <- njcos %>%
  filter(countyfp %in%
           c( '017','013'
              #, '003'
             #, '039'
             ))

trimbx <- selcos %>%
  st_transform(5070) %>%
  st_bbox()

# cast MULTISURFACE to MULTIPOLYGON
usgs2 <- usgs2 %>% st_sf()
usgs2 <- st_cast(usgs2, "MULTIPOLYGON")

tmp <- usgs2 %>%
  st_sf() %>%
  st_transform(5070) %>%
  st_crop(trimbx)

library(lwgeom)
tmp$geometry %>% st_geometry_type() %>% as_tibble() %>% count(value)
tmp$geometry <- tmp$geometry %>% st_simplify()

nrow(tmp)

tmp %>%
  mapview(zcol = 'Designation_Type')
# get historic places separate..
tmp.hdsts <-
  tmp %>%
  filter(grepl('Historic', Designation_Type
               ,ignore.case = T))

tmp %>%
  mapview(zcol = 'Designation_Type') +
  mapview(st_boundary(tmp.hdsts)
          ,color = visaux::jewel.pal()[5])


visaux::jewel.pal() %>% scales::show_col()
tmp




tmp
filter(Designation_Type %in%
           c()
           )

local.desgs
metadata2colm
usgs %>% count(Agency_Type)


usgs %>%
  filter()
  head(100) %>%
  st_sf() %>%
  mapview(zcol = 'Designation_Type')

lyrs
# even just for the one state, it takes a While to map.

# usgs %>%
#   st_sf() %>%
#   mapview(zcol = 'des_tp')




usgs %>%
  head(100) %>%
  purrr::reduce(med)

map(lyrs$name[1:7]
    ,~st_read(gdb, layer = .x))
  st_read(gdb, layer = lyrs$name[1])
st_read(gdb, layer = lyrs$name[2])
st_read(gdb, layer = lyrs$name[3])
st_read(gdb, layer = lyrs$name[4])
st_read(gdb, layer = lyrs$name[5])
st_read(gdb, layer = lyrs$name[6])



# peeking through metadata files ------------------------------------------

lyrs
lyrs$name
st_read(gdb, layer = lyrs$name[1])
st_read(gdb, layer = lyrs$name[2])
st_read(gdb, layer = lyrs$name[3])
st_read(gdb, layer = lyrs$name[4])
st_read(gdb, layer = lyrs$name[5])
st_read(gdb, layer = lyrs$name[6])
lyrs$name[15]
# st_read(gdb, layer = lyrs$name[15]) %>% View()

# desingation type
lyrs$name[3]
st_read(gdb, layer = lyrs$name[3]) %>%
  arrange(Code)

usgs$des_tp



# looking at vector analysis and summary statistics file ------------------

#' (from https://www.sciencebase.gov/catalog/item/6196b9ffd34eb622f691aca7 )
#'
#' linked from bottom of same download page..
