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


# CT analysis -------------------------------------------------------------

## load generated measures -------------------------------------------------
# FIPS for selected states
#selfps <- c('09', '25', '44')
selfps <- '09'
read.dir <- '~/R/local-data/usgs/generated-PAD-measurues/'
readpth <- paste0(read.dir, 'statefp-', selfps, '.csv')
dir.exists(read.dir)
padm <- vroom::vroom(readpth)


## also load PAD data ------------------------------------------------------

ddir <- '~/R/local-data/usgs/PADUS3_0Geodatabase/'
gdb <- ddir %>% list.files(pattern = 'gdb$', full.names = T)

lyrs <- gdb %>% st_layers()
lyrs


fcts <- tigris::tracts( year = 2021
                       ,state = '09'
                       ) %>%
  rename_with(tolower) %>%
  st_transform(5070)

# see https://github.com/pauldzy/USGS_Albers_Equal_Area_Projections for EPSG.

wktf <- fcts %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

# read
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

# intial exploration ------------------------------------------------------

padm %>% filter(is.na(protected.area.value))


padm %>% count(protected.area.descriptor)
padm %>% count(protected.area.descriptor, protected.area.value) %>% arrange(desc(n))

padm

pad$own_type
pad$own_name

# brainstorming -----------------------------------------------------------

#' i can make bivariate chloropleths with SES indicators and protected area by
#' selected type... and scatterplots of the same
#'
#' I think I need to start with some mapping
#'
#' I can calculate total acreage of protected area by type for some initial
#' analysis and see what categories are well represented...
#'
#' Remember that the "how-tos" recommends mapping by Manager type rather than
#' owned field (see
#' https://www.protectedlands.net/pad-us-technical-how-tos/#tips-for-gis-users-5)
#'


# setup for some visuals --------------------------------------------------

# epsg for good CT projection
ctcrs <- 2234
ctcrs %>% st_crs()

# get geos and tract attrs for CT --------------------------------------------------

# get state
ctstate <- tigris::states(year = 2021) %>%
  rename_with(  tolower  ) %>%
  filter(stusps == 'CT') %>%
  st_transform(ctcrs)

# ct counties
ctcos <- tigris::counties(state = 'CT'
                          ,year = 2021
                          ) %>%
  rename_with(  tolower  ) %>%
  st_transform(ctcrs)

ctcos

cattrs <- censusrx::get.tract.attrs(
                      state = '09'
                     ,cofps = ctcos$countyfp
                     ,year = 2021
                     ,geo = 'tract'
                   )

cattrs

ctsf <- tigris::tracts(state = 'CT'
                       ,year = 2021) %>%
  rename_with(tolower) %>%
  st_transform(ctcrs) %>%
  filter(aland > 0)

# med hh inc and pop dens
# cattrs %>%
#   left_join(ctsf)%>%
#   st_sf() %>%
#   ggplot() +
#   geom_sf(aes(fill = med.hhinc)
#           ,color = NA) +
#   scale_fill_viridis_b()

cattrs$pop_dens %>% head()

cattrs %>%
  left_join(ctsf)%>%
  st_sf() %>%
  mutate(pop_dens.capped =
           visaux::cap.at.quantile(pop_dens)
         ) %>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(pop_dens.capped))
          ,color = NA) +
  scale_fill_viridis_b(
     name = 'Pop dens (persons/acre)'
    ,labels = visaux::ggcapped.labels
  )


## create a subset of raw PAD data for a smaller area ----------------------

# ctcos %>% mapview(zcol = 'geoid')

selcos <- ctcos %>%
  filter(geoid == '09003') %>%
  st_transform(5070)

# cropped pad
cpad <- pad %>%
  st_sf() %>%
  st_crop(selcos) %>%
  tibble()

# exploratory visuals of PAD measures & data -----------------------------------------

padm %>% count(protected.area.descriptor)

#' join to aland to calculate n protected acres. remember units of ALAND are sq
#' meters
#'
#' acres column will be protected acreage by type
padm <- padm %>%
  left_join(
    tibble(ctsf)[,c('geoid', 'aland')]
    ) %>%
  mutate(acres =
           tract.perc.area *
           units::set_units(
             units::set_units(aland, 'meters^2')
             ,'acres'           )
  )

padm$acres <- as.numeric(padm$acres)
padm


## analysis by land managers -----------------------------------------------

padm

mgmt <- padm %>%
  filter(grepl('^mang',
               protected.area.descriptor)) %>%
  group_by(protected.area.descriptor,
           protected.area.value) %>%
  summarise( count = n()
            ,acres = sum(acres)) %>%
  arrange(protected.area.descriptor, desc(acres)) %>%
  ungroup()

mgmt

# add long name
metamanagers <-
  metadata[grepl('^Agency', names(metadata))] %>%
  imap( ~rename(.x, protected.area.value = 1,
                label = 2 )) %>%
  imap_dfr( ~mutate(.x,
                    protected.area.descriptor =
                      paste0('mang_',
                             str_extract( tolower(.y)
                                         ,'name$|type$'
                                         ))
                    ,.before = everything()
                    )
            )

mgmt <- mgmt %>%
  left_join(metamanagers) %>%
  filter( protected.area.descriptor ==
            'mang_type') %>%
  group_by(protected.area.descriptor) %>%
  mutate( label =
            if_else( row_number() >= 7
                              ,'Unknown or other (recode)'
                              ,label
                     ))

mgmt
# aggregate up to recode
mgmt <- mgmt %>%
  group_by(label) %>%
  summarise(across(c(count, acres)
                   ,~sum(.x, na.rm = T) # NAs are 0s there
                   )) %>%
  arrange(desc(acres)) %>%
  mutate(label = factor(label
                        ,levels = .$label))


mgmt
padm
metamanagers

# look at all managers by type and name
all.mgmt <- padm %>%
  filter( grepl('^mang',
                protected.area.descriptor
                ) ) %>%
  group_by(protected.area.descriptor,
           protected.area.value) %>%
  summarise( count = n()
             ,acres = sum(acres)) %>%
  arrange(protected.area.descriptor, desc(acres)) %>%
  ungroup() %>%
  left_join(metamanagers)

# all.mgmt %>% View()

# ---> confirms mgnr TYPE, note NAME is appropriate for our analysis.

### map by those categories -------------------------------------------------

library(mapview)

# tract-level by mgmt type
ct.mgmt <- padm %>%
  filter( protected.area.descriptor ==
            'mang_type') %>%
  left_join(metamanagers)
# %>%
#   mutate(label =
#            if_else(label %in% mgmt$label
#                    , label
#                    , 'Unknown or other (recode)'))
ct.mgmt

# (i haven't re-aggregated to recode yet.. because there are likely overlaps? let's
# do some exploratory mapping first.)

ct.mgmt %>% count(protected.area.value)

scales::show_col(  visaux::jewel.pal() )

# facet plot by mgmr type
ct.mgmt %>%
  full_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  ggplot() +
  geom_sf(data = ctstate
          ,fill = 'grey70') +
  geom_sf( aes(fill = tract.perc.area)
           ,color = NA
           ,linewidth = 0
           ) +
    scale_fill_viridis_c() +
  facet_wrap(vars(label)) +
  theme_void() +
  theme(strip.background =
          element_rect(fill = visaux::jewel.pal()[4] )
        ,strip.text = element_text(
          color = 'white', face = 'bold'
        ))

# pad %>%
#   filter(mang_type == 'FED') %>%
#   st_sf() %>%
#   mapview::mapview()


#' what comprise the designations by mgmt type? -- for the non-Federal common
#' mgmt types.
#'
#' Federal managed areas dwarf those w all other managers, and are entirely
#' "outer continenal shelf areas" (water) or "Approved or Proclamation Boundary"
#'
#' Note metadata on those proclamation boundaries (from
#' https://www.protectedlands.net/pad-us-technical-how-tos/#feature-classes-in-pad-us-2)
#' --
#'
#' Proclamations – Boundaries of the administrative area of National Forests,
#' Parks, Wildlife Refuges and other lands. These are not ownership lines but
#' are used for agency administration purposes (note that some commercial
#' mapping providers and others incorrectly use these boundaries to show
#' protected areas and in doing so often show large areas of private lands as
#' part of public lands). Some proclamation boundaries can cover extremely large
#' areas of private land, while others are close to ownership boundaries. In any
#' case, it’s important not to consider proclamation boundaries as reflecting
#' actual land ownership – they’re just for agency administration purposes
mgmt$label[2:6]

tmp <-
  pad %>%
  filter(mang_type %in%
           filter(metamanagers #
                  ,protected.area.descriptor == 'mang_type' &
                    label %in% mgmt$label[2:6])$protected.area.value
                    #label %in% mgmt$label[1:6])$protected.area.value

           ) %>%
  group_by(mang_type,
           des_tp) %>%
  summarise( count = n()
            ,acres = sum(gis_acres)) %>%
  arrange(mang_type, desc(acres)) %>%
  left_join(
    rename(metadata$Designation_Type
           ,label = 2)
            ) %>%
  ungroup()

ordr <- tmp %>%
  group_by(des_tp, label) %>%
  summarise(acres = sum(acres)) %>%
  arrange(desc(acres))

tmp$label <- factor(tmp$label
                    ,levels = ordr$label)

tmp %>%
  left_join(metadata$Agency_Type
            ,by = c('mang_type' = 'agency_type')
            ) %>%
  ggplot(
    aes(y = label
        ,x = acres
        ,fill = Agency_Type)
  ) +
  geom_col() +
  scale_y_discrete(
    name = NULL
    ,limits = rev
  ) +
  scale_fill_manual(
    name = 'Managing entity'
    ,values = visaux::jewel.pal()
  ) +
  theme_minimal() +
  visaux::upper.legend.box() +
  labs(
    title = 'Acreage of Protected Lands by Purpose and Managing Entity in CT'
    ,subtitle = str_wrap(' ')

  )

#' "Proclamations" with federal managers dwarf all other types of protected
#' lands, but are omitted from the plot -- because proclamations have very
#' different interpretaions.



# a plurality map
#devtools::install_github('spatial-ineq/divseg')


# analysis by designation ------------------------------------------------

# total acreage by designation type. ctdes for designation type in CT
ctdes <- padm %>%
  filter(protected.area.descriptor ==
           'des_tp') %>%
  group_by(protected.area.value) %>%
  summarise( count = n()
            ,acres = sum(acres)) %>%
  arrange(desc(acres)) %>%
  left_join(
    rename(metadata$Designation_Type,
           protected.area.value = 1, label = 2)
  )

ctdes

#'  i think a relevant set of filters will be:
#'
#'  all conservation easements, and all NGO/PVT/LOCAL gov't-managed.
#'
#'  Can recode to "parks", "conservation", and "other," with things like state
#'  resource mgmt, watershed protection areas omitted?
#'
#'  I think that kind of filter -- with Proclamations and outer continental
#'  shelf areas similarly removed -- would be good ...

#devtools::load_all()

pad %>% count(featclass)
#
# # i could regenerate for just designation type for a smarter subset..?
# dpad <- pad %>%
#   filter(! featclass %in% c( 'Proclamation'
#                             ,'Marine')) %>%
#   protected.area.by.type.by.tract(
#     cts = ctsf
#     ,category.colm = 'des_tp'
#   )


#' recode.designations
#'
#' Function to recode labels for des_tp to some combined categories. Assumes a
#' label column from metadata.
#'
#'
recode.designations <- function(x) {

  x %>%
    mutate(recode =
             case_when(
               grepl('Park|Recreation', label) ~ 'Park or recreation'
               ,grepl(
                 'Watershed Protection|Resource Management|Forest Stewardship|Wildlife Refuge|National Forest'
                 , label) ~
                 str_wrap(width = 45,'Watershed protection, resource management, or forest stewardship')
               ,grepl('Conservation', label) ~ 'Conservation'
               ,grepl('Military|Research', label) ~ 'Military or research area'
               ,grepl('Native American', label) ~ 'Native American land area'
               ,grepl('Recreation|Cultural|Historic', label) ~ 'Other recreational/historic/cultural'
               ,grepl('Agricultural|Ranch', label) ~ 'Agricultural or ranch'
               ,grepl('Marine|Continental Shelf', label) ~ 'Marine or continental shelf area'
               ,TRUE ~ 'Unkown or Other'
             ) )

}

padm %>%
  filter(protected.area.descriptor ==
           'des_tp') %>%
  left_join(
    rename(metadata$Designation_Type,
           protected.area.value = 1, label = 2)
  ) %>%
  recode.designations()

pad$gis_acres %>% quantile(seq(0, 1, .1))


tmp <-
  pad %>%
    filter(! featclass %in% c( 'Proclamation'
                              ,'Marine')) %>%
  left_join(
    rename(metadata$Designation_Type,
           des_tp = 1, label = 2)
  ) %>%
  recode.designations()

pad %>%
  group_by(featclass) %>%
  summarise(count = n(),
            acres = sum(gis_acres)) %>%
  arrange(desc(acres))
pad %>%
  group_by(featclass) %>%
  taux::quantiles.across.groups('gis_acres')

pad$gis_acres %>% quantile(seq(0,1,.1))

tmp <- tmp %>%
  st_sf() %>%
  st_simplify()

tmp %>%
  filter(gis_acres <= 12) %>%
  head(100) %>%
  mapview(zcol = 'featclass')

# tmp %>% mapview(zcol = 'recode')  # these take so long to render...
tmp %>%
  select(des_tp, label, des_recode) %>% distinct() %>% arrange(des_recode)#  %>% View

tmp %>%

group_by(recode) %>%
  summarise(aland = sum(aland, na.rm = T)) %>%
  arrange(desc(aland))



# exploratory maps of subarea ---------------------------------------------


#' it makes sense to really just retain Easements and Fees.
#'
#' Designations tend to overlap with the other cat
tmp <- cpad %>%
  filter(! featclass %in% c( 'Proclamation'
                             ,'Designation')) %>%
  filter( des_tp != 'OCS' # (outer continental shelf)
          ) %>%
  left_join(
    rename(metadata$Designation_Type,
           des_tp = 1, label = 2)
  ) %>%
  recode.designations()

tmp$gis_acres %>% quantile(seq(0,1,.1))

#tmp %>% st_sf() %>% mapview(zcol = 'featclass')


# making sure i understood Marine correctly (it also contains some swamps etc.)
pad %>%
  filter( featclass %in% c( 'Marine'
                            )) %>%
  left_join(
    rename(metadata$Designation_Type,
           des_tp = 1, label = 2)
  ) %>%
  recode.designations() %>%
  st_sf() %>%
  mapview(zcol = 'mang_name')



# coverage by feature class -----------------------------------------------


padm %>%
  filter(protected.area.descriptor == 'featclass') %>%
  left_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  filter(!is.na(protected.area.value)) %>%
  ggplot() +
  geom_sf(data = ctstate
          ,fill = 'grey90') +
  geom_sf( aes(fill = tract.perc.area)
           ,color = NA
           ,linewidth = 0
  ) +
  scale_fill_viridis_c() +
  facet_wrap(vars(protected.area.value)) +
  theme_void() +
  theme(strip.background =
          element_rect(fill = visaux::jewel.pal()[4] )
        ,strip.text = element_text(
          color = 'white', face = 'bold'
        ))



# ct analysis with revised filters ------------------------------------------

trimmed.pad <- pad %>%
  filter(! featclass %in% c( 'Proclamation'
                             ,'Designation') &
           des_tp != 'OCS' # (outer continental shelf)
         )

# check overlaps from within this subset...
#overlaps <- st_intersection(trimmed.pad$geometry)

devtools::load_all()

padm <- trimmed.pad %>%
  protected.area.by.type.by.tract(
        cts = ctsf
        ,category.colm = 'des_tp'
      )


# with labels and recodes
padmr <- padm %>%
  left_join(
    rename(metadata$Designation_Type,
           des_tp = 1, label = 2)
  ) %>%
  recode.designations()

padmr %>% select(des_tp, label, recode) %>% distinct() %>% arrange(recode)

# add acres
padmr <- padmr %>%
  left_join(
    tibble(ctsf)[,c('geoid', 'aland')]
  ) %>%
  mutate(acres =
           perc.area *
           units::set_units(
             units::set_units(aland, 'meters^2')
             ,'acres'           )
  )

# colum plot by label (not recode)
padmr %>%
  mutate(acres = as.numeric(acres)
  ) %>%
  group_by(label,recode) %>%
  summarise(across(c(perc.area, acres)
                   ,sum
  )) %>%
  ungroup() %>%
  arrange(desc(acres)) %>%
  mutate(label = factor(label,
                         levels = unique(.$label))
  ) %>%
  ggplot(
    aes(y = label
        ,x = acres
        ,fill = recode
    )
  ) +
  geom_col(
    #fill = visaux::jewel.pal()[4]
  ) +
  scale_y_discrete(
    name = NULL
    ,limits = rev
  ) +
  scale_x_continuous(
    labels = scales::comma
  ) +
  scale_fill_manual(
    values = c(visaux::jewel.pal(), 'black', 'grey70', 'blue')
  ) +
theme_minimal() +
  visaux::upper.legend.box() +
  labs(
    #title = ''
    subtitle = str_wrap('Acreage of Protected Lands after suggested filters applied, all of CT')

  )

# to look at State Rsrc Mgmt Areas
#pad %>% filter(des_tp == 'SRMA') %>% View()



## column plot by designation ----------------------------------------------


# # aggregate to recode
# padmra <- padmr %>%
#   group_by(geoid, recode) %>%
#   summarise(across(c(perc.area, acres)
#                    ,sum
#   ))

padmr$perc.area %>% quantile(seq(0,1,.05))


padmra %>%
  mutate(acres = as.numeric(acres)
         ) %>%
  group_by(recode) %>%
  summarise(across(c(perc.area, acres)
                   ,sum
  )) %>%
  arrange(desc(acres)) %>%
  mutate(recode = factor(recode,
                         levels = .$recode)
         ) %>%
  ggplot(
    aes(y = recode
        ,x = acres
        #,fill = Agency_Type
        )
  ) +
  geom_col(
    fill = visaux::jewel.pal()[4]
  ) +
  scale_y_discrete(
    name = NULL
    ,limits = rev
  ) +
  scale_x_continuous(
    labels = scales::comma
  )
  theme_minimal() +
  visaux::upper.legend.box() +
  labs(
    title = 'Acreage of Protected Lands after suggested filters applied'
    ,subtitle = str_wrap(' ')

  )

# facet map of designation  ----------------------------------------------

  padmra %>%
  full_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  mutate(capped_perc.area =
           visaux::cap.at.quantile(perc.area
                                   ,.9)
         ) %>%
  ggplot() +
  geom_sf(data = ctstate
          ,fill = 'grey90') +
  geom_sf( aes(fill = capped_perc.area)
           ,color = NA
           ,linewidth = 0
  ) +
  scale_fill_viridis_c(
     '% of tract'
    ,labels = visaux::ggcapped.labels
  ) +
  facet_wrap(vars(recode)) +
  theme_void() +
  theme(strip.background =
          element_rect(fill = visaux::jewel.pal()[4] )
        ,strip.text = element_text(
          color = 'white', face = 'bold'
        )) +
  visaux::upper.legend.box()

# selected designations within that filters.. just drop agriculatural/ranch,
# native american lands, and military/research lands
#padmr %>% select(des_tp, label, recode) %>% distinct() %>% arrange(recode) %>% View()


## map of protected areas w suggested filters  --------------------------------

padsel <- padmr %>%
  filter( !grepl('Agricultural|Military|Native American Land Area',
                 label) )

padsel <- padsel %>%
  left_join(
    tibble(ctsf)[,c('geoid', 'aland')]
  ) %>%
  mutate(acres =
           perc.area *
           units::set_units(
             units::set_units(aland, 'meters^2')
             ,'acres'           )
  ) %>%
  group_by(geoid) %>%
  summarise(across(c(perc.area, acres)
                   ,sum
  ))


padsel %>% select(perc.area, acres) %>% map( ~quantile(.x, seq(0,1,.1)))



padsel %>%
  full_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  mutate(capped_perc.area =
           visaux::cap.at.quantile(perc.area
                                   ,.9)
  ) %>%
  ggplot() +
  geom_sf(data = ctstate
          ,fill = 'white') +
  geom_sf( aes(fill = capped_perc.area)
           ,color = NA
           ,linewidth = 0
  ) +
  scale_fill_viridis_c(
    '% of tract'
    ,labels = visaux::ggcapped.labels
  ) +
  theme_void() +
  theme(strip.background =
          element_rect(fill = visaux::jewel.pal()[4] )
        ,strip.text = element_text(
          color = 'white', face = 'bold'
        )) +
  visaux::upper.legend.box()+
  labs(
    title = 'Protected area coverage for selected types'
    ,subtitle =
      str_wrap(width = 80,
'Filters are: Not Proclamation or Designation feature class (only Fees, Easements, and Marine); not continental shelf; not Indian land; not agricultural or military.')
  )


# bivariate plots ------------------------------------------------------

#install.packages("biscale", dependencies = TRUE)
library(biscale)
library(cowplot)

##  w pop dens and protected land -----------------------------------------

tmp <- padsel %>%
  full_join(cattrs)

tmp %>%
  select(geoid, pop, pop_dens,
         aland.acre, perc.area, med.hhinc) %>%
  filter(is.na(perc.area) |
           is.na(med.hhinc))

tmp$pop_dens %>% as.numeric()

btmp <-
  tmp %>%
  mutate(pop_dens = as.numeric(pop_dens)
         ) %>%
  filter(!is.nan(pop_dens)
         ) %>%
  bi_class( x =
              pop_dens
           , y = perc.area
           , style = "quantile",
           dim = 3)


# bi_pal('DkCyan')
bi.map <-
  btmp %>%
  full_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  ggplot() +
  geom_sf(aes(fill = bi_class),
          color = NA,
          size = 0.0,
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan",
                dim = 3) +
  bi_theme()

#?bi_pal()
bi.lgnd <-
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "More population ",
            ylab = "More protected area",
            size = 8)

cowplot::ggdraw() +
  draw_plot(bi.map, 0, 0, 1, 1) +
  draw_plot(bi.lgnd, .7, .1, 0.25, 0.25)

##  w hh income and protected land -----------------------------------------




tmp <- padsel %>%
  full_join(cattrs)

tmp %>%
  select(geoid, pop, aland.acre, perc.area, med.hhinc) %>%
  filter(is.na(perc.area) |
           is.na(med.hhinc))

cattrs %>%
  select(geoid, pop, n.hh, aland.acre, med.hhinc) %>%
  filter(is.na(med.hhinc))

cattrs$n.hh
btmp <-
  bi_class(tmp
           , x =
             med.hhinc
           , y = perc.area
           , style = "quantile", dim = 3)


bi_pal('PurpleGrn')
bi.map <-
  btmp %>%
  full_join(ctsf[c('geoid', 'geometry')]) %>%
  st_sf() %>%
  ggplot() +
  geom_sf(aes(fill = bi_class),
          color = NA,
          size = 0.0,
          show.legend = FALSE) +
  bi_scale_fill(pal = "PurpleGrn",
                dim = 3) +
  bi_theme()

bi.lgnd <-
  bi_legend(pal = "PurpleGrn",
            dim = 3,
            xlab = "Higher income ",
            ylab = "More protected area",
            size = 8)

cowplot::ggdraw() +
  draw_plot(bi.map, 0, 0, 1, 1) +
  draw_plot(bi.lgnd, .7, .1, 0.25, 0.25)
#



# remaining questions -----------------------------------------------------

#' does Marine overlap systematically with Fee or Designation??
#'
#' Based on Sofia's analysis it does..
#'
#'


## looking at Fee/easements overlaps & scratch code ------------------------

ddir <- '~/R/local-data/usgs/PADUS3_0Geodatabase/'
gdb <- ddir %>% list.files(pattern = 'gdb$', full.names = T)

lyrs <- gdb %>% st_layers()
lyrs$name
# Fees are layer 11 and Easements are 13.

fcts <- tigris::tracts( year = 2021
                        ,state = 'RI' # RI
) %>%
  rename_with(tolower) %>%
  st_transform(5070) %>%
  filter(aland > 0)

# see https://github.com/pauldzy/USGS_Albers_Equal_Area_Projections for EPSG.
wktf <- fcts %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

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

# pfe %>% map(st_crs)
# st_crs(5070)
# CRS is right


pfe <- pfe %>%
  map( ~st_transform(.x, 5070) ) %>%
  map( ~st_cast(.x, 'MULTIPOLYGON') ) %>%
  map( st_make_valid ) %>%
  map( st_simplify )

pfe$PADUS3_0Fee %>% glimpse()

ints <-
  #st_crosses()
  st_intersects(
    pfe$PADUS3_0Fee,
    pfe$PADUS3_0Easement )

library(mapview)

# scales::show_col(visaux::jewel.pal())

#pfe$PADUS3_0Fee %>% colnames()
pfe <- pfe %>%
  map( ~select(.x,
               matches('^Own|Loc_Mang|FeatClass|Nm$|^Mang|^Des|^Unit|GAP|IUCN|Pub_Access')
               )
  )

# map of all Easements and Fees that intersect for CT
easements <- st_boundary(pfe$PADUS3_0Easement)#[1:100,]
intersecting.fees  <- pfe$PADUS3_0Fee[lengths(ints) > 0, ]
all.fees <- pfe$PADUS3_0Fee

mapview( all.fees#intersecting.fees
           ,col.regions = visaux::jewel.pal()[4]
  ) +
    mapview( easements
             ,color = visaux::jewel.pal()[5]
             ,size = 2)

metadata$Designation_Type %>% View()

# how many Unknowns or similar for the intersecting fees vs the easements?
ints[lengths(ints) > 0]
ints

# can i just union away overlaps from within fees?
unioned.fees <- all.fees %>%
  group_by(Mang_Type, Des_Tp) %>%
  summarise(., do_union = T)

unioned.fees %>%
  ungroup() %>%
  mapview(zcol = 'Des_Tp')

# overlaps remain. but arbitrarily taking just one would seem fine.
tmp <- unioned.fees %>%
  ungroup() %>%
  st_difference()
pfe


#'  t_difference documentation:
#'
#'  When st_difference is called with a single argument, overlapping areas are
#'  erased from geometries that are indexed at greater numbers in the argument
#'  to x; geometries that are empty or contained fully inside geometries with
#'  higher priority are removed entirely. The st_difference.sfc method with a
#'  single argument returns an object with an "idx" attribute with the orginal
#'  index for returned geometries.
