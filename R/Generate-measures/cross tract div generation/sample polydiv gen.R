
# notes ------------------------------------------------------------------------

#' This demonstrates process of allocating neighborhoods (tracts or block groups...)
#' to divisions within a city/region/cz/...
#'
#' There is a markdown version of this in the same folder.

# ---------------------------------------------------------------------------
rm(list = ls())
require(tidyverse)
require(sf)

# option setting
sf_use_s2(T)
options(tigris_use_cache = TRUE)

# dropbox dir or della base dir
ddir <- # Sys.getenv('drop_dir')
  '/scratch/gpfs/km31/'

devtools::load_all()

# possible sample areas -------------------------------------------------------------

arrs <- geox::rpops %>%
  filter(pop > 50e3
         ,rt == 'cz') %>%
  geox::add.rns()

arrs %>%
  filter(pop > 500e3) %>%
  arrange(pop) %>%
  head(100) %>%
  pull(rn)

arrs %>%
  filter(grepl('Portland|Seattle', rn))

czsf <- geox::build.CZs('20100') # Portland, ME
#czsf <- build.CZs('39400') # Seattle

plcsf <- geox::places.wrapper(x = czsf)

library(mapview)
mapview(plcsf) + mapview(czsf)

# do network analysis at Place level -------------------------------------------------

# for analysis, i want probably cz/cbsa (or maybe sometimes Place)
# for visuals, i want region network cropped to bbox
# for sample, i'll just use Place

smplc <- plcsf %>%
  filter(grepl('^Portland|^Seattle', name)) %>%
  st_transform(4326)

smplc %>% mapview()

# get block groups
bgs <- geox::tracts.from.sf(x = st_bbox(smplc)
                            , query.fcn = tigris::block_groups
                            , year = 2019)
bgs[3] %>% plot()

# remove water areas (where tract geoid begins 99, per census bureau naming)
bgs <- bgs %>%
  filter(substr(tractce
                ,1,2) != '99')

# get block group/place xwalk
sbgs <- xwalks::get.spatial.overlap(bgs, smplc
                                    ,'geoid', 'placefp')
sbgs


# get stamen background for viz
sttm <- visaux::get.stamen.bkg(sfx = smplc
                               , zoom = 11)


# visualize blocks in sample area
ggmap(sttm) +
  geom_sf(data = st_boundary(smplc)
          ,color = 'red',linetype = '21'
          ,size = 1.3
          ,inherit.aes = F) +
  geom_sf(data = sbgs
          ,aes(fill = factor(geoid))
          ,alpha = .6
          ,color = 'white'
          ,inherit.aes = F) +
  scale_fill_discrete(guide = 'none') +
  theme_void()


sbgs %>% mapview(zcol = 'perc.area') +
  mapview(st_boundary(smplc)
          ,color = 'red')

# get the overlapping county
cos <- sbgs$geoid %>% substr(1,5) %>% unique()

# to get water
#wtr <- visaux::water.wrapper(countyfps = '23005')
#wtr <- wtr %>% st_transform(4326)
#wtr %>% mapview()

# to remove 1-bg islands
sbgs <- sbgs[lengths(st_intersects(sbgs)) > 1, ]
sbgs %>% mapview()

sbgs <- sbgs %>% st_transform(4326)
smplc <- smplc %>% st_transform(4326)

# get boundary area
bounds <- sbgs %>%
  st_union() %>%
  st_sf()

# get a division ---------------------------------------------------------------

# hwys
nhpn <- geox::get.NHPN(sfx = bounds)

nhpn <- nhpn %>% filter(!st_is_empty(geometry)) # (added this to helper)

nhpn %>% count(signt1)

# comparing NHPN hwy data to the base tile:
ggmap(sttm) +
  geom_sf(data = st_boundary(bounds)
          ,color = 'red',linetype = '21'
          ,size = 1.3
          ,inherit.aes = F) +
  geom_sf(data = nhpn
          ,aes(color = signt1)
          ,size = 1.5
          ,inherit.aes = F) +
  visaux::bbox2ggcrop(bounds)

# get subpolys divs ------------------------------------------------------------

devtools::load_all()

# defined in 'polys fcns.R'
fdivs <- polygonal.div(bounds, nhpn
                       ,return.sf = T)


# visualizing highwy division
ggmap(sttm) +
  geom_sf(data = st_boundary(bounds)
          ,alpha =.8
          ,fill = 'purple'
          ,inherit.aes = F) +
  geom_sf(
    data = fdivs
    ,aes(fill = poly.id)
    ,color = 'white'
    ,alpha = .7
    ,inherit.aes = F
  ) +
  scale_fill_discrete(guide = 'none') +
  visaux::bbox2ggcrop(bounds) +
  theme_void()


# allocate tracts / bgs to divisions -------------------------------------------

sbgs
fdivs <- st_transform(fdivs, 4326) %>% rename(poly.id = id)

xnhood.div <- xwalks::get.spatial.overlap(sbgs
                            ,fdivs
                            ,'geoid'
                            ,'poly.id')

xnhood.div <- xnhood.div %>%
  group_by(geoid) %>%
  filter(perc.area ==
           max(perc.area))

xnhood.div <- xnhood.div %>% tibble()

dbgs <- bgs %>%
  select(geoid, aland,awater) %>%
  filter(geoid %in% sbgs$geoid) %>%
  tibble() %>%
  left_join(xnhood.div[c('geoid', 'poly.id')]
            ,by = 'geoid')

dbgsf <- dbgs %>% st_sf()



# visualize each n'hood based on highway divsisions
ggmap(sttm) +
  geom_sf(data = st_boundary(bounds)
          ,alpha =.8
          ,fill = 'purple'
          ,inherit.aes = F) +
  geom_sf(
    data = dbgsf
    ,aes(fill = factor(poly.id))
    ,color = 'white'
    ,alpha = .7
    ,inherit.aes = F
  ) +
  geom_sf(data = nhpn
          ,aes(color = factor(signt1))
          ,size =  1.1
          ,inherit.aes = F
  ) +
  scale_fill_discrete(guide = 'none') +
  visaux::bbox2ggcrop(bounds) +
  theme_void()

# functionalize whole process  ---------------------------------------------------------------

#' workflow:
#'
#' 1) decide on region(s)
#'
#' 2) query nb'hoods
#'
#' 3) query divisions
#'
#' 4) apply polygonal divs
#'
#' 5) join/allocate nb'hoods to division
#'

# decided region:
smplc

# query nbhds:
nbhds <- geox::tracts.from.sf(x = st_bbox(smplc))
nbhds %>% plot()

# remove only-water nbhoods
nbhds <- nbhds %>%
  filter(substr(tractce
                ,1,2) != '99')

nbhds <- xwalks::get.spatial.overlap(nbhds, smplc
                                      ,'geoid', 'placefp'
                                     , filter.threshold = 0.01)

# could be other trims/transformations, like erasing water areas
nbhds

# query divs:
divs <- geox::get.NHPN(sfx = bounds)
divs <- divs %>% filter(!st_is_empty(geometry))

# div subpolys
subpolys <- ws_polygonal.div(smplc
                             ,divs
                             ,return.sf = T)
subpolys['poly.id'] %>% plot()

# merge and allocate nbhoods:
xnhood.div <- xwalks::get.spatial.overlap(nbhds
                                          ,fdivs
                                          ,'geoid'
                                          ,'poly.id')

xnhood.div <- xnhood.div %>%
  group_by(geoid) %>%
  filter(perc.area ==
           max(perc.area)) %>%
  tibble()

div.nbhd <- nbhds %>%
  tibble() %>%
  left_join(xnhood.div[c('geoid', 'poly.id')]
            ,by = 'geoid')

div.nbhd
div.nbhdsf <- div.nbhd %>% st_sf()

div.nbhdsf %>% plot()
smplc

# or, just use function (defined in tract-lvl-divM-fcns)
plcdiv <- gen.cross.tract.dividedness(smplc
                                      ,nhpn
                                      ,nbd.query.fcn = tigris::tracts
                                      ,region.id.colm = 'placefp'
                                      ,year = 2019)


plcdiv %>% geox::attach.geos() %>% mapview(zcol = 'poly.id') +
  mapview(nhpn)

czsf

# ez variation to move from Place to CZ:
cz.hwys <- czsf %>% st_transform(4326) %>% geox::get.NHPN() %>% filter(!st_is_empty(geometry))

devtools::load_all()
czdiv <-
  gen.cross.tract.dividedness(czsf
                              ,cz.hwys
                              ,nbd.query.fcn = tigris::tracts
                              ,negative.buffer = 500
                              ,region.id.colm = 'cz'
                              ,year = 2019)

czdiv %>% geox::attach.geos() %>% mapview(zcol = 'poly.id') +
  mapview(cz.hwys)

czsf %>% mapview




# scratch ----------------------------------------------------------------------


# maine CZ with just interstates (do i need hwy fixing?)
czsf

hwys %>% head(1)
czsf %>% head()
hwys <- hwys %>% st_transform(st_crs(czsf))
cz.hwys <- st_crop(hwys, czsf)
ints <- hwys %>% filter(signt1 == 'I')
mapview(czsf) + mapview(ints)


devtools::load_all()
tmpsub <- polygonal.div( czsf
                        , ints
                        ,return.sf = T)

tmpsub %>% mapview()
# (small gaps in hwy shpfile interupts the logic of finding polygon divisions. This
# funcion can fill gaps with a flexible maximum distance threshold)
fhwys <- divM::Fix.all.hwys(hwys)

# peak at inputs; signt1 is highway type column (see NHPN metadata)
ggplot() + geom_sf(data = st_boundary(czsf)) + geom_sf(data = hwys, aes(color = signt1))





# addl sample ------------------------------------------------------------------

.countyfps <- geox::x2cos(cz='03101', cbsa = NULL)
rsf <- geox::build.CZs('03101'
                       #, crs = 4326
                       ) %>%
  mutate(rid = cz, rt = 'cz')
rsf

rds <- map_dfr(.countyfps
               ,~ tigris::roads(state = substr(.x,1,2)
                                ,county = substr(.x,3,5)
                                ,year = 2019)
               ) %>%
  rename_with(tolower) %>%
  st_transform(st_crs(rsf)) %>%
  st_crop(rsf)

interstates <- rds %>%
  filter(rttyp %in% 'I')

arterials <- rds %>%
  filter(mtfcc %in%
           Hmisc::Cs(S1100, S1200, S1630))


nbrsf <- rsf %>% st_buffer(-100)
polys <-
  st_split(nbrsf
            , interstates)

polys

# explode from GEOCOLLECTION/MULTIPOLYGON
polys <- polys$geometry %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON")
polys <- st_make_valid(polys) %>% st_sf()
polys <- handle.overlaps(polys)



polygonal.div( rsf
               ,interstates
               ,return.sf = T)

#tigris.call <- tigris::tracts
tigris.call <- tigris::block_groups

devtools::document()
devtools::load_all()
?divM::gen.cross.tract.dividedness




int.div <-
  divM::gen.cross.tract.dividedness(
    region = rsf
    ,divs = interstates
    ,nbd.query.fcn = tigris.call
    ,year = 2019
    ,region.id.colm = 'rid'
    ,erase.water = F
    ,negative.buffer = 200
  ) %>%
  select(geoid, int.poly = poly.id)

ar.div <-
  divM::gen.cross.tract.dividedness(
    region = rsf
    ,divs = arterials
    ,nbd.query.fcn = tigris.call
    ,year = 2019
    ,region.id.colm = 'rid'
    ,erase.water = F
  ) %>%
  select(geoid, arterial.poly = poly.id)


## hard scratch
polys <-
  st_split(st_buffer(region
                     , -100)
           , divs)$geometry

# explode from GEOCOLLECTION/MULTIPOLYGON
polys <- polys %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON")
polys <- st_make_valid(polys) %>% st_sf()
polys <- handle.overlaps(polys)
