

# dd ---------------------------------------------------------------------------

#' working out funcions to generate between-tract measure: is/is not across division.
#'
#' Workflow:
#'
#' - start with btwn tract matrix (can initialize with sfg or use
#' tidyr::expand_grid
#'
#' - get polygons and associate poly id with each tract
#'
#' - compare: in.same.poly?
#


library(tidyverse)

# build tract matrix -----------------------------------------------------------

ctx <- xwalks::ctx %>%
  filter(grepl("Minneap", cz_name) )

cts <- ctx %>%
  select(geoid)

ctsq <-
  expand_grid(cta = cts$geoid,
              ctb = cts$geoid)

# note how quickly grid expands--- from 761 CTs in minneapolis to 580,000
# combinations



# maybe igraph object is better initialized with SFG flows. I think we have to
# determine if we want the relationship for ALL tracts or only tracts connected with flows.
library(igraph)
library(tidygraph)



# spatializing MN tracts -------------------------------------------------------

cfps <- ctx %>%
  pull(countyfp) %>% unique()

ctsf <-
  map2_dfr(substr(cfps, 1, 2),
           substr(cfps, 3, 5),
           ~tigris::tracts(.x,.y)
  )

ctsf


# get div data -----------------------------------------------------------------

library(sf)
hwys <- st_read("~/R/shapefiles/National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp")

hwys <- st_transform(hwys, st_crs(st_sf(ctsf)))

hwys <- st_intersection(hwys,
                        st_union(ctsf))

hwys <- hwys %>%
  divM::denode.lines()

ints <- hwys %>%
  filter(SIGNT1 %in% "I")

ints <- ints %>%
  divM::Fix.all.hwys()

plot(ints["SIGN1"])

# gen poly ---------------------------------------------------------------------

# get the single polygon for the study area (region minneapolis = rmn)
rmn <-
  ctsf %>% st_union() %>%
  st_sf(region.name =
          unique(ctx$cz_name),
        region.id =
          unique(ctx$cz),
        region.type =
          "cz",
        geometry = .)

ctsf <- ctsf %>% divM::conic.transform() %>% rename_with(tolower)
rmn <- rmn %>% divM::conic.transform()
ints <- ints %>% divM::conic.transform()

ppolys <-
  divM::polygonal.div(
    rmn,
    ints,
    # min.size = 5e+06,
    #min.population.count = ,
    #min.population.perc = 0.005, # a half percent of cz pop
    return.sf = T)

ppolys["id"] %>% plot()

ppolys <- ppolys %>%
  rename(poly.id = id)

tmp <-
  st_intersection(ctsf["geoid"],
                  ppolys["poly.id"])


# how do we handle border cases? CT split by the division -- do we keep both rows
# "across border from itself", or go back to 1row/CT and associate the CT with the
# polygon that contains the greatest share?


# to do largest share:
tmp <-
  xwalks::get.spatial.overlap(
    ctsf,
    ppolys,
    "geoid",
    "poly.id",
    filter.threshold = 0.01
  )

tmp %>% arrange(geoid)

tmp <- tmp %>%
  group_by(geoid) %>%
  filter(perc.area == max(perc.area))


# get across-interstate status -------------------------------------------------

dpoly <- tmp %>%
  tibble() %>%
  select(1,2)

#xd: cross-div
xda <-
  cts %>%
  left_join(dpoly) %>%
  rename(
    cta = 1,
    polya = poly.id)
xdb <-
  cts %>%
  left_join(dpoly) %>%
  rename(ctb = 1,
         polyb = poly.id)

xd <- expand_grid(
  xda,
  xdb)

xd$cross.div <-
  with(xd, polya != polyb)
