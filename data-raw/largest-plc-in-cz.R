library(sf)
library(tidyverse)
rm(list = ls())

# for iterating census api pulls

# looks like sopme colonies are available; I get what I can
state_list  <- xwalks::state2div %>%
  filter(as.numeric(statefp) < 60 |
           statefp %in% "72")
(state_list <- state_list$statefp)

plcs <-
  map_dfr(state_list
          , ~ tigris::places(state = .,
                             class = "sf",
                             year = 2019))

head(plcs)


# get czs  ----------------------------------------------------------------

# form czs from counties
counties <- tigris::counties( year = 2019)

counties <- counties %>%
  left_join(xwalks::co2cz,
            by = c("GEOID" = "countyfp",
                   "STATEFP" = "statefp"))

czs <- counties %>%
  divM::conic.transform() %>%
  group_by(cz, cz_name) %>%
  summarise(., do_union = T)

czs <- czs %>% filter(!is.na(cz))

# transform ---------------------------------------------------------------

plcs <- divM::conic.transform(plcs)

# get plc2cz xwalk
plc2cz <- xwalks::get.spatial.overlap(select(plcs
                                            ,plcid=GEOID
                                            ,plc_name = NAME)
                                     , czs
                                     , "plcid"
                                     , "cz" )


# assign plc to czs by majority area
plc2cz.trimmed <-
  tibble(plc2cz) %>%
  group_by(plcid) %>%
  filter(perc.area ==
           max(perc.area,
               na.rm = T))


# add population to spatial data ---------------------------------------------------------

# get population from census tables
# uses 5-yr aggregations for 2018 population estimates
plc.pop <-
  tidycensus::get_acs(geography = "place",
                      year = 2019,
                      variables = c("B01001_001")) # Total population

plc.pop

plc.pop <- plc.pop %>%
  select(c(plcid = 1,
           plc.name = 2,
           plc.pop = estimate))

# join population to spatial data
head(plc.pop)
plc2cz.pop <- left_join( plc2cz.trimmed,
                         plc.pop,
                         by = c("plcid"))

# filter to largest centers in cz's --------------------------------------------------------------
largest.plc.in.cz <-
   tibble(plc2cz.pop) %>%
   mutate(plc.pop =
            as.integer(plc.pop)) %>%
   group_by(cz) %>%
   filter(plc.pop ==
            max(plc.pop,
                na.rm = T)) %>%
  ungroup()

largest.plc.in.cz
# duplcate czs?
largest.plc.in.cz %>% map_int( ~sum(duplicated(.)))
largest.plc.in.cz %>%
  filter(cz %in%
           largest.plc.in.cz$cz[duplicated(cz)])

# reorg colms
(largest.plc.in.cz <- largest.plc.in.cz %>%
  select(plc.id = 1, cz.id = 2, plc.name, plc.pop))

czs %>% arrange(cz)
nrow(czs)

# add raw plc geometries --------------------------------------------------
# (not spatial overlap geos)
largest.plc.in.cz <-
  left_join(
    largest.plc.in.cz,
    plcs[, c("GEOID", "geometry")],
    by = c("plc.id" = "GEOID")
    )

library(lwgeom)
largest.plc.in.cz %>% st_sf() %>% st_is_valid() %>% sum()


# save in 4326 epsg? ------------------------------------------------------

largest.plc.in.cz <-
  largest.plc.in.cz %>% st_sf() %>% st_transform(4326)

# write data to pkg -----------------------------------------------------------------
usethis::use_data(largest.plc.in.cz, overwrite = TRUE)
