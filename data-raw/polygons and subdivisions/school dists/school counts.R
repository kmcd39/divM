library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(lwgeom)
library(divFcns)
rm(list=ls())

czs <- divDat::czs

sds <- divDat::school.dists
sds <- sds %>%
  filter( !grepl("Not Defined", NAME, ignore.case = T) )


czs
# Note-- looks like some school districts ~can~ cross county lines (not co-terminous)

# use 1 state to work out code --------------------------------------------
ma = tigris::states() %>% filter(STUSPS == "MA")
tmp = sds %>% filter(STATEFP %in% ma$STATEFP)
plot(tmp['GEOID'])
#cob <- divDat::counties %>% filter(statefp == '25' ) %>% st_boundary() %>% st_union()
#mapview(tmp, zcol = "GEOID") + mapview(cob, color = "#008080")

#   -----------------------------------------------------------------------

sds.polys <- xwalks::get.spatial.overlap( sds, czs,
                                          "GEOID", "cz" )

sds.polys <- rename(sds.polys, sd = 1)


# checking results --------------------------------------------------------
sds.polys$perc.area %>% summary()
sds.polys[ duplicated(sds.polys$sd), ]

# remove <x% of SD-in-CZ. I.e., if it's split but >(1-x)% falls in one CZ, count
# that SD only w/in that CZ.
sds.polysC = sds.polys %>%
  filter(perc.area > .05) 

sum(!sds$GEOID %in% sds.polysC$sd) # lost 0 sds

# count school district (segments) by CZ
sds.polys <- sds.polysC %>%
  tibble() %>% count(cz) %>%
  rename(school.dist_polys = n)
sds.polys

# write output
write.csv(sds.polys, ".polys output/schooldist_polys.csv")


# to map ------------------------------------------------------------------

tmpczs <- czs %>% 
  left_join(xwalks::co2cz) %>% 
  filter(state == "MA") %>% 
  select(-countyfp) %>%
  distinct()

to.map <- xwalks::get.spatial.overlap( sds[sds$STATEFP==25, ] 
                                       , tmpczs
                                       , "GEOID", "cz"
                                       , filter.threshold = .05) %>%
  divFcns::conic.transform()

to.map <- rename(to.map, sd = 1)

# That spatial overlap fcn can really mess with the geometries...
# it might make sense to develop xwalks fcn to filter out points and linestrings w/in fcn..
tmp = to.map %>%
  st_collection_extract(c("POLYGON"))
tmp %>%
  rmapshaper::ms_explode() %>%
  group_by(sd, cz, perc.area) %>%
  summarise(., do_union = T)


mapview(tmp, zcol="cz", color = "white", lwd=1.5) +
  mapview(st_cast(tmpczs, "MULTILINESTRING"), color = "black", lwd=1.8)

tmp %>%
  mapview(zcol="perc.area", color = "white", lwd=1.1) +
  mapview(st_cast(tmpczs, "MULTILINESTRING"), color = "black", lwd=1.8)

library(ggplot2)
ggplot() +
  geom_sf(data= sds.polys, aes(fill=cz),color="white") +
  theme_void()

co2cz %>% 
  rename(cz = cz_1990) %>%
  left_join(counties
            ,by = c("fips_county" = "geoid")) %>%
  filter(cz_name == "Houston") %>%
  st_sf() %>%
  mapview(zcol = "fips_county")