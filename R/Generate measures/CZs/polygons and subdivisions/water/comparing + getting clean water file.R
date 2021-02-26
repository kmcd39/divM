library(sf)
library(dplyr)
library(dblinkr)
rm(list=ls())
# set test areas
czs <- divDat::geo.list$cz %>% select(region.id, region.name, region.type, geometry) %>% st_transform(4326)
phl <- czs[czs$region.name=="Philadelphia",]
mn <- czs[czs$region.name=="Minneapolis",]
# get water by county, upload raw into db ---------------------------------------------------------
con <- princeton.db.connect() # use this one for schemas to work
library(dbplyr)
tmp = tbl(con, in_schema("regions", 
                         "counties"))
county_list <- tmp %>% select(geoid) %>% collect() %>% pull(geoid)
county_list

# takes 5-character geoid
get.water.in.county <- function(county_id, ...) {
  cat("getting water for county", county_id, "\n")
  state = substr(county_id,1,2)
  county = substr(county_id,3,5)
  water <- tigris::area_water(state = state
                              ,county = county
                              ,refresh=TRUE, ...)
  if("sf" %in% class(water)) {
    water <- water %>% select(HYDROID, FULLNAME, geometry) %>%
      mutate(statefp = state, countyfp = county)
    st_write(water, con, Id(schema="divs", table="water_tigris")
             , append = TRUE)
    return(T)
  } else
    cat("no water found\n")
  
}
library(purrr)
# iterate through counties and add to db
'map(county_list
    ,purrr::possibly( ~get.water.in.county(.) 
                      ,otherwise = F))'


# check counties that were missed?
library(dbplyr)
inc <- tbl(con, in_schema("divs", "water_tigris"))
inc <- inc %>%
  select(statefp, countyfp) %>%
  count(statefp, countyfp) %>%
  collect()

inc$concatfp = paste0(inc$statefp, inc$countyfp)
inc$statefp %>% unique() # states incl.
# still missing (or there was no water in tigris)
inc %>% filter(statefp == "42" & countyfp == "091") # montco, pa?
county_list[! county_list %in% inc$concatfp] %>% sort() # all missing
# query.division(con, phl, "divs.water_tigris") %>% mapview::mapview()
rmw <- county_list[! county_list %in% inc$concatfp] %>% sort()

# do 2018 for counties that aren't working for 2019....
map(rmw
    ,purrr::possibly( ~get.water.in.county(., year = 2018) 
                      ,otherwise = F))

# missing some elements?
tmp <- tigris::area_water(state = "10", county = 3, year = 2017, refresh = T)
tmp %>% mapview()
# are there duplicatiosn?
duw <- tbl(con, in_schema("divs", "water_tigris"))
duw <- duw %>%
  select(statefp, countyfp, HYDROID) %>%
  collect()
#duplicated(duw$HYDROID) %>% sum()
dupeids <- duw$HYDROID[duplicated(duw$HYDROID)]
dupeids

duw <- tbl(con, in_schema("divs", "water_tigris"))
duwl <- duw %>% select(-geometry) %>% collect()
tmp <- duwl %>%
  filter(HYDROID %in% as.numeric(dupeids)) %>%
  count(HYDROID) %>% arrange(desc(n))



# check w/ test area ------------------------------------------------------
#tmp %>% filter(name == "Cumberland" & statefp == "34")
#cuw <- st_read(con, query = "select * from divs.water_tigris where statefp = '34' and countyfp='011'")

library(mapview)
tmp <- query.division(con , phl , "divs.water_tigris")
st_crs(tmp) <- 4326
mapview(tmp) + mapview(st_boundary(phl), color = "#000000")
# YAY
# check counties in ara
tarea.counties <- query.division(con, phl, "regions.counties")
st_crs(tarea.counties) <- 4326
tarea.counties %>% mapview(zcol="name", alpha.regions = .1) +
  mapview(tmp)
inc[inc$concatfp=="10003",]

# function to clean to named and touching-named ---------------------------------------
all_water <- st_read()





# alt shpfile..? ----------------------------------------------------------

wtmp <- st_read("~/R/shapefiles/water/Lakes_and_Rivers_Shapefile_NA_Lakes_and_Rivers_data_hydrography_l_rivers_v2/hydrography_l_rivers_v2.shp")
phl
mn <- czs[czs$name=="Minneapolis",] 
wtmp <- wtmp %>% st_transform(4326)
tmp <- st_intersection(phl, wtmp)
tmp <- st_intersection(mn, wtmp)
# wow that's great
tmp %>% mapview(zcol = "NAMEEN") + mapview(st_boundary(mn))
# checking old intermediate save ------------------------------------------
# it misses random counties (cry emoji)
'
aw <- readRDS("~/R/all sharkey geoseg work/dblinkr/.intermediate-saves/awater_better i think.RDS")


phw <- czs[czs$name=="Philadelphia",] %>% st_intersection(tw)
phw %>%
  mutate(is_named = !is.na(FULLNAME)) %>%
  mapview(zcol = "is_named")

'


