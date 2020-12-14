# setup ws ----------------------------------------------------------------
# NOTE--- this is now archived--- active version moved to divFcns; used to
# generate largest-plc-in-cz, which will now live there

library(sf)
library(dplyr)
library(purrr)
rm(list = ls())

plcs <- st_read("~/R/shapefiles/places/places.shp") %>% st_transform(4326)
csds <- st_read("~/R/shapefiles/county-subdivisions/csds.shp") %>% st_transform(4326)

#cts <- divDat::ctpts %>% st_transform(4326)
czs <- divDat::geo.list$cz %>% st_transform(4326)

czs <- czs %>%
  select( cz = region.id
          ,cz_name = region.name
          ,state
          ,cz.pop = population
          ,geometry)


# grab helper fcns --------------------------------------------------------
# will probably put all these into divFcns pckg later
source("places & rays/places & rays fcns.R")


# clip places to shoreline ------------------------------------------------
# CZ shapefile already is clipped to shoreline, so i just union that and then clip
# weirdly, it looks like czs don't cover all of Hawaii, so this step should change if we're generating stats for non-cz larger regions
land.area <- st_union(czs)

csds <- st_intersection(csds, land.area)
plcs <- st_intersection(plcs, land.area)
# mapview(filter(plcs, STATEFP == 25))


# add population to spatial data ---------------------------------------------------------

# do PLACES first ---------------------------------------------------------
# get population from census tables
# uses 5-yr aggregations for 2018 population estimates
plc.pop <- data.table::fread("addl-census-data/ACSST5Y2018-population/places/ACSST5Y2018.S0101_data_with_overlays_2020-07-10T143423.csv")
plc.pop <- plc.pop %>% 
  tibble() %>%
  select(c(1,population =3))

plc.pop$GEO_ID <- 
  strsplit(plc.pop$GEO_ID, "US") %>%
  map_chr(~`[`(.,2))

head(plc.pop)
natl.plcs <-
  plcs %>%
  left_join(plc.pop,
            by = c("GEOID" = "GEO_ID")) 

# now do CSDs -------------------------------------------------------------

csd.pop <- data.table::fread("addl-census-data/ACSST5Y2018-population/countysubdivisions/nhgis0020_ds239_20185_2018_cty_sub.csv")
csd.pop <- 
  csd.pop %>%
  select(c(1,5:10,
           NAME = NAME_E,
           population = AJWME001)) %>%
  select(-NAME) %>% # (NAME colm is redudant after merge)
  tibble() %>%
  mutate( STATEA = stringr::str_pad(STATEA, 2, "left", "0")
          ,COUNTYA = stringr::str_pad(COUNTYA, 3, "left", "0")
          ,COUSUBA = stringr::str_pad(COUSUBA, 5, "left", "0"))

# join attr w spatial data
csds %>% colnames() #map(class)
csd.pop %>% colnames() # %>% map(class)
natl.csd <-
  csds %>%
  left_join(csd.pop,
            by = c( "STATEFP" = "STATEA"
                    ,"COUNTYFP" = "COUNTYA"
                    ,"COUSUBFP" = "COUSUBA")) 


# filter to largest centers in cz's --------------------------------------------------------------

largest.plc.in.cz <- largest.centers.in.region(natl.plcs, czs)
largest.csd.in.cz <- largest.centers.in.region(natl.csd, czs)

# rejoin to polygon info --------------------------------------------------
# ("largest.centers" fcn returns as point data)
largest.plc.in.cz <- return.to.polygon(largest.plc.in.cz, natl.plcs)
largest.csd.in.cz <- return.to.polygon(largest.csd.in.cz, natl.csd)

# make visuals to compare (leaflet fcns) -------------------------------------------------
divFcns::quick.leaflet
specialized.quick.leaflet <- function (st_df, color_by = "div_type", pal = RColorBrewer::brewer.pal(8, 
                                                                                                    "Dark2"), weight = .5) 
{
  require(leaflet)
  st_df <- st_transform(st_df, 4326)
  if (!is.null(color_by)) {
    var <- rlang::sym(color_by)
    val <- pull(st_df, !!var)
    pal <- colorFactor(pal, domain = val)
  }
  out <- leaflet(st_df) %>% addProviderTiles(provider = "CartoDB.DarkMatter") %>% 
    addPolygons(color = "#008080", fillColor = ~pal(val), 
                opacity = 0.7, weight = weight, label = lapply(paste0("cz name: ",val,
                                                                      "<br> name: ",st_df$NAME
                                                                      ,"<br> class: ",st_df$CLASSFP
                ), 
                shiny::HTML))
  return(out)
}

add.larger.areas <- function(map, larger.areas) {
  larger.areas <- st_transform(larger.areas, 4326)
  
  map %>%
    addPolylines(data = st_boundary(larger.areas)
                 , weight = 1,
                 color = "#BB2040"
                 ,fillColor = "#001080",
                 fillOpacity = .3)
}

# no surprise; instances where places are on cz borders get weird.
# Need to figure out how to fix. snap to grid and join by st_covers or within?
'
specialized.quick.leaflet(largest.plc.in.cz, "cz_name") %>%
  add.larger.areas(czs)

specialized.quick.leaflet(largest.csd.in.cz, "cz_name") %>%
  add.larger.areas(czs)
'

# also looking, particularly at "place" shapefile, strking cutouts and weird political divisions?
# I wonder what's going on in Ada, OK

# mapview visualisations --------------------------------------------------

library(mapview)
natl.plcs %>%
  filter(STATEFP == 25) %>%
  mapview(zcol = "CLASSFP")






# save --------------------------------------------------------------------

saveRDS(largest.plc.in.cz,
        "intermediate saves/largest areas within regions/largest-plc-in-cz.RDS")
saveRDS(largest.csd.in.cz,
        "intermediate saves/largest areas within regions/largest-csd-in-cz.RDS")


largest.plc.in.cz %>%
  divFcns::abv_out() %>%
  count(CLASSFP)

largest.csd.in.cz %>%
  divFcns::abv_out() %>%
  count(CLASSFP)



# for getting population from CTs -----------------------------------------

# (don't use; ez to just get from source)
'get.population.from.cts <- function(regions, cts) {
  regions %>%
    st_join(cts,
            .) %>%
    data.frame() %>%
    group_by(GEOID) %>%
    summarise(population = sum(population, na.rm = T)) %>%
    ungroup() %>%
    left_join(regions[,c("GEOID", "NAME", "STATEFP", "CLASSFP", "geometry")],
              by = c("GEOID")) %>% 
    st_sf()
}

plcs <- get.population.from.cts(plcs, cts)
'