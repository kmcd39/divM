# setup ws ----------------------------------------------------------------
rm(list=ls())
source("polygons and subdivisions/polys fcns.R")

library(sf)
library(lwgeom)
library(dplyr)
library(purrr)
library(mapview)

# get subset of CZs that have some intersection w/ water lines ------------
wtr.eligible <- st_filter(czs, gsw)$region.id


# helper fcns -------------------------------------------------------------

#' cts.to.cz
#' 
#' Given a cz id, get all tracts in that cz.
#' Assumes existence of cts sf and ct/cz xwalk called ctxcz in ws
cts.to.cz <- function(cz.id) {
  ctids <- ctxcz[ctxcz$cz %in% cz.id,]$tract
  cts[cts$gisjoin %in% ctids, ]
}


# specialized polys fcn ----------------------------------
pop.filter.water.polys <- function(region, neg.buffer = 500
                                   , pop.perc.min = .05
                                   , return.map = F) {
  require(lwgeom)
  region.id = unique(region$region.id)
  cat("\ngetting water polys for", region.id, "\n")
  
  rwtr <- st_intersection(region, gsw)
  split <- st_split(st_buffer(region, -neg.buffer)
                    , rwtr) %>%
    rmapshaper::ms_explode() %>%
    mutate(id = 1:nrow(.))
  
  split <- st_make_valid(split)
  
  # also filter by pop.minimum
  if(!is.null(pop.perc.min)) {
    cz.tracts = cts %>% filter(cz == region.id) %>% st_make_valid()
    cz.pop = sum(cz.tracts$population)
    
    return.class = case_when(return.map ~ "sf",
                             !return.map ~ "tibble")
    
    polys = areal::aw_interpolate( split, tid = id
                                   ,cz.tracts, sid = gisjoin
                                   ,weight = "total" # tracts are co-terminous w/ czs
                                   ,extensive = c("population")
                                   ,output = return.class )  
    
    
    polys = polys %>%
      mutate(pop.perc = population / cz.pop) %>%
      filter(pop.perc >= pop.perc.min) %>%
      mutate(id = 1:nrow(.))
  }
  
  if(return.map) 
    return(polys)
  else
    return(
      data.frame(  region.id = region$region.id
                   ,region.name = region$region.name
                   ,region.type = region$region.type
                   ,n.water.polys = max(polys$id)  )
    )
}

# map thru regions --------------------------------------------------------

usgs.water.polys <- wtr.eligible %>%
  map_dfr( ~pop.filter.water.polys(region = czs[czs$region.id == ., ]
                                 ,neg.buffer = 500
                                 ,return.map = F))

write.csv(usgs.water.polys, 
          file = "usgs water polys.csv")


# spot checks -------------------------------------------------------------
pop.filter.water.polys(region = czs[czs$region.id == 34802,])

tmpcz <- czs[grepl("Minneapolis" # "Corinth"
                   , czs$region.name), ]
tmpcz
pop.filter.water.polys(tmpcz, return.map = T) %>%
  mapview(zcol= "id") + mapview(st_boundary(tmpcz), color = "red")








# area.filter.water.polys -------------------------------------------------
area.filter.water.polys <- function(region, neg.buffer = 500
                                    , area.min = 2e7
                                    , pop.min.perc = .05
                                    , return.map = F) {
  
  region.id = unique(region$id)
  cat("\ngetting water polys for", region.id, "\n")
  
  rwtr <- st_intersection(region, gsw)
  split <- st_split(st_buffer(region, -neg.buffer)
                    , rwtr) %>%
    rmapshaper::ms_explode()
  
  out <- split %>% st_transform(4326) %>%
    mutate(area = lwgeom::st_geod_area(.$geometry)) %>%
    filter(as.numeric(area) > area.min) %>%
    mutate(id = 1:nrow(.))
  
  if(return.map) 
    return(out)
  else
    return(
      data.frame(  region.id = region$region.id
                   ,region.name = region$region.name
                   ,region.type = region$region.type
                   ,n.water.polys = max(out$id)  )
    )
}



