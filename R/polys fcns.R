
#' subset.polys.divs
#'
#' Preps "division" data, whether water, highways, etc, to generate polygon
#' measure for supplied region. If, as with NHPN data, the divisions have an
#' "identifier column," the later arguments can be specified to control what
#' divisions are kept for polygon creation.
#' @param region 1 sf polygon to create sub-division measures for
#' @param div.sf sf object with division.
#' @param div.identifier.column If 'div.sf' has an identifier column to be used
#'   to filter by, supply here. Must be supplied for subsequent arguments to work.
#' @param divs.always.include Always include divisions that have this value in
#'   the identifier column
#' @param divs.include.intersecting Include additional divisions that don't have
#'   above identifier, but which intersect with divs that do.
#' @param remove.NA.divs Whether or not to remove all divisions with an NA in the identifier column.
subset.polys.divs <- function(region, div.sf,
                              div.identifier.column = NULL, 
                              divs.always.include = NULL, divs.include.intersecting = F,
                              remove.NA.divs = T, ...) {
  
  # trim to region
  div <- st_crop( div.sf
                  ,region )
  
  # End here if no hwy types excluded
  if( is.null(div.identifier.column) ) return(div)
  
  # remove NAs if 'remove.NA.divs'
  if(remove.NA.divs)
    div <- div %>% filter(! is.na(!!rlang::sym(div.identifier.column)) )
  
  # trim to always included types
  if( !is.null(divs.always.include) )
    divP <- div %>% 
      filter(!!rlang::sym(div.identifier.column)
             %in% divs.always.include)
  else divP <- div
  
  # end here if finished
  if( is.null(divs.include.intersecting) | 
      !divs.include.intersecting) return(divP)
  
  # Incl hwys that intersect otherwise ------------
  rt <- div %>%
    filter(! (!!rlang::sym(div.identifier.column) %in% divs.always.include) )
  
  touches.p <- st_filter(rt, divP)
  
  if(nrow(touches.p) == 0)
    out <- divP
  else
    out <- rbind(divP,touches.p)
  
  return(out)
}


#' polygonal.div
#'
#' Given a boundary and corresponding divisions, within boundary, filters by
#' div.type (currently hardcoded to SIGNT1 column), then finds number of
#' sub-polygons in region. Returns a dataframe with region identifiers and
#' number of polygons. Filter all subpolygons w/ area between 50,000 m^2 (these
#' comprise highway interchanges for example.) Also applies "negative buffer" --
#' i.e., shrinks the area to find polygons. This handles issues where hwy
#' resolution is slightly different from that of CZs or ends slightly earlier at
#' international boundary. It also means that if a hwy ends or changes class a
#' few meters from the cz boundary, it can still count as a division. (Operative
#' difference in Dallas, TX, for example)
polygonal.div <- function(  region, divs
                            , negative.buffer = 100
                            , min.size = 5e5
                            , min.population.count = 1000
                            , min.population.perc = NULL
                            , verbose = T, return.sf = F, ...) {
  require(lwgeom) 
  region.type = unique(region$region.type)
  region.id = unique(region$region.id)
  if(verbose) cat("generating", region.type,
                  "-",region.id, "\n" )
  
  initial.crs = st_crs(divs)
  
  # return w/ 0 if no included divisions overlap
  if( nrow(divs) == 0 )
    return( abv_out(region) %>% 
              select(region.id, region.name, region.type) %>%
              mutate(n.polys = 0) )
  
  # finally, calculate subdivisions
  polys <- 
    st_split(st_buffer(region
                       , -negative.buffer)
             , divs) %>%
    select(contains("region.")) %>%
    rmapshaper::ms_explode() %>%
    mutate(id = 1:nrow(.))
  
  polys <- st_make_valid(polys)
  
  polys <- handle.overlaps(polys)
  
  # filter by size if appropriate
  if( min.size > 0 )
    polys <- polys %>%
    st_transform(4326) %>%
    rmapshaper::ms_explode() %>%
    mutate(area = as.numeric(
           st_geod_area(geometry))) %>%
    filter(area > min.size) %>%
    st_transform(initial.crs) %>%
    mutate(id = 1:nrow(.))
  
  # filter by population if appropriate
  if( is.numeric(min.population.count) | 
      is.numeric(min.population.perc) )
    polys <- trim.polys.by.pop( polys, region.id, region.type
                                ,min.population.count, min.population.perc, return.sf)
  
  if(return.sf) return(polys)
  
  out <- polys %>% 
    select(region.id, region.name, region.type) %>%
    mutate(n.polys = max(polys$id)) %>%
    distinct()
  
  return(out)
}

#' trim.polys.by.pop
#' 
#' Trims polygons by population. Helper fcn called from polygonal.div if min.population has a non-null value.
#' CT table with population and tract identifiers is hardcoded in; expects 'cts' table in environment.
trim.polys.by.pop <- function(sub.polys, region.id,
                              region.type = "cz",
                              min.population.count = NULL, min.population.perc = NULL
                              , return.sf) {
  
  # filter cts to area
  cz.tracts = cts %>% 
    filter(!!rlang::sym(region.type) == region.id) %>% 
    st_make_valid() %>%
    select(geoid, population) # geoid gisjoin
  #  & get total population for area
  cz.pop = sum(cz.tracts$population)
  
  # buffer 0 pre-validate trick
  sub.polys <- sub.polys %>% st_buffer(0)
  cts <- cts %>% st_buffer(0)
  
  return.class = case_when(return.sf ~ "sf",
                           !return.sf ~ "tibble")
  
  polys = areal::aw_interpolate( sub.polys, tid = id   ### seems there is a bug here....
                                 ,cz.tracts, sid = geoid # gisjoin
                                 ,weight = "total" # tracts are co-terminous w/ czs
                                 ,extensive = c("population")
                                 ,output = return.class )
  
  polys = polys %>%
    mutate(pop.perc = population / cz.pop) 
  
  # filter by total population and/or percent, based on which arguments are supplied
  if( is.numeric(min.population.count) )
    polys = polys %>%
    filter(population > min.population.count)
  if( is.numeric(min.population.perc) )
    polys = polys %>%
    filter(pop.perc >= min.population)
  
  # count em up
  polys = polys %>%
    mutate(id = 1:nrow(.))
  
  return(polys)
}

#' handle.overlaps
#'
#' Some shapes (loops, conjoined loops) get a series of overlapping
#' polygons from st_split. Example, minneapolis BTS rails. This cuts out areas
#' so all sub polygons are non-overlapping.
handle.overlaps <- function(x) {
  # get polygons formed from overlaps
  overlaps <- x %>% st_intersection() %>% filter(grepl("POLYGON", st_geometry_type(.$geometry)))
  
  overlaps %>% select(-c(n.overlaps, origins))
}

# connecting segments -----------------------------------------------------

#' fill.single.gap
#'
#' Uses segment endpoints and connects those within threshold distance. Creates
#' continuous line that I think is as appropriate as possible.
fill.single.gap <- function(edge, nodes, threshold = 50, verbose = T, ...) {
  
  # add meters to threshold
  threshold = units::set_units(threshold, "m")
  
  # for each edge, find nearest edge on other node
  eligible.nodes = nodes[!nodes$nodeID %in% edge$nodeID,]
  neasest.to.i.nodes = edge %>%
    st_nearest_feature(eligible.nodes)
  
  # get nearest node(s) on other edges
  nn = eligible.nodes[neasest.to.i.nodes,]$nodeID
  
  # lines to nearest nodes...
  to.nn <- c(st_nearest_points(edge[1,],
                               eligible.nodes[eligible.nodes$nodeID == nn[1],])
             ,st_nearest_points(edge[2,],
                                eligible.nodes[eligible.nodes$nodeID == nn[2],]))
  
  to.nn <- unique(to.nn) %>% st_sfc(crs = st_crs(edge))
  
  edge$nn = nn 
  edge$to.nn = to.nn
  edge$dist2nn = geod.length(to.nn)
  # filter those below threshold & make geometry the line
  new.seg = edge[edge$dist2nn < threshold,]
  if(nrow(new.seg) == 0) {
    edge$geometry = NULL
    return(new.seg)
  }
  #cat("addressed gaps at ", unique(edge$SIGN1), "\n")
  
  new.seg = abv_out(new.seg) %>%
    rename(geometry= to.nn) %>% st_sf()
  
  return(new.seg)
}

#' fill.hwy.gaps
#'
#' maps fill.single.gap across output of previous functions. Takes ~denoded~
#' lines; expects nhpn hwy data.
fill.gaps <- function(hwy, threshold = 200, return.gap.map = F, ...) {
  hwy.type <- unique(hwy$SIGNT1)
  hwy.id <- unique(hwy$SIGN1)
  
  # subset to non-loops
  # (loops are very rare but problematic. Found one-- S213 in Boston. Use cat to locate others
  delooped <- hwy[st_startpoint(hwy) != st_endpoint(hwy), ]
  if(nrow(delooped) != nrow(hwy)) cat("Loop found in", hwy.id)
  
  # if no more than 1 non-loop segment, return HWYS early
  if( nrow(delooped) <= 1 ) {
    out <- hwy %>% select(SIGN1, SIGNT1, geometry) %>% mutate(id = 1)
    return(out)
  }
  
  cat("fixing",hwy.id,"\n")
  
  # to fill gaps, first get endpoints of existing segments
  hwy.n = find.endpoint.nodes(hwy)
  
  fillers = hwy.n %>%
    split(.$edge.id) %>%
    purrr::map(~fill.single.gap(., hwy.n, threshold = threshold), ...)
  
  # return early if no gaps filled
  if( all(map_lgl(fillers, ~(nrow(.) == 0))) )
    return(hwy)
  
  fillers <- do.call("rbind", fillers)
  
  # return map if appropriate
  if( return.gap.map )
    return(mapview(hwy) + mapview(fillers, color = "red"))
  
  # join segments with gap-fillers
  continuous.intst = st_union(hwy,
                              fillers) %>%
    st_union() %>%
    st_line_merge() %>%
    st_sf(SIGNT1 = hwy.type
          ,SIGN1 = hwy.id
          ,geometry = .)
  
  continuous.intst$id <- 1:nrow(continuous.intst)
  return(continuous.intst)
}



#' Fix all hwys
#' 
#' Fixes all hwys in place from raw data from subset of raw nhpn data
Fix.all.hwys <- function(hwy, return.gap.map = F, ...) {
  
  dn.hwy <- hwy %>%
    split(.$SIGN1) %>%
    imap( ~denode.lines(.) )
  
  hwy <- dn.hwy %>%
    imap( ~fill.gaps(., return.gap.map = return.gap.map), ...)
  
  if(return.gap.map)
    return(hwy[map_lgl(hwy, ~("mapview" %in% class(.)))])
  
  hwy <- do.call("rbind", hwy) %>% 
    ez.explode()
  
  return(hwy)
}


#Fix.all.hwys(int) %>% mapview()
#tmp <- Fix.all.hwys(int, return.sf = T)
#lac.c <- Fix.all.hwys(lac)



# hwys S4013 and S9 are weird
#lac.c <- Fix.all.hwys(lac, return.sf = T)
#tmp$I76


# wrapper fcn -------------------------------------------------------------

#' Polys.wrapper
#'
#' Wraps all the functions used to generate the polygonal subdivision measure.
#' @param region 1 sf polygon to create sub-division measures for
#' @param div.sf sf object with division.
#' @section Passed to Fix.all.hwys fcn
#' @param fill.gaps Whether to check for and may fill small gaps between line
#'   segments of division. Specify \code{threshold} for maximum gap size to
#'   be filled in.
#' @param threshold maximum gap size to be filled in, in meters, if fill.gaps is
#'   TRUE.
#' @section Passed to subsetting fcn subset.polys.divs
#' @param div.identifier.column If 'div.sf' has an identifier column to be used
#'   to filter by, supply here. Must be supplied for subsequent arguments to
#'   work. For NHPN data, this is likely "SIGNT1"
#' @param divs.always.include Always include divisions that have this value in
#'   the identifier column. I.e., Interstate for hwy identifier.
#' @param divs.include.intersecting Include additional divisions that don't the
#'   have above identifier, but which intersect with divs that do.
#' @param remove.NA.divs Whether or not to remove all divisions with an NA in
#'   the identifier column.
#' @section Passed to polygonal.div
#' @param negative.buffer Shrink region by this amount only for calculating
#'   polygons. Useful for handling shpfiles w/ different resolutions, especially
#'   for regions along an international border. Defaults to 100m.
#' @param min.size Minimum (area) size for population, in meters. Defaults to
#'   5e5, or 1/2 a km^2. Set to NULL to not filter by size.
#' @param min.population.count Minimum population that must fall into a polygon
#'   subdivision for it to be counted. Population is interpolated from census
#'   tracts based on % area overlap of CT within sub-polygon. Set to NULL or 0
#'   to filter by percent instead.
#' @param min.population.perc Minimum percent % of population that must fall
#'   into a polygon subdivision for it to be counted. Population is interpolated
#'   from census tracts based on % area overlap of CT within sub-polygon.
#' @param return.sf If true, returns an sf object, w/ one row per polygon
#'   subdivision, that can easily be mapped. If false (default), returns a one
#'   row df for region with columns for region identifiers and number of
#'   polygons.
Polys.wrapper <- function( region, div.sf, fill.gaps = F, ...) {
  
  divs <- subset.polys.divs(region, div.sf, ...)
  cat("subsetting done\n")
  if( fill.gaps ) {
    divs <- denode.lines(divs, group.cols = c("SIGNT1", "SIGN1"))
    divs <- Fix.all.hwys(divs, ...)
    cat("gaps filled\n")
  }
  out <- polygonal.div(region, divs, ...)
  
  return(out)
}


# troubleshooting ---------------------------------------------------------
'
tmpcz <- czs[grepl("Minneapolis" # "Corinth"
                   , czs$region.name), ]

#tmpcz <- czs[grepl( "Corinth"
#                   , czs$region.name), ]


mn.watr = Polys.wrapper(  region = tmpcz
                          , div.sf = gsw
                          , fill.gaps = F
                          , div.identifier.column = NULL
                          , divs.always.inculude = NULL
                          , divs.include.intersecting = NULL
                          , remove.NA.divs = F
                          , negative.buffer = 100
                          , min.size = 0
                          , min.population.count = 1000
                          , min.population.perc = NULL
                          , return.sf = T)


mn.watr %>% mapview(zcol="id") + mapview(st_crop(gsw,
                                                 tmpcz),color = "#800030")

st_crs(gsw) = st_crs(cts)
nhpn <- nhpn %>% conic.transform()
cts <- cts %>% conic.transform()  
czs <- czs %>% conic.transform()

mn.int = Polys.wrapper(  region = tmpcz
                         , div.sf = nhpn
                         , fill.gaps = T
                         , div.identifier.column = "SIGNT1"
                         , divs.always.include = "I"
                         , divs.include.intersecting = F
                         , remove.NA.divs = T
                         , negative.buffer = 100
                         , min.size = 0
                         , min.population.count = 1000
                         , min.population.perc = NULL # very low so i can just keep all of them and look
                         , return.sf = T)

mn.int %>%
  mapview(zcol="pop.perc") +
  mapview(st_crop(filter(nhpn, SIGNT1=="I"),
                  tmpcz),color = "#800030")


mn.lac = Polys.wrapper(  region = tmpcz
                         , div.sf = lac
                         , fill.gaps = T
                         , div.identifier.column = "SIGNT1"
                         , divs.always.include = NULL
                         , divs.include.intersecting = F
                         , remove.NA.divs = T
                         , negative.buffer = 100
                         , min.size = 0
                         , threshold = 500
                         , min.population.count = 1000
                         , return.sf = T)            # theres 1 5e-4 percent polygon with still 1,600 people.
# Also a 1e-5 % poly with 35 people. So maybe should just trim by absolute value

mn.lac %>%
  mapview(zcol="pop.perc") +
  mapview(st_crop(lac,
                  tmpcz),color = "#800030"
          ,lwd = 5)
'


'#  minneapolis rails
rail.mn = Polys.wrapper(tmpcz,
                        tmp
                        , fill.gaps = F
                        , div.identifier.column = "DIRECTION"
                        , negative.buffer = 100
                        , min.size = 5e5
                        , min.population.count = 250
                        , min.population.perc = NULL
                        , return.sf = T)
rail.mn %>% mapview(zcol = "population")
mapview(tmp)
cts %>%
  filter(czname=="Minneapolis") %>%
  mapview(zcol= "population" ) +
  mapview(tmp, lwd =3 , color = "red")
rail.mn
'

# this one got a topologyexception: cz 26801
'
Polys.wrapper(czs[czs$region.id == 26801, ],
              bts.nona
              , fill.gaps = F
              , div.identifier.column = NULL
              , negative.buffer = 100
              , min.size = 5e5
              , min.population.count = 250
              , min.population.perc = NULL
              , return.sf = T) #F

'
'
boston.polys <- polygonal.div(   boston
                                 ,tmp.divs
                                 ,return.sf = T  )
'
'
spf.polys <- polygonal.div(   spf
                              ,tmp.divs
                              ,return.sf = T
                              ,min.size = NULL
                              ,min.population.count = 50  )
'