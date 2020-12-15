
#' subset.polys.divs
#'
#' Preps "division" data, whether water, highways, etc, to generate polygon measure
#' for supplied region. If, as with NHPN data, the divisions have an "identifier
#' column," the later arguments can be specified to control what divisions are kept
#' for polygon creation.
#' @param region 1-row sf polygon to create sub-division measures for
#' @param div.sf sf object with division.
#' @param div.identifier.column If 'div.sf' has an identifier column to be used to
#'   filter by, supply here. Must be supplied for subsequent arguments to work.
#' @param always.include Always include divisions that have this value in the
#'   identifier column
#' @param include.intersecting Include additional divisions that don't have above
#'   identifier, but which intersect with divs that do.
#' @param remove.NA.divs Whether or not to remove all divisions with an NA in the
#'   identifier column.
#' @export subset.polys.divs
subset.polys.divs <- function(region, div.sf,
                              div.identifier.column = NULL,
                              always.include = NULL, include.intersecting = F,
                              remove.NA.divs = T, ...) {

  # trim to region
  divs <- st_crop( div.sf
                  ,region )

  # End here if no hwy types excluded
  if( is.null(div.identifier.column) ) return(divs)

  # remove NAs if 'remove.NA.divs'
  if(remove.NA.divs)
    divs <- divs %>% filter(! is.na(!!rlang::sym(div.identifier.column)) )

  # trim to always included types
  if( !is.null(always.include) )
    divP <- divs %>%
      filter(!!rlang::sym(div.identifier.column)
             %in% always.include)
  else
    divP <- divs

  # end here if finished
  if( is.null(include.intersecting) |
      !include.intersecting) return(divP)

  # Incl hwys that intersect otherwise ------------
  rt <- divs %>%
    filter(! (!!rlang::sym(div.identifier.column) %in% always.include) )

  touches.p <- st_filter(rt, divP)

  if(nrow(touches.p) != 0)
    divP <- rbind(divP,touches.p)

  return(divP)
}


#' polygonal.div
#'
#' Given a boundary and corresponding divisions, within boundary, filters by div.type
#' (currently hardcoded to SIGNT1 column), then finds number of sub-polygons in
#' region. Returns a dataframe with region identifiers and number of polygons. Filter
#' all subpolygons w/ area between 50,000 m^2 (these comprise highway interchanges
#' for example.) Also applies "negative buffer" -- i.e., shrinks the area to find
#' polygons. This handles issues where hwy resolution is slightly different from that
#' of CZs or ends slightly earlier at international boundary. It also means that if a
#' hwy ends or changes class a few meters from the cz boundary, it can still count as
#' a division. (Operative difference in Dallas, TX, for example)
#' @param negative.buffer Shrink region by this amount only for calculating polygons.
#'   Useful for handling shpfiles w/ different resolutions, especially for regions
#'   along an international border. Defaults to 100m.
#' @param min.size Minimum (area) size for population, in meters. Defaults to 5e5, or
#'   1/2 a km^2. Set to NULL to not filter by size.
#' @param min.population.count Minimum population that must fall into a polygon
#'   subdivision for it to be counted. Population is interpolated from census tracts
#'   based on % area overlap of CT within sub-polygon. Set to NULL or 0 to not apply
#'   the filter.
#' @param min.population.perc Minimum percent % of population that must fall into a
#'   polygon subdivision for it to be counted. Population is interpolated from census
#'   tracts based on % area overlap of CT within sub-polygon. Set to NULL or 0 to not
#'   apply the filter.
#' @param return.sf If true, returns an sf object, w/ one row per polygon
#'   subdivision, that can easily be mapped. If false (default), returns a one row df
#'   for region with columns for region identifiers and number of polygons.
#' @export polygonal.div
polygonal.div <- function(  region, divs
                            , negative.buffer = 100
                            , min.size = 5e5
                            , min.population.count = NULL
                            , min.population.perc = NULL
                            , verbose = F, return.sf = F, ...) {
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
             , divs)$geometry

  polys <- rmapshaper::ms_explode(polys)
  polys <- st_make_valid(polys) %>% st_sf()
  polys <- handle.overlaps(polys)

  # filter by size if appropriate
  if( min.size > 0 )
    polys <- polys %>%
    st_transform(4326) %>%
    mutate(area = as.numeric(
           st_geod_area(geometry))) %>%
    filter(area > min.size) %>%
    st_transform(initial.crs)

  # filter by population if appropriate
  if( is.numeric(min.population.count) |
      is.numeric(min.population.perc) )
    polys <- trim.polys.by.pop( polys, region.id, region.type
                                ,min.population.count, min.population.perc,
                                return.sf )

  # number the subpolygons
  polys$id = 1:nrow(polys)

  # return sf w/ each polygonal subdivision...
  if(return.sf)
    return(polys)

  # ... or a 1-row tibble with region ids and poly count.
  out <- tibble(
    region.id = region.id,
    region.type = region.type,
    region.name = region$region.name,
    n.polys = nrow(polys)
  )

  return(out)
}

#' trim.polys.by.pop
#'
#' Trims polygons by population. Helper fcn called from polygonal.div if
#' min.population has a non-null value. Uses helpers bundled in package (and depening
#' on divDat package) to construct tract populations.
#' @inheritParams polygonal.div
#' @importFrom areal aw_interpolate
trim.polys.by.pop <- function(sub.polys, region.id,
                              region.type = "cz",
                              min.population.count = NULL,
                              min.population.perc = NULL,
                              return.sf = F) {

  # filter cts to area
  cz.tracts = divM::ct.pops %>%
    filter(!!rlang::sym(region.type) == region.id) %>%
    select(geoid, population) %>%
    divM::attach.tract.geometry()

  #  & get total population for area
  cz.pop = sum(cz.tracts$population, na.rm = T)

  # buffer 0 pre-validate trick
  # sub.polys <- sub.polys %>% st_buffer(0)

  # add ids to polys for interpolation
  sub.polys$id = 1:nrow(sub.polys)

  # interpolate
  return.class = case_when(return.sf ~ "sf",
                           !return.sf ~ "tibble")

  polys = areal::aw_interpolate( sub.polys, tid = id
                                 ,cz.tracts, sid = geoid
                                 ,weight = "sum" #"total"
                                 ,extensive = c("population")
                                 ,output = return.class )

  polys = polys %>%
    mutate(pop.perc = population / cz.pop * 100)

  # filter by total population and/or percent, based on which arguments are supplied
  if( is.numeric(min.population.count) )
    polys = polys %>%
    filter(population > min.population.count)
  if( is.numeric(min.population.perc) )
    polys = polys %>%
    filter(pop.perc >= min.population)

  return(polys)
}


#' handle.overlaps
#'
#' Some shapes (loops, nested loops) get a series of overlapping
#' polygons from st_split. Example, minneapolis BTS rails. This cuts out areas
#' so all sub polygons are non-overlapping.
handle.overlaps <- function(x) {
  # get polygons formed from overlaps
  overlaps <- x %>%
    st_intersection() %>%
    filter(grepl("POLYGON", st_geometry_type(.$geometry)))

  overlaps %>% select(-c(n.overlaps, origins))
}

# connecting segments -----------------------------------------------------

#' fill.single.gap
#'
#' Uses segment endpoints and connects those within threshold distance. Creates
#' continuous line that I think is more appropriate. First two arguments
#' are taken from call within fill.gaps.
#' @param threshold Threshold in crs units. Fill gap if distance between endpoint of
#'   given edge and another in \code{lines} is < this threshold.
fill.single.gap <- function(edge, nodes, threshold = 200, ...) {

  # add meters to threshold
  threshold = units::set_units(threshold, "m")

  # for given edge, find nearest start/endpoint that is not a part of given edge
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

  new.seg = abv_out(new.seg) %>%
    rename(geometry = to.nn) %>% st_sf()

  return(new.seg)
}

#' fill.gaps
#'
#' maps fill.single.gap across all subsections of a single hwy. Takes ~denoded~ lines;
#' expects nhpn hwy data. The purpose is that if a highway has a short break
#' (<threshold) in the middle, this allows the polygon measure to ignore a short break.
#' Call with \code{return.gap.map=T} to visualize what it does.
#' @param hwy a single hwy, i.e., with single unique SIGN1, after divM::denode.lines
#'   is run on it
#' @param return.gap.map Return mapview leaflet to visualize output of fcn
#' @inheritDotParams fill.single.gap
fill.gaps <- function(hwy, return.gap.map = F, verbose = T, ...) {
  hwy.type <- unique(hwy$SIGNT1)
  hwy.id <- unique(hwy$SIGN1)

  # subset to non-loops
  # (loops are rare but problematic. One example -- S213 in Boston.
  delooped <- hwy[st_startpoint(hwy) != st_endpoint(hwy), ]
  if(nrow(delooped) != nrow(hwy)) cat("Loop found in", hwy.id)

  # if no more than 1 non-loop segment, return early (no gaps to fill)
  if( nrow(delooped) <= 1 ) {
    out <- hwy %>% select(SIGN1, SIGNT1, geometry) %>% mutate(id = 1)
    return(out)
  }

  # to fill gaps, first get endpoints of existing segments
  hwy.n = find.endpoint.nodes(hwy)

  fillers = hwy.n %>%
    split(.$edge.id) %>%
    purrr::map(~fill.single.gap(., hwy.n, threshold = threshold), ...)

  # return early if no gaps are filled
  if( all(purrr::map_lgl(fillers, ~(nrow(.) == 0))) )
    return(hwy)

  # alert user gap being filled if verbose
  if(verbose)
    cat("filling gap in",hwy.id,"\n")

  fillers <- do.call("rbind", fillers)

  # return map if appropriate
  if( return.gap.map )
    return(mapview::mapview(hwy) + mapview::mapview(fillers, color = "red"))

  # join segments with gap-fillers
  continuous.hwy = st_union(hwy,
                              fillers) %>%
    st_union() %>%
    st_line_merge() %>%
    st_sf(SIGNT1 = hwy.type
          ,SIGN1 = hwy.id
          ,geometry = .)

  continuous.hwy$id <- 1:nrow(continuous.hwy)
  return(continuous.hwy)
}



#' Fix all hwys
#'
#' Fixes all hwys in place from raw data from subset of raw nhpn data. Note these
#' functions expect NHPN format, i.e., SIGNT1 and SIGNN1 columns as identifiers.
#' @inheritParams fill.gaps
#' @inheritDotParams fill.single.gap
#' @import purrr
#' @export
Fix.all.hwys <- function(hwy, ...) {

  dn.hwy <- hwy %>%
    split(.$SIGN1) %>%
    purrr::imap( ~denode.lines(.) )

  hwy <- dn.hwy %>%
    purrr::imap( ~fill.gaps(., return.gap.map = return.gap.map), ...)

  if(return.gap.map)
    return(hwy[purrr::map_lgl(hwy, ~("mapview" %in% class(.)))])

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
#' @inheritParams polygonal.div
#' @inheritDotParams subset.polys.divs
#' @inheritDotParams Fix.all.hwys
#' @inheritDotParams polygonal.div
#' @export
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

