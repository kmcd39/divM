
#' subset.polys.divs
#'
#' Preps "division" data, whether water, highways, etc, to generate polygon measure
#' for supplied region. If, as with NHPN data, the divisions have an "identifier
#' column," the later arguments can be specified to control what divisions are kept
#' for polygon creation.
#' @param region 1-row sf polygon with `region.id`/`region.type` columns to create
#'   sub-division measures for
#' @param div.sf sf object with division.
#' @param div.identifier.column If 'div.sf' has an identifier column to be used to
#'   filter by, supply here. Must be supplied for other subset arguments to work;
#'   `NULL` by default.
#' @param always.include Always include divisions that have this value in the
#'   identifier column
#' @param include.intersecting Include additional divisions that don't have above
#'   identifier, but which intersect with divs that do.
#' @param remove.NA.divs Whether or not to remove all divisions with an NA in the
#'   identifier column.
#' @export subset.polys.divs
subset.polys.divs <- function(region, div.sf,
                              div.identifier.column = NULL,
                              always.include = NULL,
                              include.intersecting = F,
                              remove.NA.divs = T, ...) {

  # trim to region
  divs <- st_crop( div.sf
                  ,region )

  # End here if no hwy types excluded
  if( is.null(div.identifier.column) )
    return(divs)

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
  if( is.null(include.intersecting) ||
      !include.intersecting)
    return(divP)

  # Incl hwys that intersect otherwise ------------
  rt <-
    divs %>%
    filter(! (!!rlang::sym(div.identifier.column) %in% always.include) )

  touches.core <- st_filter(rt, divP)

  if(nrow(touches.core) != 0)
    divP <- rbind(divP, touches.core)

  return(divP)
}


#' polygonal.div
#'
#' Given a boundary and corresponding divisions within boundary,  finds number of
#' sub-polygons in region. Returns a dataframe with region identifiers and number of
#' polygons. Filter all subpolygons w/ area between 50,000 m^2 (these comprise
#' highway interchanges for example.) Also applies "negative buffer" -- i.e., shrinks
#' the area to find polygons. This handles issues where hwy resolution is slightly
#' different from that of CZs or ends slightly earlier at international boundary. It
#' also means that if a hwy ends or changes class a few meters from the cz boundary,
#' it can still count as a division. (Operative difference in Dallas, TX, for
#' example)
#' @inheritParams subset.polys.divs
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
  if(!all(c("region.id"  , "region.type", "region.name", "geometry") %in%
          colnames(region)))
    warning('warning: polygon function expects columns c("region.id", "region.type", "region.name", "geometry")
            in first supplied argument')


  require(lwgeom)
  region.type <- unique(region$region.type)
  region.id <-  unique(region$region.id)
  if(verbose)
    cat("generating", region.type, "-",region.id, "\n" )

  initial.crs <- st_crs(divs)

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

  # explode from GEOCOLLECTION/MULTIPOLYGON
  polys <- polys %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON")
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
    polys <- trim.polys.by.pop( polys,
                                region.id,
                                region.type = region.type,
                                min.population.count = min.population.count,
                                min.population.perc = min.population.perc,
                                return.sf = return.sf )

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

  # population-by-ct is bundled with package; attach crosswalk
  area.cts <-
    divM::ct.pops %>%
    left_join(xwalks::ctx[,c("geoid", "cbsa")])

  # filter to current area based on supplied region type.
  area.cts  <-
    area.cts %>%
    filter(!!rlang::sym(region.type) == region.id) %>%
    select(geoid, population) %>%
    attach.tract.geometry()

  #  & get total population for area
  area.pop <- sum(area.cts$population, na.rm = T)

  # buffer 0 pre-validate trick
  # sub.polys <- sub.polys %>% st_buffer(0)

  # add ids to polys for interpolation
  sub.polys$id <- 1:nrow(sub.polys)

  # interpolate population from CTs to polygons
  return.class <- case_when(return.sf ~ "sf",
                           !return.sf ~ "tibble")

  polys <- areal::aw_interpolate( sub.polys, tid = id
                                 ,area.cts, sid = geoid
                                 ,weight = "sum" #"total"
                                 ,extensive = c("population")
                                 ,output = return.class )

  polys$pop.perc <- polys$population / area.pop

  # filter by total population and/or percent, based on which arguments are supplied
  if( is.numeric(min.population.count) )
    polys  <- polys %>%
    filter(population > min.population.count)

  if( is.numeric(min.population.perc) )
    polys <- polys %>%
    filter(pop.perc >= min.population.perc)

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


# wrapper fcn -------------------------------------------------------------

#' Polys.wrapper
#'
#' Wraps all the functions used to generate the polygonal subdivision measure.
#' @inheritParams polygonal.div
#' @inheritDotParams subset.polys.divs
#' @inheritDotParams Fix.all.hwys
#' @inheritDotParams polygonal.div
#' @export Polys.wrapper
Polys.wrapper <- function( region, div.sf, fill.gaps = T, ...) {

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

