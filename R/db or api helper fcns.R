
#' tracts.from.region
#'
#' @param region.ids 1-row tibble with region id/name/type, as returned by
#'   `get.region.identifiers`.
#' @param query_fcn function to query geos from census api. Defualt gets tracts
#' @param ... passed onto `query_fcn`
#'
#' @return associated census tracts (or other geometry queried from census api)
#'
#' @export tracts.from.region
tracts.from.region <- function(region, query_fcn = tigris::tracts,
                               cutout.water = F, ...) {

  # get all tracts for region
  requireNamespace("xwalks")

  params <- list(...)

  cts <-
    xwalks::ctx %>%
    filter(!!rlang::sym(region$region.type) ==
             region$region.id)

  # get tract geometries
  .countyfps <- unique(cts$countyfp)
  ctsf <- map_dfr(.countyfps,
                  ~do.call(query_fcn,
                           c(list(substr(.x, 1,2),
                                  substr(.x, 3,5)),
                             params))) %>%  # (ellipsis has to be passed onto both fcn and map calls)
    rename_with(tolower)

  # remove water areas if appropriate
  if(cutout.water)
    ctsf <- download.and.cutout.water(ctsf["geoid"])

  return(ctsf)

}

# getting tigris water areas ---------------------------------------------------

#' get.water.for.region
#'
#' Given a cz,cbsa, or county identifier/geoid, wraps `tigris::area_water` for the
#' overlapping counties, interfacing with my `xwalks` library to get overlap.
#' Returns all water areas above the size minimum for the given region.
#'
#' @param ... CZ, CBSA, county, or places, passed on to `xwalks::x2cos`
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#' @return water areas for region.
#'
#' @export
get.water.for.region <- function(...,
                                 size.min = 5e6) {

  requireNamespace("xwalks")
  .countyfp <- xwalks::x2cos(...)

  # download water
  water <- map_dfr(.countyfp,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5))
  )

  # union and explode water
  water <- st_union(water) %>% st_cast("POLYGON") %>% st_sf()

  # filter by size of union'd body
  water <- water %>% filter(as.numeric(st_area(.$geometry)) > size.min )

  return(water)
}

#' get.clean.tigris.waterN
#'
#' Wraps `tigris::area_water`; takes an sf argument with an id column that follows
#' county/tract/blockgroup census ID, so that first 2 digits are statefp and latter 3
#' are countyfp. Downloads appropriate regions and erases them from input sf x. I
#' think being replaced by `get.water.for.region`, so isn't exported.
#'
#' @param region a df/tibble containing region identifiers, as created by
#'   `divM::get.region.identifiers`
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#'
#'
download.and.cutout.water <- function(x,
                                      id.col = "geoid",
                                      size.min = 5e6) {

  # all 5-char state-counties
  counties <-
    pull(x, id.col) %>%
    substr(1, 5) %>%
    unique()

  # download water
  water <- map_dfr(counties,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5))
  )
  # union and explode water
  water <- st_union(water) %>% st_cast("POLYGON") %>% st_sf()
  # filter by size of union'd body
  water <- water %>% filter(as.numeric(st_area(.$geometry)) > size.min )
  water <- water %>% st_transform(st_crs(x))

  if(nrow(water) == 0)
    return(x)

  xt <- st_difference(x, st_union(water))

  return(xt)
}


# from sql

#' db.query.and.cutout.water
#'
#' Queries a spatial database for tigris water areas and erases those water areas
#' from the supplied region to return only land areas. I could refactor this
#' connection to just get water areas from \code{tigris} too...
#' @param con Active connection with a divs.water_tigris table
#' @param region an sf object from which to erase water areas
#' @param sf.simplify Whether to apply rmapshaper::ms_simplify to water areas before
#'   trimming
#' @param trim.unnamed whether or not to retain unnamed water areas. These tend to be
#'   very small and may introduce a lot of unneeded complexity to supplied region.
#' @export
db.query.and.cutout.water <- function(con, region,
                                      sf.simplify = TRUE,
                                      trim.unnamed = T, trim.by.area = 2e6) {

  require(sf)

  water <- dblinkr::query.division(con,
                                   st_transform(region, 4326),
                                   "divs.water_tigris")
  st_crs(water) <- 4326

  if(trim.unnamed) {
    named <- water[!is.na(water$FULLNAME), ]
    sgbp <- st_intersects(water, named)
    water <- water[lengths(sgbp) > 0, ]
  }

  if(sf.simplify)
    water <- rmapshaper::ms_simplify(water)

  # (this happens here so adjacent water entities are merged before area check)
  water <- st_union(water)

  if(!is.null(trim.by.area) && trim.by.area > 0) {
    require(lwgeom)
    water <- water %>% rmapshaper::ms_explode()
    water <- st_sf(geometry = water)
    water$area = st_geod_area(water$geometry) %>% as.numeric()
    water <- water %>% filter(area > trim.by.area) %>% st_union()
  }

  water <- st_transform(water,
                        st_crs(region))

  water_trimmed <- st_difference(region,
                                 water)

  return(water_trimmed)
}


# open street map query ---------------------------------------------------


#' osm.query
#'
#' Extracts bbox from supplied base sf, formats for osm api and extracts
#' requested features.
#' @param basesf sf object to query osm over bbox of
#' @param features osm features to request. See \code{?add_osm_feature} or
#'   https://wiki.openstreetmap.org/wiki/Map_Features
#' @examples
#' \dontrun{phr <- osm.query(divDat::czs[1,], "railway") }
osm.query <- function(basesf, features) {
  require(sf)

  bboxstr <-
    matrix(
      st_bbox(st_transform(basesf, 4326)), nrow = 2,
      dimnames = list( c("x","y")
                       ,c("min","max"))
    )

  osm_sf <- osmdata::opq(bboxstr) %>%
    osmdata::add_osm_feature(features) %>%
    osmdata::osmdata_sf

  return(osm_sf)
}

