# getting tigris water areas ---------------------------------------------------


# from tigris/census api more directly

#' get.clean.tigris.water
#'
#' Wraps `tigris::area_water`; takes an sf argument with an id column that follows
#' county/tract/blockgroup census ID, so that first 2 digits are statefp and latter 3
#' are countyfp. Downloads appropriate regions and erases them from input sf x.
#' @param x,id.col sf object with a geoid `id.col`  (5-digit for counties; 11 for
#'   tracts; etc.)
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#' @export
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

