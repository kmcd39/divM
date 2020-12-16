

# getting tigris water areas ---------------------------------------------------

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

# from tigris/census api more directly

#' get.clean.tigris.water
#'
#' Queries a spatial database for tigris water areas and erases those water areas
#' from the supplied region to return only land areas. I could refactor this
#' connection to just get water areas from \code{tigris} too...
#' @param cz,counties a character vector containing CZ or county ids. County ids
#'   should be 5 characters (concatenated state + county fp codes). One of these
#'   should be supplied, the other left as null.
#' @param trim.unnamed whether or not to retain unnamed water areas. These tend to be
#'   very small and may introduce a lot of unneeded complexity to supplied region.
#' @param out.crs output crs. Can be left as NULL to retain default from tigris.
#' @param ... Additional arguments passed on to \code{tigris::area_water}
#' @importFrom tigris area_water
#' @export
get.clean.tigris.water <- function(czs = NULL, counties = NULL,
                                   trim.unnamed = T, out.crs = NULL, ...) {#, trim.by.area = 1e6) {

  # get corresponding counties if cz was supplied
  if(!is.null(czs) & is.null(counties)) {

    xw <- xwalks::co2cz %>% filter(cz %in% czs)
    counties <- xw$countyfp
  }

  # separate out state and county fp codes
  states <- substr(counties, 1,2)
  counties <- substr(counties, 3,5)

  water <-
    purrr::map2_dfr(.x = states,
                    .y = counties,
                    ~tigris::area_water(state = .x,
                                        county = .y,
                                        ...))

  # some idiosyncratic cases where a water feature is split up into many parts, only
  # some of which have names, so this trims to all named features and those
  # touching named features.
  if(trim.unnamed) {
    named <- water[!is.na(water$FULLNAME),]
    sgbp <- st_intersects(water, named)
    water <- water[lengths(sgbp) > 0]
  }

  # trim to size option?

  # set crs if specified
  if(!is.null(out.crs))
    water <- water %>% st_transform(st_crs(out.crs))


  water <- st_union(water)

  return(water)
}

# open street map query ---------------------------------------------------


#' osm.query
#'
#' Extracts bbox from supplied base sf then formats for osm api and extracts
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

