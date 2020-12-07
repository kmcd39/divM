
# convenience -------------------------------------------------------------

#' conic.transform
#'
#' Transforms sf spatial data to a conic projection with distance in meters
#' @export
conic.transform <- function(stdf, crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45") {
  st_transform(stdf, crs)
}


#' geod.length
#'
#' Returns more accurate length for linestrings on the earth's surface as vector.
#' @export
geod.length <- function(x) lwgeom::st_geod_length(sf::st_transform(x, 4326))

#' ez.explode
#'
#' Drops units from any columns that have them and then calls
#' rmapshaper::ms_explode(), which cannot handle units.
#' @export
ez.explode <- function(x) {
  x %>%
    mutate_if(~"units" %in% class(.)
              ,as.numeric) %>%
    rmapshaper::ms_explode() %>%
    select(all_of(colnames(x)))
}

#' buffered.hull
#'
#' Creates a buffered convex hull around a polygon. Used to filter proximate
#' highways given a place.
#' @export
buffered.hull <- function(x, buffer = 300) st_buffer(st_convex_hull(x), buffer)

#' abv_out
#'
#' abbreviates a spatial df by dropping geometry
#' @export
abv_out <- function(sf_df) {
  require(dplyr)
  sf_df %>%
    tibble() %>%
    select(-geometry)
}


#' count.geo
#'
#' Counts geometry types of sf object.
#' @export
count.geo <- function(x) {
  require(dplyr)
  require(sf)
  x %>%
    mutate(type =
             st_geometry_type(.$geometry)) %>%
    tibble() %>%
    count(type)
}


# open street map query ---------------------------------------------------

#' osm.query
#'
#' Extracts bbox from supplied base sf then formats for osm api and extracts
#' requested features.
#' @param basesf sf object to query osm over bbox of
#' @param features osm features to request. See \code{?add_osm_feature} or
#' https://wiki.openstreetmap.org/wiki/Map_Features
#' @examples phr <- osm.query(divDat::czs[1,], "railway")
#' @export
osm.query <- function(basesf, features) {
  require(sf)
  require(osmdata)
  bboxstr <-
    matrix(
      st_bbox(st_transform(basesf, 4326)), nrow = 2,
      dimnames = list( c("x","y")
                       ,c("min","max"))
    )

  osm_sf <- opq(bboxstr) %>%
    add_osm_feature (features) %>%
    osmdata_sf

  return(osm_sf)
}


# deprecated --------------------------------------------------------------


#' build_identifier_df
#'
#' Helper function for fcns that split list of division types x regions. Designed for retaining
#' identifiers; i.e., cz & cz_name for regions.
build_identifier_df <- function(df, split_cols) {

  ids <-
    suppressMessages(
      purrr::map_dfc(split_cols,
                     ~unique(data.frame(pull(df, .)))))

  colnames(ids) <- split_cols
  return(ids)
}


#' return.to.polygon
#'
#' Transforms point output from largest.centers.in.region fcn back to polygons.
#' Assumes column names were unchanged.
return.to.polygon <- function(point.data, poly.data) {

  point.data %>%
    abv_out() %>%
    left_join(poly.data) %>%
    st_sf()
}

