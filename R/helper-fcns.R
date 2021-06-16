
# region construction and tigris retrievals ------------------------------------

#' get.region.identifiers
#'
#' Gets region.id/region.name and attaches to region.type, to get bundled region id
#' information in format expected by other functions.
#'
#' @param cz,cbsa one of a cz or cbsa identifier code (either 5-digit # or 5-char
#'   numeric)
#'
#' @return a 1-row tibble that organizes the region id/name/type
#'
#' @export
get.region.identifiers <- function(
  cz = NULL,
  cbsa = NULL) {

  requireNamespace("xwalks")

  if (is.null(c(cz, cbsa)))
    stop("no non-null arguments")

  if (!is.null(cz)) {
    xw <- xwalks::co2cz
    type <- "cz"
    id <- cz
    name <- xw[xw$cz %in% cz, ]$cz_name[1]

  } else if (!is.null(cbsa)) {
    xw <- xwalks::co2cbsa
    type <- "cbsa"
    id <- cbsa
    name <- xw[xw$cbsa %in% cbsa, ]$cbsa_name[1]
  }

  return(tibble(
    region.type = type,
    region.id = id,
    region.name = name
  ))
}


# spatial convenience -------------------------------------------------------------

#' conic.transform
#'
#' Transforms sf spatial data to a conic projection with distance in meters
#' @export
conic.transform <- function(stdf,
                            crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45") {
  st_transform(stdf, crs)
}


#' geod.length
#'
#' Returns more accurate length for linestrings on the earth's surface as vector.
#' @importFrom lwgeom st_geod_length
#' @export
geod.length <- function(x)
  lwgeom::st_geod_length(sf::st_transform(x, 4326))


#' buffered.hull
#'
#' Creates a buffered convex hull around a polygon. Used to filter proximate
#' highways given a place.
#' @export
buffered.hull <- function(x, buffer = 300)
  st_buffer(st_convex_hull(x), buffer)

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
#' @export count.geo
count.geo <- function(x) {
  require(dplyr)
  require(sf)
  x %>%
    mutate(type =
             st_geometry_type(.$geometry)) %>%
    tibble() %>%
    count(type)
}




# probably to-delete -----------------------------------------------------------

#' attach.tract.geometry
#'
#' Attaches tract geometries and xwalk info to ct.pops. The reason for this function
#' is to avoid bundling geometries with this package, as they take up a lot of space.
#'
#' Called from `trim.polys.by.pop` -- an important dependency and it works well, but
#' it would improve poly fcns to delete this fcn and do this a different way
attach.tract.geometry <- function(cts) {

  # add geometries
  out <- st_sf(
    left_join(cts,
              divDat::cts))

  # remove water tracts with 0 or NA populations
  out <- out %>%
    filter(!st_is_empty(.))

  return(out)
}



#' region.reorg
#'
#' Turns columns with i.e., cz & cz_name to region.id/region.type format that is
#' expected in a variety of places in code that I've been writing.
#'
#' i may delete here; similar fcn in geoseg
#'
#' @param df table to rename
#' @param region.type A column in df containing region identifiers, with column
#'   itself indicating level or type of region.   #' not-exported region.reorg
region.reorg <- function(df, region.type) {

  # rename region.ids
  out <- df %>% rename("region.id" = !!rlang::sym(region.type))

  # if name column is found rename it to "region.name"
  if(paste0(region.type, "_name") %in% colnames(df))
    out <- out %>% rename("region.name" = paste0(region.type, "_name"))

  # add region.type column
  out$region.type <- region.type

  # ensure geometry column is last (if it exists)
  if("geometry" %in% colnames(df))
    out <- out %>% select(-geometry, geometry)

  return(out)
}
