

# specific  for 'meta polys' ----------------------------------------------------

#' dissolve.to.boundaries.w.ID
#'
#' Dissolves polygons to linear divisions, keeping some identifier columns specified
#' with dtype and admin aguments. Drops other pre-existing columns
#' @param sf sf object
#' @param divtype,admin additional identifiers to be bundled with the dissolved sf.
#' @export
dissolve.to.boundaries.w.ID <- function(sf, divtype, admin) {
  sf <- st_combine(sf)
  sf <- st_boundary(sf)
  sf <- rmapshaper::ms_explode(sf)
  sf <- st_sf(
    divtype = divtype
    ,admin = admin
    ,geometry = sf
  )
  return(sf)
}



#' Meta.Polys.wrapper
#'
#'
