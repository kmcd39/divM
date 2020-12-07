#' this script provides functions for getting the proximity proportion and mean
#' centroid dist-to-division measures. They provide functions both to get the
#' measure for a particular region and to map them across all regions.
#'
#' There's a set of functions for re-projecting spatial data based on its
#' centroid. This can be used to use distance- or area-preserving map
#' projections centered on local areas. This should only be necessary when a
#' function that can only operate on planar geometries is needed.



#' local_equidistant_project
#'
#' Projects a shapefile to an Azimuth Equidistant Projection centered on a
#' supplied centroid, or if \code{projection_center} is null, on a centroid
#' automatically generated for the supplied sf object.
#' @export
local_equidistant_project <- function(x, projection_center = NULL) {
  x <- st_sf(x)
  if (is.null(projection_center))
    projection_center <- geosphere::centroid(st_coordinates(x)[,c("X", "Y")])

  x %>%
    st_transform(crs = paste0("+proj=aeqd +lat_0=",projection_center[2],
                              " +lon_0=",projection_center[1]))
}

#' get_azimuthal_projection_centroid
#'
#' Gets a centroid of sf object x, or midpoint if a straight line is provided
get_azimuthal_projection_centroid <- function(x) {
  # longlat project input and extract coordinates
  coords <- st_coordinates(
    st_transform(st_as_sf(x),
                 4326))
  # if possible to create poitns from polygon, calculate centroid
  if (nrow(st_df) > 2)
    projection_centroid <-
      geosphere::centroid(st_df[,c("X", "Y")])
  # if not enough points, caluclate midpoint
  else
    projection_centroid <-
      geosphere::midPoint(st_df[1,c("X", "Y")],
                          st_df[2,c("X", "Y")])

  return( projection_centroid )
}

#' proximity_div_proportion
#'
#' Calculates the proportion of a population in a study area k meters from division.
#' pop_col specifies the colname for column in \code{centroid_df} that contains CT populations
#' #' @export
proximity_div_proportion <- function(centroid_df, div_df, identifier_column, k = 400, pop_col = "population", verbose = FALSE) {
  if(unique(pull(centroid_df, identifier_column))
     != unique(pull(div_df, "region.id"))) stop("region identifier is different across supplied dataframes")

  if(verbose)
    cat("generating ",unique(div_df$region.name), "-", unique(div_df$region.id)," (",identifier_column, ")\n",
        "ct count:", unique(div_df$cts), "found centroids:",nrow(centroid_df),"\n")

  # generate NA for this measure for areas w/ only one census tract
  if( #unique(div_df$cts) <= 1 |
    nrow(centroid_df) <= 1)
    return(mutate(centroid_df, proximity_proportion = NA))

  projection_centroid <- get_azimuthal_projection_centroid(centroid_df)

  centroid_df <- local_equidistant_project(centroid_df, projection_centroid)
  div_df <- local_equidistant_project(div_df, projection_centroid)

  proximity_df <- st_buffer( centroid_df , k)

  within_threshold <- st_filter(proximity_df, div_df,  join = st_intersects)

  out <- abv_out(within_threshold) %>%
    mutate(proximity_proportion =
             sum(pull(within_threshold, !!pop_col), na.rm = T ) /
             sum(pull(centroid_df, !!pop_col), na.rm = T ))
  return(out)
}
#proximity_div_proportion(point_dat, mn_hwys, 1200, "POPULATION")

#' get_mean_centroid_dist2div
#'
#' Calculated mean centroid distance within a study area to geographic
#' divisions. Takes as input a spatial dataframe for centroids and another with
#' divisions with a shared geographic identifier column.
#' @export
mean_centroid_dist2div <- function(centroid_df, div_df, identifier_column, pop_col = "population", verbose = FALSE) {
  if(unique(pull(centroid_df, identifier_column))
     != unique(pull(div_df, "region.id"))) stop("region identifier is different across supplied dataframes")

  cat("generating ",unique(div_df$region.name), "-", unique(div_df$region.id)," (",identifier_column, ")\n",
      "ct count:", unique(div_df$cts), "found centroids:",nrow(centroid_df),"\n")

  # generate NA for this measure for areas w/ only one census tract
  if( #unique(div_df$cts) <= 1 |
    nrow(centroid_df) <= 1)
    return(mutate(centroid_df, mean_dist2div = NA))

  projection_centroid <- get_azimuthal_projection_centroid(centroid_df)

  centroid_df <- local_equidistant_project(centroid_df, projection_centroid)
  div_df <- local_equidistant_project(div_df, projection_centroid)

  # taking the union dissolves all segments and division types to one collection.
  div_df <- st_union(div_df)

  proximity_df <- centroid_df %>%
    mutate(dists_to_div =
             st_length(st_nearest_points(centroid_df, div_df)))

  out <- abv_out(proximity_df) %>%
    mutate(mean_dist2div =
             stats::weighted.mean(proximity_df$dists_to_div,
                                  pull(centroid_df, !!pop_col)))
  return(out)
}



# region mappers ----------------------------------------------------------

#' proximity_div_proportion.region_mapper
#'
#' Given a df of centroids and a df of divisions, with common region (cz, cbsa,
#' county) identifiers, this maps the \code{proximity_div_proportion} function
#' across the identified regions. \code{geo_level} parameter should identify the region.type.
#' @export
proximity_div_proportion.region_mapper <- function(centroid_df, div_df, geo_level, k = 800, verbose = F) {

  # filter NAs and split CT centroids by appropriate region type
  centroidL <- centroid_df %>%
    filter(!is.na(!!rlang::sym(geo_level))) %>%
    group_split(!!rlang::sym(geo_level))

  names(centroidL) <- names_from_col(centroidL, geo_level)

  # do the same with divisions
  divL <- divs[[geo_level]] %>%
    group_split(region.name, region.id, region.type)

  names(divL) <- names_from_col(divL, "region.id")

  # drop centroids for which no div info exists (this is PR cbsa's-- see note in script)
  centroidL <- centroidL[names(centroidL) %in% names(divL)]

  # map the two df lists across the calculator fcn
  # -- ensure the region lists are ordered identically w/ re-indexing
  metrics <-
    map2_dfr(centroidL, divL[names(centroidL)],
             ~proximity_div_proportion(.x, .y, geo_level, k, verbose = verbose))

  # clean and output
  keep_cols <-  c(geo_level, "proximity_proportion")

  metrics <- metrics %>%
    select(all_of(keep_cols)) %>%
    unique()

  return(metrics)
}

#proximity_div_proportion.region_mapper(points, divs$cbsa,  "cbsa")
#proximity_div_proportion.region_mapper(centroids$cz, divs$cz,  "cz")


#' mean_centroid_dist2div.region_mapper
#'
#' Given a df of centroids and a df of divisions, with common region (cz, cbsa,
#' county) identifiers, this maps the \code{mean_centroid_dist2div} function
#' across the identified regions. \code{geo_level} parameter should identify the region.type.
#' @export
mean_centroid_dist2div.region_mapper <- function(centroid_df, div_df, geo_level, verbose = F) {

  # filter NAs and split CT centroids by appropriate region type
  centroidL <- centroid_df %>%
    filter(!is.na(!!rlang::sym(geo_level))) %>%
    group_split(!!rlang::sym(geo_level))

  names(centroidL) <- names_from_col(centroidL, geo_level)

  # do the same with divisions
  divL <- divs[[geo_level]] %>%
    group_split(region.name, region.id, region.type)

  names(divL) <- names_from_col(divL, "region.id")

  # drop centroids for which no div info exists (this is PR cbsa's-- see note in script)
  centroidL <- centroidL[names(centroidL) %in% names(divL)]

  # map the two df lists across the calculator fcn
  # -- ensure the region lists are ordered identically w/ re-indexing
  metrics <-
    map2_dfr(centroidL, divL[names(centroidL)],
             ~mean_centroid_dist2div(.x, .y, geo_level, verbose = verbose))

  # clean and output
  keep_cols <-  c(geo_level, "mean_dist2div")

  metrics <- metrics %>%
    select(all_of(keep_cols)) %>%
    unique()

  return(metrics)
}
#mean_centroid_dist2div.region_mapper(centroids$cz, divs$cz,  "cz")
