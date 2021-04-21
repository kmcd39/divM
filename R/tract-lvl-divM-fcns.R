


#' tracts.across.division
#'
#' Given a subset of CTs and a set of linear division, gets whether the tracts are
#' across the division from one another
#'
#' @inheritParams tracts.from.region
#' @param div sf containing only linear features. Can have national features;
#'   subsetted within function
#' @param ctsf an sf object as returned by `tracts.from.region`. Can be NULL, in
#'   which case they are retrieved based on `region` argument
#' @param ... passed onto `polygonal.div`
#'
#' @return
#' @export tracts.across.division
tracts.across.division <- function(div,
                                   region,
                                   ctsf = NULL,
                                   cutout.water = F,
                                   ...) {

  # get all tracts for region
  if(is.null(ctsf))
    ctsf <- tracts.from.region(region,
                               cutout.water = cutout.water)

  # use div crs
  ctsf <- st_transform(ctsf, st_crs(div))

  # union counties to get region-wide geometry
  region <- region %>%
    cbind(geometry = st_union(ctsf)) %>%
    st_sf()

  # gen polys
  ppolys <-
    divM::polygonal.div(
      region,
      div,
      ...,
      return.sf = T) %>%
    rename(poly.id = id)

  # get % area of ct in each sub poly
  ct.poly <-
    xwalks::get.spatial.overlap(
      ctsf,
      ppolys,
      "geoid",
      "poly.id",
      filter.threshold = 0.01
    )

  # some CTs are split by divisions; attribute a ct to the division based on which
  # side contains the greatest share of the CT area
  ct.poly <- ct.poly %>%
    group_by(geoid) %>%
    filter(perc.area == max(perc.area))

  ct.poly <- tibble(ct.poly) %>%
    select(1,2)

  # xd: cross div associate each CT with which side of the divisions. They are across
  # a division from each other if they are on different sides


  xda <-
    cts["geoid"] %>%
    left_join(ct.poly) %>%
    rename(
      cta = 1,
      polya = poly.id)
  xdb <-
    cts["geoid"] %>%
    left_join(ct.poly) %>%
    rename(ctb = 1,
           polyb = poly.id)

  xd <- expand_grid(
    xda,
    xdb)

  xd$cross.div <-
    with(xd, polya != polyb)

  return(xd)
}



# wrapper ----------------------------------------------------------------------

Wrapper

#' wrapper flow:
#'
#' -start with cz/cbsa id -region call -> get tracts
#'
#' -call tracts.across.division or st_intersects or st_is_within_distance with list
#' of divisions
#'
#' - cbinds and write/return.
#'
Wrapper_gen.tract.div.measures <- function(  cz = NULL,
                                             cbsa = NULL,
                                             divs,
                                             cutout.water = F,
                                             ...) {

  region <- get.region.identifiers(cz, cbsa)
  ctsf <- tracts.from.region(region,
                             cutout.water = cutout.water)



}



# to delete(?) -----------------------------------------------------------------



#' tracts.touching.division
#'
#' Wraps st_intersects so that it can be mapped across czs easily.
#' [i think i delete this and just have a wrapper fcn that calls the others]
#'
#' @inheritParams tracts.across.division
#' @param st_fcn Geometric binary predicate function; `st_intersects` by default. Use
#'   `st_is_within_distance` for proximity
#' @param ... add'l arguments passed onto Geometric binary predicate fcn and/or
#'   `subset.polys.divs`
#'
#' @return For each CT in cz/cbsa, is the tract proximate to the division
tract.proxims2div <- function(div,
                              ctsf,
                              st_fcn = st_intersects,
                              cutout.water = F,
                              ...) {


  sbgp <- st_fcn(ctsf, div, ...)

  ctsf <- ctsf["geoid"] %>%
    mutate(div.prox =
             lengths(sbgp) > 0)

  return(ctsf)
}
