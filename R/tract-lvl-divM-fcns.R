


#' tracts.across.division
#'
#' Given a subset of CTs and a set of linear division, gets whether the tracts
#' are across the division from one another
#'
#' @inheritParams tracts.from.region
#' @param div sf containing only linear features. Can have national features;
#'  subsetted within function
#' @param ctsf an sf object as returned by `tracts.from.region`. Can be NULL, in
#'  which case they are retrieved based on `region.ids` argument
#' @param ... passed onto `polygonal.div`
#'
#' @return a data.frame with one row per census tract in the supplied region,
#'  with a geoid column and a poly.id column indicating which "polygon division"
#'  each tract is in. Tracts with different identifiers in this column are
#'  across a division from one another.
#'
#' @export tracts.across.division
tracts.across.division <- function(div,
                                   region.ids,
                                   ctsf = NULL,
                                   cutout.water = F,
                                   ...) {

  # get all tracts for region
  if(is.null(ctsf))
    ctsf <- tracts.from.region(region.ids,
                               cutout.water = cutout.water)

  # use div crs
  ctsf <- st_transform(ctsf, st_crs(div))

  # union areas to get region-wide geometry
  region <- region.ids %>%
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
      filter.threshold = 0.00
    )

  # some CTs are split by divisions; attribute a ct to the division based on which
  # side contains the greatest share of the CT area
  ct.poly <- ct.poly %>%
    group_by(geoid) %>%
    filter(perc.area == max(perc.area)) %>%
    ungroup()

  ct.poly <- tibble(ct.poly) %>%
    select(1,2)

  return(ct.poly)
}





#' tracts.across.water
#'
#' Water divisions are areas, rather than
#'
#' @inheritParams tracts.across.division
#' @param .cos counties, as downloaded from tigris. Downloads them if left as
#'   null.
#' @param area.floor area floor for water polygon in sq km. I think leaving at 0
#'   and effectively filtering with the tract overlay will be best.
#' @param write.path a file path to write output to.
#'
#' @export tracts.across.water
tracts.across.water <- function(cz = NULL, cbsa = NULL,
                                ctsf = NULL,
                                .cos = NULL,
                                area.floor = 0.00,
                                write.path = NULL,
                                ...) {

  region.ids <- get.region.identifiers(cz, cbsa)

  # get counties overlapping region
  if(is.null(.cos))
    .cos <- tigris::counties()

  colnames(all.cos) <-
    tolower(colnames(all.cos))

  .cos <- sfg.seg::geo.subset.cbgs(.cos,
                                   "geoid",
                                   cz = cz, cbsa = cbsa)

  # get tracts if not supplied
  if(is.null(ctsf))
    ctsf <- tracts.from.region(region.ids)

  # union areas to get region-wide geometry
  region <- region.ids %>%
    cbind(geometry = st_union(ctsf)) %>%
    st_sf()

  # get water areas
  .wtr <-
    visaux::water.wrapper(.cos$geoid,
                          x = region)

  # define a "no division" table to return early if there are none
  no.divs <- tibble(ctsf) %>%
    select(geoid) %>%
    mutate(poly.id = 1,
           cbsa = cbsa, cz = cz)

  # if no water areas, skip
  if(nrow(.wtr) == 0) {
    ct.poly <- no.divs
  } else {

    # trim from larger region area - rnw region no water
    .rnw <- st_difference(region,
                          st_union(.wtr))

    # explode to 1 row/polygon
    .rnw <- .rnw %>% st_cast("POLYGON")

    # get areas in KM^2 & filter based on floor
    .rnw$area <- st_area(.rnw$geometry)
    .rnw$area <- with(.rnw, as.numeric(area) / 1e6)
    .rnw <- .rnw %>%
      filter(area > area.floor)

    # again, skip if no subpolys after clean
    if(nrow(.rnw) <= 1)
      return(no.divs)

    .rnw <- .rnw %>%
      mutate(poly.id = row_number())

    # get % area of ct in each sub poly
    ct.poly <-
      xwalks::get.spatial.overlap(
        ctsf,
        .rnw,
        "geoid",
        "poly.id",
        filter.threshold = 0.00
      )

    # some CTs are split by divisions; attribute a ct to the division based on which
    # side contains the greatest share of the CT area
    ct.poly <- ct.poly %>%
      group_by(geoid) %>%
      filter(perc.area == max(perc.area)) %>%
      ungroup()

    ct.poly <- tibble(ct.poly) %>%
      select(1,2) %>%
      mutate( cbsa = cbsa,
              cz = cz )
  }

  if(!is.null(write.path))
    sfg.seg::write.running.table(ct.poly,
                                 write.path)

  return(ct.poly)
}

# wrapper ----------------------------------------------------------------------


# wrapper flow:
#
# -start with cz/cbsa id -region call -> get tracts
#
# -call tracts.across.division or st_intersects or st_is_within_distance with list
# of divisions
#
# - cbinds and write/return.

#' Wrapper_gen.tract.div.measures
#'
#'
#' @param ... passed onto `tracts.across.division` and/or `subset.polys.divs`
#' @inheritParams get.region.identifiers
#' @inheritParams tracts.from.region
#' @param clean.nhpn Logical; whether or not to clean divs as if they are
#'   NHPN hwy data by applying cleaning fcns `denode.lines` and `Fix.all.hwys`. Can
#'   be a vector of length `divs` to clean for some measures.
#'
#' @export Wrapper_gen.tract.div.measures
Wrapper_gen.tract.div.measures <- function(  cz = NULL,
                                             cbsa = NULL,
                                             divs, # NAMED list
                                             cutout.water = F,
                                             clean.nhpn = F,
                                             ...) {

  params <- list(...)

  region.ids <- get.region.identifiers(cz, cbsa)

  ctsf <- tracts.from.region(region.ids,
                             cutout.water = cutout.water,
                             year = 2019)

  ctsf <- ctsf %>% conic.transform()
  divs <- divs %>% map( conic.transform )

  # subset divs
  divs <- map(divs,
              ~do.call(
                subset.polys.divs,
                c(list(ctsf, .x), params)))

  # clean divs when appropriate.
  if(length(clean.nhpn) == 1)
    clean.nhpn <- rep(clean.nhpn, length(divs))

  divs <- map2(divs, clean.nhpn,
               ~{ if( .y &
                     !is.null(.x) &
                     nrow(.x) > 0)
                 .x %>%
                   denode.lines() %>%
                   Fix.all.hwys()
                 else .x
               })

  cross.divs <-
    map2(divs, names(divs),
             ~{do.call(
               tracts.across.division,
               c(list(.x, region.ids,
                      ctsf = ctsf,
                      cutout.water = cutout.water),
                 params)) %>%
                 rename(
                   !!paste0(.y, ".poly") := poly.id)
               })

  ctdivm <-
    purrr::reduce(c(cross.divs, touching.divs),
                  full_join,  by = "geoid")

  return(ctdivm)
}



#' Wrapper_gen.tract.within.distance
#'
#'
#' @inheritParams get.region.identifiers
#' @inheritParams tracts.from.region
#' @param clean.nhpn Logical; whether or not to clean divs as if they are
#'   NHPN hwy data by applying cleaning fcns `denode.lines` and `Fix.all.hwys`. Can
#'   be a vector of length `divs` to clean for some measures.
#' @param ... passed onto `tracts.across.division` and/or `subset.polys.divs`
#'
#' @export Wrapper_gen.tract.within.distance
Wrapper_gen.tract.within.distance <- function(  cz = NULL,
                                             cbsa = NULL,
                                             divs, # NAMED list
                                             cutout.water = F,
                                             clean.nhpn = F,
                                             ...) {

  params <- list(...)

  region.ids <- get.region.identifiers(cz, cbsa)
  ctsf <- tracts.from.region(region.ids,
                             cutout.water = cutout.water,
                             year = 2019)

  ctsf <- ctsf %>% conic.transform()
  divs <- divs %>% map( conic.transform )

  # subset divs
  divs <- map(divs,
              ~do.call(
                subset.polys.divs,
                c(list(ctsf, .x), params)))

  # clean divs when appropriate.
  if(length(clean.nhpn) == 1)
    clean.nhpn <- rep(clean.nhpn, length(divs))
  divs <- map2(divs, clean.nhpn,
               ~{if(.y)
                 .x %>%
                   denode.lines() %>%
                   Fix.all.hwys()
                 else .x
               })

  within.dist2divs <-
    map2(divs, names(divs),
         ~{sbgp <- st_is_within_distance(
           ctsf,
           .x,
           500)
         tibble(
           geoid = ctsf[["geoid"]],
           !!paste0("within.500m.", .y) :=
             lengths(sbgp) > 0)
         })

  ctdivm <-
    purrr::reduce(within.dist2divs,
                  full_join,  by = "geoid")
  return(ctdivm)
}



# to delete(?) -----------------------------------------------------------------



#' tracts.touching.division
#'
#' Wraps st_intersects so that it can be mapped across czs easily.
#' --i think i delete this and just have a wrapper fcn that calls the others--
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



unused <- function(asdf) {  # xd: cross div associate each CT with which side of the divisions. They are across
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


