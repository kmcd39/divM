


#' gen.cross.tract.dividedness
#'
#' Given a large region: Place, CZ, CBSA, etc., and a division object, represented by
#' an `sf` object with only linear features, allocate each neighborhood, defined as
#' tracts or block groups, to a side of the division within the area.
#'
#'
#' @inheritParams gen.cross.tract.dividedness
#' @param nbd.query.fcn i.e., tracts or block_groups; passed onto
#'   `geox::tracts.from.sf`
#' @param region.id.colm identifer column for supplied region
#' @param cutout.water wheter or not to cut out water areas before getting
#'   dividedness.
#' @inheritDotParams polygonal.div
#'
#' @return a data.frame with one row per census tract in the supplied region, with a
#'   geoid column and a poly.id column indicating which "polygon division" each tract
#'   is in. Tracts with different identifiers in this column are across a division
#'   from one another.
#'
#' @export gen.cross.tract.dividedness
gen.cross.tract.dividedness <- function(region
                                        ,divs
                                        ,nbd.query.fcn = tigris::tracts
                                        ,region.id.colm = 'rid'
                                        ,fill.nhpn.gaps = F
                                        ,erase.water = F
                                        ,year = 2019
                                        ,...) {


  require(sf)
  requireNamespace('geox')

  # browser()

  # use div crs
  region <- region %>% st_transform(st_crs(divs))

  # crop divs to region bounding box
  divs <- st_crop(divs, region)

  # trim "slivers", point geometries that might be formed by cropping
  divs <- divs %>%
    filter(st_geometry_type(.) %in%
             c("LINESTRING", "MULTILINESTRING"))

  # fill NHPN gaps if asked to
  if(fill.nhpn.gaps)
    divs <- Fix.all.hwys(divs)

  # query nbhds; use bbox and then trim based on spatial overlap below
  nbhds <- geox::nbhds.from.sf(
    x = st_bbox(region)
    ,query.fcn = nbd.query.fcn
    ,year = year
  ) %>%
    st_transform(st_crs(divs))

  # remove only-water nbhoods
  nbhds <- nbhds %>%
    filter(substr(tractce
                  ,1,2) != '99')

  # validate before overlap is good.
  nbhds <- divM::absolute.validate(nbhds)

  # browser()

  # trim to region (important for places)
  nbhds <- xwalks::get.spatial.overlap(
    nbhds, region
    ,'geoid', region.id.colm
    , filter.threshold = 0.10 # >=10% overlap to be included with region
  )

  if(erase.water) {
    # not fully tested
    water <- geox::water.wrapper(x = st_bbox(region)
                                 ,year = year)
    water <- st_transform(water, st_crs(nbhds))
    nbhds <- st_difference(nbhds, st_union(water))
    region$geometry <- st_union(nbhds)
  }


  # crop divs:
  divs <- divs %>% filter(!st_is_empty(geometry))
  divs <- divs %>% st_crop(st_bbox(region))

  # div subpolys
  subpolys <- polygonal.div( region
                             ,divs
                             ,return.sf = T)

  require(lwgeom)
  subpolys <- subpolys %>% st_set_precision(1e5) %>% st_make_valid()

  # merge and allocate nbhoods:
  xnhood.div <- xwalks::get.spatial.overlap(nbhds
                                            ,subpolys
                                            ,'geoid'
                                            ,'poly.id')

  xnhood.div <- xnhood.div %>%
    group_by(geoid) %>%
    filter(perc.area ==
             max(perc.area))

  # return tibble
  xnhood.div <- xnhood.div %>%
    ungroup() %>%
    tibble() %>%
    select(-geometry) %>%
    rename(perc.in.div = perc.area) %>%
    mutate(poly.id =
             factor(poly.id))

  return(xnhood.div)
}

#' tracts.across.water
#'
#' Water divisions are areas, rather than
#'
#'
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


# to delete(?) -----------------------------------------------------------------



#' tracts.touching.division
#'
#' Wraps st_intersects so that it can be mapped across czs easily.
#' --i think i delete this and just have a wrapper fcn that calls the others--
#'
#' @inheritParams gen.cross.tract.dividedness
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




# wrapper flow:
#
# -start with cz/cbsa id -region call -> get tracts
#
# -call tracts.across.division or st_intersects or st_is_within_distance with list
# of divisions
#
# - cbinds and write/return.



#' Wrapper_gen.tract.within.distance
#'
#'
#'
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
