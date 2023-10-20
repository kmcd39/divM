


#' remove.water.from.nbhds
#'
#' Removes water from census tracts or block groups.
#'
#'
#' @param nbhds an `sf` objects representing tracts or blockgroups, with
#'   `countyfp` and `statefp` columns
#' @param year year for which to get water areas
#' @param water.size.floor passed onto `geox::trim.tigris.waters`
#'
remove.water.from.nbhds <- function(nbhds,
                                    year = NULL,
                                    water.size.floor = 1e5) {

  # retrieve water
  state.cofps <- tibble(nbhds) %>%
    select(statefp, countyfp) %>%
    distinct()

  wtr <-
    map2_dfr(state.cofps$statefp,
             state.cofps$countyfp,
            ~tigris::area_water( state = .x
                                 ,county = .y
                                 , year = year)
    ) %>%
    rename_with(tolower)

  wtr <- wtr %>%
    geox::trim.tigris.waters(
      size.floor = water.size.floor )

  # re-project & remove water
  wtr <- wtr %>%
    st_transform( st_crs(nbhds) )

  nbhds <- nbhds %>%
    st_difference( st_union(wtr) )

  return(nbhds)
}



#' read.PAD.over.area
#'
#' Reads PAD data over a given area or statefips
#'
#' @param sfx `sf` object over which to read PAD data. Leave NULL to read over a
#'   state.
#' @param statefips State over which to read data; only used if `sfx` is not
#'   supplied.
#' @param pad.dir directory containing PAD data
#' @param pad.layers Layers of PAD data to load; defaults to Fees and Easements.
#'
read.PAD.over.area <- function(
  sfx = NULL,
  statefips = NULL,
  pad.dir = '~/R/local-data/usgs/PADUS3_0Geodatabase/',
  pad.layers = c(11, 13)) {

  if( is.null(sfx) )
    sfx <- tigris::states() %>%
      rename_with(tolower) %>%
      filter(statefp  == statefips)

  # get bbox and wkt filter for area
  wktf <- sfx %>% st_transform(5070) %>%
    st_bbox() %>% st_as_sfc() %>% st_as_text()

  # find geodatabase/load list of layers
  gdb <- pad.dir %>% list.files(pattern = 'gdb$', full.names = T)
  lyrs <- gdb %>% st_layers()

  # read Pad fees & easements
  pad <-
    map(lyrs$name[c( 11, 13 )]
        , ~st_read(
          gdb
          , layer = .x
          , wkt_filter = wktf # over sample area
        )
    ) %>%
    set_names( lyrs$name[c( 11, 13 )] )
  # may be a warning/GDAL error reading..

  pad <- pad %>%
    map( tibble ) %>%
    map( ~rename_with(., tolower) ) %>%
    map( ~rename(., geometry = shape) )

  ## trim colms and rbind
  keep.cols <- Hmisc::Cs(featclass, mang_type,
                         loc_mang, loc_ds,
                         unit_nm,
                         src_date, date_est,
                         gis_acres,
                         pub_access, gap_sts,
                         des_tp, geometry )

  pad <- pad %>%
    map_dfr( ~select(  tibble(.x)
                       , all_of(keep.cols)) )

  pad <- pad %>%
    st_sf() %>%
    st_transform(5070) %>%
    st_cast('MULTIPOLYGON')

  # spatial filter to area as well -- the wktf filter when first reading in can
  # be very rough it seems
  pad <- pad %>%
    st_filter(sfx)

  return(pad)

}



# wrapper fcns ----------------------------------------------------------------



#' local.Wrapper_pad.area.by.nbhd
#'
#' Wraps all steps to generate measures for protected areas by type (and
#' overall) for a given state, by tract or block group. Local version queries
#' from tigris.
#'
#' @param statefips FIPS code for state to generate
#' @param pad.dir Directory of USGS PAD geodatabase. Default parameter for KM
#'   della
#' @param tigris.dir Directory with tigris geometries, if on Della w/o internet
#'   access (so can't just pull directly.) Should have either tracts or
#'   blockgroups for this function. Default parameter for block groups on KM
#'   della
#' @param remove.water Whether to remove water from tracts
#' @param water.size.floor Minimum square meters to use as a catch-all filter to
#'   keep water areas (can be 0)
#' @param simplify.geos T/F whether to run st_simplify on PAD data before
#'   measure generation.
#'
local.Wrapper_pad.area.by.nbhd <- function(
    statefips,
    pad.dir = '~/R/local-data/usgs/PADUS3_0Geodatabase/',
    nbhds.fcn = tigris::block_groups,
    remove.water = T,
    water.size.floor = 1e5,
    geo.precision =
      units::set_units(1, 'meters'),
    simplify.geos = F,
    sv.dir = '~/R/divM/genereated-measures/fall-2023-conservation-areas/') {

  require(tidyverse)
  require(sf)
  cat(paste0('generating for state ', statefips, '\n'))

  # get nbhds for given state
  cat('querying for tracts or block groups \n')

  nbhds <- nbhds.fcn( state = statefips
                     ,year = 2021 ) %>%
    rename_with(tolower) %>%
    st_transform(5070) %>%
    select( statefp, countyfp, geoid,
            aland, awater, geometry ) %>%
    filter( aland > 0 )

  # trim water if desired
  if(remove.water) {
    cat('removing water \n')
    nbhds <- remove.water.from.nbhds(nbhds
                                     ,year = 2021
                                     ,water.size.floor = 1e5)
  }

  # set precision and validate nbhds
  nbhds <- nbhds %>%
    st_set_precision(
      geo.precision) %>%
    st_make_valid()

  cat('loading USGS PAD data \n')

  pfe <- read.PAD.over.area(
    sfx = nbhds,
    pad.dir = '~/R/local-data/usgs/PADUS3_0Geodatabase/',
    pad.layers = c(11, 13)
  )

  ## spatial cleans
  cat('spatial cleaning PAD data... \n')

  browser()

  pfe <- pfe %>%
    st_set_precision(
      geo.precision ) %>%
    st_make_valid()

  if(simplify.geos)
    pfe <- pfe %>% st_simplify()

  ## union by relevant columns -- simplifying geometries by union'ing is
  ## necessary to not mess them up (get invalid geos) during the differenc'ing,
  ## it seems
  pfed <- pfe %>%
    group_by(featclass, mang_type, des_tp) %>%
    summarise(., do_union = T) %>%
    ungroup()

  # pfed %>% st_collection_extract('POLYGON') %>% tibble() %>% count(geotype = st_geometry_type(geometry))

  # handle slivers & re-validate
  pfed <- pfed %>%
    st_collection_extract('POLYGON') %>%
    mutate(calcd.area = st_area(geometry))

  # # (check slivers)
  # pfed %>% arrange(calcd.area) %>% head(100)
  # pfed$calcd.area %>% quantile(seq(0,1,.05))

  pfed <- pfed %>%
    filter(calcd.area >
             units::set_units(100, 'm^2') # seems a reasonable threshold
           )  %>%
    st_set_precision(
      geo.precision ) %>%
    st_make_valid()

  ## remove overlaps (this takes a while -- computationally intensive)
  pfed <- pfed %>%
    arrange(desc(featclass)) %>%
    st_difference()

  # # backup for developing
  # pfed_backup <- pfed

  # validate and remove post-difference slivers
  pfed <- pfed %>%
    st_collection_extract('POLYGON') %>%
    st_set_precision(
      geo.precision ) %>%
    st_make_valid()

  ## again union, dropping FeatureClass now, which won't be accurate because
  ## sometimes they will have been overlapping. Then another cleaning repetition
  ## (not sure necessary rn buut always nice to not have invalid geometry
  ## errors.)
  pfed <- pfed %>%
    group_by(mang_type, des_tp) %>%
    summarise(., do_union = T) %>%
    ungroup() %>%
    st_set_precision(
      geo.precision ) %>%
    st_make_valid() %>%
    mutate(id = 1:nrow(.)
           ,.before = everything())


  # tibble(pfed) %>% count(geo.type = st_geometry_type(geometry))

  # get overlap by nbhd, long by manager and designation type
  cat('generating measures \n')
  perc.by.nbhd <-
    geox::get.spatial.overlap(
      nbhds,
      pfed,
      'geoid',
      'id',
      filter.threshold = 0.005 # half a percent overlap.
    )

  # add full list of tracts
  perc.by.nbhd <- perc.by.nbhd %>%
    full_join(tibble(nbhds)[c('geoid', 'aland')])

  # make 0s explicit (replace NAs where there was no conserved area in
  # tract/nbhd), and add nbhd acreage
  perc.by.nbhd <- perc.by.nbhd %>%
    mutate(perc.area =
             if_else( is.na(perc.area),
                      0, perc.area)
           ,nbhd.land.acres =
             as.numeric(
               units::set_units(
                 units::set_units(aland, 'm^2')
                 ,'acres')
               )
           )

  # add des/mgnr type back and rearrange
  perc.by.nbhd <- perc.by.nbhd %>%
    left_join(tibble(pfed)[c('id', 'mang_type', 'des_tp' )]
              ,by = 'id') %>%
    select(-id) %>%
    select(geoid, mang_type, des_tp, nbhd.land.acres, perc.area) %>%
    mutate(conserved.acreage = nbhd.land.acres * perc.area)

  # save & return set of combined measures
  if( !is.null(sv.dir) )
    write.csv(perc.by.nbhd
              ,file = paste0(sv.dir, 'state-',statefips,'.csv')
              ,row.names = F)

  cat('finished \n')
  return(perc.by.nbhd)
}


#' della.Wrapper_pad.area.by.nbhd
#'
#' Wraps all steps to generate measures for protected areas by type (and
#' overall) for a given state, by tract or block group.
#'
#' @param statefips FIPS code for state to generate
#' @param pad.dir Directory of USGS PAD geodatabase. Default parameter for KM della
#' @param tigris.dir Directory with tigris geometries, if on Della w/o internet
#'   access (so can't just pull directly.) Should have either tracts or
#'   blockgroups for this function. Default parameter for block groups on KM della
#' @param remove.water Whether to remove water from tracts
#' @param water.size.floor Minimum square meters to use as a catch-all filter to
#'   keep water areas (can be 0)
#' @param simplify.geos T/F whether to run st_simplify on PAD data before
#'   measure generation.
#'
della.Wrapper_pad.area.by.nbhd <- function(
    statefips,
    pad.dir = '/scratch/gpfs/km31/protected-areas/usgs-data/PADUS3_0Geodatabase/',
    nbhd.dir = '/scratch/gpfs/km31/census-geos/bgs/',
    remove.water = T,
    water.size.floor = 1e5,
    wtr.dir = '/scratch/gpfs/km31/census-geos/water/',
    geo.precision = units::set_units(1, 'meters'),
    simplify.geos = F) {

  require(tidyverse)
  require(sf)
  cat(paste0('generating for state ', statefips, '\n'))

  # get nbhds for given state
  cat('querying for tracts or block groups \n')
  fn <- list.files(nbhd.dir, pattern = 'shp$')
  cat('using file ', nbhd.dir, fn)

  nbhds <- st_read( paste0(nbhd.dir, fn)) %>%
    rename_with(tolower) %>%
    filter(statefp == statefips) %>%
    st_transform(5070) %>%
    select(1:4, aland, awater, geometry)

  # remove only-water CTs
  nbhds <- nbhds %>%
    filter(aland > 0 )

  # trim water if desired (UGH this won't work on della w/o more setup)
  if(remove.water) {
    # retrieve water
    wtr <- st_read(
      paste0(
        '/scratch/gpfs/km31/census-geos/water/state-',
        statefips, '/water.shp'))

    wtr <- wtr %>%
      geox::trim.tigris.waters(
        size.floor = water.size.floor )

    wtr <- wtr %>%
      st_transform(5070)

    nbhds <- nbhds %>%
      st_difference( st_union( wtr ) ) %>%
      st_set_precision(
        geo.precision) %>%
      st_make_valid()
  }

  # get bbox and wkt filter for area
  wktf <- nbhds %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

  # load raw USGS PAD data.
  cat('loading USGS PAD data \n')
  gdb <- pad.dir %>% list.files(pattern = 'gdb$', full.names = T)
  lyrs <- gdb %>% st_layers()

  # read Pad fees & easements
  pfe <-
    map(lyrs$name[c( 11, 13 )]
        , ~st_read(
          gdb
          , layer = .x
          , wkt_filter = wktf # over sample area
        )
    ) %>%
    set_names( lyrs$name[c( 11, 13 )] )
  # may be a warning/GDAL error reading..

  pfe <- pfe %>%
    map( tibble ) %>%
    map( ~rename_with(., tolower) ) %>%
    map( ~rename(., geometry = shape) )

  ## trim colms and rbind
  keep.cols <- Hmisc::Cs(featclass, mang_type,
                         loc_mang, loc_ds,
                         unit_nm,
                         src_date, date_est,
                         gis_acres,
                         pub_access, gap_sts,
                         des_tp, geometry )

  pfe <- pfe %>%
    map_dfr( ~select(  tibble(.x)
                       , all_of(keep.cols)) )

  ## spatial cleans
  pfe <- pfe %>%
    st_sf() %>%
    st_transform(5070) %>%
    st_cast('MULTIPOLYGON') %>%
    st_set_precision(
      geo.precision ) %>%
    st_make_valid()

  if(simplify.geos)
    pfe <- pfe %>% st_simplify()

  ## removing overlaps
  pfed <- pfe %>%
    arrange(desc(featclass)) %>%
    st_difference()

  # union by relevant columns & add index/id
  pfed <- pfed %>%
    group_by(mang_type, des_tp) %>%
    summarise(., do_union = T) %>%
    ungroup() %>%
    mutate(id = 1:nrow(.)
           ,.before = everything())

  # get overlap by nbhd, long by manager and designation type
  cat('generating measures \n')
  perc.by.nbhd <-
    geox::get.spatial.overlap(
      fcts,
      pfed,
      'geoid',
      'id',
      filter.threshold = 0.005 # half a percent overlap.
    )

  # add full list of tracts and make 0s explicit
  perc.by.tract <- perc.by.tract %>%
    full_join(tibble(fcts)['geoid']) %>%
    mutate(perc.area =
             if_else( is.na(perc.area),
                      0, perc.area)
    )

  # add des/mgnr type back
  perc.by.tract <- perc.by.tract %>%
    left_join(tibble(pfed)[c('id', 'mang_type', 'des_tp' )]
              ,by = 'id') %>%
    select(-id)

  # return set of combined measures
  return(perc.by.tract)
}



# archived ----------------------------------------------------------------


#' geo.clean
#'
#' After repeated spatial manipulation, geometries can be corrupted. This wraps
#' steps i'm using to re-validate geometries as I go
#'
geo.clean <- function(sfx,
                      precision = units::set_units(1, 'm'),
                      geo.types = 'POLYGON') {

  sfx

}


# out of use, for first pass:


#' protected.area.by.type.by.tract
#'
#' @param pad data for protected areas
#' @param cts data for census tracts. Will assume geoid column
#' @param category.colm string for the category by which to get protected area
#'   purpose
#'
protected.area.by.type.by.tract <-
  function(pad,
           cts,
           category.colm = NULL) {

    require(tidyverse)
    require(sf)
    #browser()

    pad <- pad %>% st_sf() %>% st_transform(5070)
    cts <- cts %>% st_sf() %>% st_transform(5070)

    # group pad and union by the category
    if(!is.null(category.colm)) {

      gpad <- pad %>%
        group_by( !!rlang::sym(category.colm) ) %>%
        summarise(., do_union = T) %>%
        mutate(id = 1:nrow(.))

    } else if(is.null(category.colm)) {
      gpad <- pad %>%
        summarise(., do_union = T) %>%
        mutate(any.protected.area = 'any')

      category.colm <- 'any.protected.area'
    }

    out <-
      geox::get.spatial.overlap(cts,
                                gpad,
                                'geoid',
                                category.colm,
                                filter.threshold = 0.005 # half a percent overlap.
      )

    # add back to full list of CTs and make 0s explicit
    out <- cts %>%
      tibble() %>%
      select(geoid) %>%
      left_join(out) %>%
      mutate(perc.area =
               if_else( is.na(perc.area )
                        ,0 , perc.area ))

    return(out)
  }


#' Wrapper_pad.area.by.nbhd_archived
#'
#' Wraps all steps to generate measures for protected areas by type (and
#' overall) for a given state, by tract or block group.
#'
#' @param statefp FIPS code for state to generate
#' @param pad.dir Directory of USGS PAD geodatabase.
#' @param category.colms Column names in PAD data by which we want protected
#'   area measures disaggregated.
#' @param geo.query `tigris` function to get tracts of block groups.
#' @param geo.yr Year of geographies we want.
#' @param simplify.geos T/F whether to run st_simplify on PAD data before
#'   measure generation.
#'
Wrapper_pad.area.by.nbhd_archived <- function(
    statefp,
    pad.dir,
    category.colms = c( 'featclass'
                        ,'own_type', 'own_name'
                        ,'mang_type', 'mang_name'
                        ,'des_tp'
                        ,'gap_sts'),
    geo.query = tigris::tracts,
    geo.yr = 2021,
    simplify.geos = F
) {

  require(tidyverse)
  require(sf)
  cat(paste0('generating for state ', statefp, '\n'))

  # get tracts for given state
  cat('querying for tracts or block groups \n')
  fcts <- geo.query(year = geo.yr
                    ,state = statefp) %>%
    rename_with(tolower) %>%
    select(1:4, aland, awater, geometry) %>%
    st_transform(5070)

  # get bbox and wkt filter for that area
  wktf <- fcts %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

  # load raw USGS PAD data.
  cat('loading USGS PAD data \n')
  gdb <- pad.dir %>% list.files(pattern = 'gdb$', full.names = T)
  lyrs <- gdb %>% st_layers()

  pad <- st_read(
    gdb
    , layer = lyrs$name[9]
    , wkt_filter = wktf # over sample area
  )
  # may be a warning/GDAL error reading..

  # cast to polygons (get rid of multisurface), then turn to tibble for
  # non-spatial processing
  pad <- pad %>%
    rename(geometry = SHAPE) %>%
    rename_with(tolower) %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(5070) %>%
    st_make_valid()

  if(simplify.geos)
    pad <- pad %>% st_simplify()

  # run measure generation function over each category
  cat('generating measures \n')
  combined <- category.colms %>%
    map(
      ~protected.area.by.type.by.tract(
        pad, fcts, .x )
    ) %>%
    set_names(category.colms)

  # remember to add %area for "no type"
  any.protected.area <-
    protected.area.by.type.by.tract(
      pad, fcts,
      category.colm = NULL )

  combined <- c(combined,
                'any.protected.area' = list(any.protected.area))

  # then bind to long dataframe.
  combined <- combined %>%
    imap_dfr(
      ~mutate(.x,
              protected.area.descriptor =  .y
              ,.after = geoid) %>%
        rename( protected.area.value = !!.y
                ,tract.perc.area = perc.area)
    )

  # return set of combined measures
  return(combined)
}
