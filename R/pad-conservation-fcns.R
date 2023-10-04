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


#' Wrapper_pad.area.by.nbhd
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
Wrapper_pad.area.by.nbhd <- function(
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

  # get tracts for given state
  fcts <- geo.query(year = geo.yr
                    ,state = statefp) %>%
    rename_with(tolower) %>%
    select(1:4, aland, awater, geometry) %>%
    st_transform(5070)

  # get bbox and wkt filter for that area
  wktf <- fcts %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

  # load raw USGS PAD data.
  gdb <- pad.dir %>% list.files(pattern = 'gdb$', full.names = T)

  lyrs <- gdb %>% st_layers()

  # load spatial data
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
