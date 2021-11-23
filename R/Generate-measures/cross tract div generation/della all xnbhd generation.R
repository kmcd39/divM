# ws -------------------------------------------------------------------------

rm(list = ls())
require(tidyverse)
require(sf)

# option setting
sf_use_s2(T)
options(tigris_use_cache = TRUE)

# dropbox dir or della base dir
ddir <- # Sys.getenv('drop_dir')
  '/scratch/gpfs/km31/'

devtools::load_all()


# della fcn --------------------------------------------------------------------


#' Wrapper_gen.tract.div.measures
#'
#'
#' @param rsf sf object representing region to get dividedness within. Expects `rt`
#'   and `rid` columns for region type/id
#' @param save.dir if not NULL, directory to save output to as .csv
#' @inheritDotParams gen.cross.tract.dividedness
#'
Wrapper_gen.tract.div.measures <- function( rsf
                                            , save.dir = NULL
                                            , ...) {

  require(tidyverse)
  require(sf)

  require(divM)

  # option setting
  sf_use_s2(T)
  options(tigris_use_cache = TRUE)


  params <- list(...)

  .rt <- rsf$rt
  .rid <- rsf$rid

  # start out

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

  # get x-tract measure
  cross.divs <-
    map2(divs, names(divs),
         ~{cat("\ndivision:",.y, "\n")
           do.call(
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


# czs --------------------------------------------------------------------------


