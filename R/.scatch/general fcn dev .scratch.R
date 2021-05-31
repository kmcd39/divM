library(tidyverse)
library(sf)

devtools::load_all()

tmpr <- divM::get.region.identifiers(cz = "26002")

sfg.seg::geo.subset.cbgs


get.water.for.region <- function(cz_name = NULL, cz = NULL,
                                 countyfp = NULL, cbsa_id = NULL,
                                 size.min = 5e6) {

  requireNamespace("xwalks")
  co2cz <- xwalks::co2cz
  if (is.null(c(cz_name, cz, countyfp, cbsa_id, plc_id)))
    stop("No subset parameter provided -- supply a cz/county to subset to.")
  if (!is.null(cz_name))
    .countyfp <- co2cz[co2cz$cz_name %in% cz_name, ]$countyfp
  else if (!is.null(cz))
    .countyfp <- co2cz[co2cz$cz %in% cz, ]$countyfp
  else if (!is.null(cbsa_id))
    .countyfp <- xwalks::co2cbsa %>% filter(cbsa == cbsa_id) %>%
    pull(countyfp)

  # download water
  water <- map_dfr(.countyfp,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5))
  )

  # union and explode water
  water <- st_union(water) %>% st_cast("POLYGON") %>% st_sf()

  # filter by size of union'd body
  water <- water %>% filter(as.numeric(st_area(.$geometry)) > size.min )
  water <- water %>% st_transform(st_crs(x))

  return(water)
}
