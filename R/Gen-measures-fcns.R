
# slurm.ray.wrapper_cbsas -------------------------------------------------

#' slurm.ray.wrapper_cbsas
#'
#' combines Count.rays call with save.csv instructions and more slurm-friendly
#' parameters.
slurm.ray.wrapper_cbsas <- function(
  place.geoid,
  trim2LAC = FALSE,

  remove.holes = TRUE,
  minimum.segment.length = 10,
  minimum.hwy.length = 1000,
  fill.gaps = T,
  always.include = c("I"), # hwy types to include (interstates)
  include.intersecting = FALSE,
  hwy.types = NULL, # (intersecting types)
  drop.NA = T,
  buffer.meters = 300,
  save.dir,
  save.name
) {

  require(tidyverse)
  require(sf)
  require(lwgeom)
  require(divM)

  print(place.geoid)
  # browser()

  #load("~/all/divM/R/Generate-measures/rays/ray ws.Rdata")

  # refresh crs(sometimes gets unbundled from object when moving across systems)
  st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
  st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

  # trim to limited-access if appropriate
  if(trim2LAC)
    hwys <- hwys %>% filter(FCLASS %in% divM::lac_codes |
                              SIGNT1 == "I")

  # pass on all the ridiculous # of parameters.. ----------------------------
  rays <- Count.rays(
    hwy.sf = hwys,
    place.sf = plc,
    place.geoid = place.geoid,
    remove.holes = remove.holes,
    minimum.segment.length = minimum.segment.length,
    minimum.hwy.length = minimum.hwy.length,
    fill.gaps = fill.gaps,
    always.include = always.include,
    include.intersecting = include.intersecting,
    hwy.types = hwy.types, # (intersecting types)
    drop.NA = drop.NA,
    buffer.meters = buffer.meters,
    include.map = F
  )

  # as table --------------------------------------------------------
  rays <- tibble(  name = plc[plc$geoid == place.geoid,]$name
                   ,plc.geoid = place.geoid
                   ,cbsa.id = plc[plc$geoid == place.geoid,]$cbsa.id
                   ,rays = rays$n.rays
  )

  save.path <- paste0(save.dir,
                      save.name,
                      ".csv")

  # write (append to running list of measures)
  write.table(rays,
              save.path,
              sep = ",",
              col.names =
                !file.exists(save.path),
              append = T,
              row.names = F)

  return(rays)
}



# -------------------------------------------------------------------------



# slurm.ray.wrapper_czs ---------------------------------------------------

#' slurm.ray.wrapper_czs
#' combines Count.rays call with save.csv instructions and more slurm-friendly
#' # parameters.
slurm.ray.wrapper_czs <- function(
  place.geoid,
  trim2LAC = FALSE,

  remove.holes = TRUE,
  minimum.segment.length = 10,
  minimum.hwy.length = 1000,
  fill.gaps = T,
  always.include = "I", # hwy types to include (interstates default)
  include.intersecting = FALSE,
  hwy.types = NULL, # (intersecting types)
  drop.NA = T,
  buffer.meters = 300,
  save.dir,
  save.name
) {

  require(tidyverse)
  require(sf)
  require(lwgeom)
  require(divM)

  print(place.geoid)
  # browser()

  #load("~/all/divM/R/Generate-measures/rays/ray ws.Rdata")

  # refresh crs(sometimes gets unbundled from object when moving across systems)
  st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
  st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

  # trim to limited-access if appropriate
  if(trim2LAC)
    hwys <- hwys %>%
    filter(FCLASS %in%
             divM::lac_codes | SIGNT1 == "I")

  # pass on all the ridiculous # of parameters.. ----------------------------
  rays <- Count.rays(
    hwy.sf = hwys,
    place.sf = plc,
    place.geoid = place.geoid,
    remove.holes = remove.holes,
    minimum.segment.length = minimum.segment.length,
    minimum.hwy.length = minimum.hwy.length,
    fill.gaps = fill.gaps,
    always.include = always.include,
    include.intersecting = include.intersecting,
    hwy.types = hwy.types, # (intersecting types)
    drop.NA = drop.NA,
    buffer.meters = buffer.meters,
    include.map = F
  )

  # as table --------------------------------------------------------
  rays <- tibble(  name = plc[plc$geoid == place.geoid,]$name
                   ,plc.geoid = place.geoid
                   ,rays = rays$n.rays
  )

  # write (append to running list of measures)
  save.path <- paste0(save.dir,
                      save.name,
                      ".csv")

  write.table(rays,
              save.path,
              sep = ",",
              col.names =
                !file.exists(save.path),
              append = T,
              row.names = F)

  return(rays)
}
