# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

# devtools::load_all()
# source(here::here("R/Generate measures/rays/setup ray ws.R"))
rm(list = ls())
load(here::here("R/Generate measures/rays/ray ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"


# int eligible places -------------------------------------------------------------

sbgp <- st_intersects(plc, filter(hwys, SIGNT1 == "I"))
int.eligible <- plc$geoid[lengths(sbgp) > 0]
int.eligible <- plc.ids[plc.ids %in% int.eligible] # rejoin to names


# test run ---------------------------------------------------------------------
'
Count.rays(plc.ids["Providence"], #RI
           hwy.sf = hwys,
           place.sf = plc,
           remove.holes = TRUE,
           minimum.segment.length = 10,
           minimum.hwy.length = 1000,
           fill.gaps = T,
           always.include = c("I"), # hwy types to include (interstates)
           #always.include = c("I", "U", "S"),
           include.intersecting = FALSE,
           hwy.types = NULL, # (intersecting types)
           drop.NA = T,
           buffer.meters = 300,
           return.map = TRUE
)
'
# more "raw" draw
'
pvd <- plc[plc$geoid == plc.ids["Providence"], ]
hwys %>%
  st_crop(st_bbox(pvd)) %>%
  st_collection_extract("LINESTRING") %>% st_cast("LINESTRING") %>%
  mapview::mapview(zcol = "SIGN1") + mapview::mapview(st_boundary(pvd), color = "#800000")
'


# Wrapper fcn for slurm ---------------------------------------------------
# combines Count.rays call with save.csv instructions and more slurm-friendly
# parameters.
slurm.ray.wrapper <- function(
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

  # browser()

  #load("~/all/divM/R/Generate measures/rays/ray ws.Rdata")
  #load("/scratch/gpfs/km31/ray ws.Rdata")

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

# call
slurm.ray.wrapper(int.eligible[2],
                  save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/",
                  save.name = "ray test")


# interstates -------------------------------------------------------------

#plc <- plc %>% filter(STATEFP  == 42) # (for test state)

# count rays with named paramters instead of ..., for rslurm compatibility(?)
#Count.rays.Wrapper

# ?Count.rays
test.ray.params1 <-
  tibble(
    place.geoid = int.eligible[3:4], # a cpl for test
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
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/",
    save.name = "rays-test"
  )

library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper,
  params = test.ray.params1,
  jobname = "Interstate_ray_test",
  nodes = 2,
  cpus_per_node = 1,
  slurm_options = list(time = "0:10:00",
                       "mem-per-cpu" = "3G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),

  add_objects = c("czs", "hwys", "plc")
)



# -------------------------------------------------------------------------


# count rays --------------------------------------------------------------




# limited-access (approx) -------------------------------------------------

?divM::lac_codes

sbgp <- st_intersects(plc, lac)
lac.eligible <- plc$geoid[lengths(sbgp) > 0]
lac.eligible <- plc.ids[plc.ids %in% lac.eligible]

# the NULL collection extract
plc.ids %in% int.eligible %>% head()


Count.rays(plc.ids[2],
           hwy.sf = hwys,
           place.sf = plc,
           remove.holes = TRUE,
           minimum.segment.length = 10,
           minimum.hwy.length = 1000,
           fill.gaps = T,
           always.include = c("I"), # hwy types to include (interstates)
           #always.include = c("I", "U", "S"),
           include.intersecting = FALSE,
           hwy.types = NULL, # (intersecting types)
           drop.NA = T,
           buffer.meters = 300,
           return.map = TRUE
)

