# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

# devtools::load_all()
# source(here::here("R/Generate measures/setup ray ws.R"))
load(here::here("R/Generate measures/ray-ws.Rdata"))

# replace plc list from cz ray ws
plc <-
  readRDS(here::here("R/Generate measures/CBSAs/rays/largest.plc.in.cbsa.rds"))

plc <- st_sf(plc) %>% divM::conic.transform()
plc$geoid <- plc$plc.id
plc$name <- plc$plc.name
# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

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

  print(place.geoid)
  # browser()

  #load("~/all/divM/R/Generate measures/rays/ray ws.Rdata")

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



divM::Count.rays(
  hwy.sf = hwys,
  place.sf = plc,
  place.geoid = plc$geoid[2],
  always.include = c("I", "U", "S"),
  include.map = T
)


#list.files("/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/")

slurm.ray.wrapper(plc$geoid[1],
                  save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
                  save.name = "ray test")

# to slurm ----------------------------------------------------------------

# ?Count.rays
interstate.ray.params1 <-
  tibble(
    # to iterate thru
    place.geoid = plc$geoid,
    # spatial clean params
    trim2LAC = FALSE,
    remove.holes = TRUE,
    minimum.segment.length = 10,
    minimum.hwy.length = 1000,
    fill.gaps = T,
    # hwy-inclusion params
    always.include = c("I"), # hwy types to include (interstates)
    include.intersecting = FALSE,
    hwy.types = NULL, # (intersecting types)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
    save.name = "Rays-interstates-v1"
  )

library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper,
  params = interstate.ray.params1,
  jobname = "rays-interstates-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "2:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),
  add_objects = c("czs", "hwys", "plc")
)

# -------------------------------------------------------------------------

# limited-access (approx) -------------------------------------------------
# to slurm

lac.ray.params1 <-
  tibble(
    # to iterate thru
    place.geoid = plc$geoid,
    # spatial clean params
    trim2LAC = TRUE,
    remove.holes = TRUE,
    minimum.segment.length = 10,
    minimum.hwy.length = 1000,
    fill.gaps = T,
    # hwy-inclusion params
    always.include = NULL, # pre-filtered to LACs
    include.intersecting = FALSE,
    hwy.types = NULL, # (intersecting types)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
    save.name = "Rays-limitedaccess-v1"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper,
  params = lac.ray.params1,
  jobname = "rays-LACs-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "2:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),

  add_objects = c("czs", "hwys", "plc")
)


# interstates + intersecting ----------------------------------------------

int.intersecting.params1 <-
  tibble(
    # to iterate thru
    place.geoid = plc$geoid,
    # spatial clean params
    trim2LAC = FALSE,
    remove.holes = TRUE,
    minimum.segment.length = 10,
    minimum.hwy.length = 1000,
    fill.gaps = T,
    # hwy-inclusion params
    always.include = c("I"),
    include.intersecting = TRUE,
    hwy.types = NULL, # (intersecting types - NULL is all)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
    save.name = "rays-interstates-and-intersecting-v1"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper,
  params = int.intersecting.params1,
  jobname = "int.intersecting.params1-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "2:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),

  add_objects = c("czs", "hwys", "plc")
)



# did all generate? -------------------------------------------------------
'
save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/"
(fns <- list.files(save.dir, full.names = T))
cbsa.rays <-
  map(fns,
      vroom::vroom)

names(cbsa.rays) <- list.files(save.dir)

# which not in each .csv?
(remaining <- cbsa.rays %>%
  map( ~{plc %>%
      filter(! plc.id %in% .x$plc.geoid)
  })
  )



#LAC test
divM::Count.rays(place.geoid = "4248360",
                 hwys,
                 plc,
                 # spatial clean params
                 remove.holes = TRUE,
                 minimum.segment.length = 10,
                 minimum.hwy.length = 1000,
                 fill.gaps = T,
                 # hwy-inclusion params
                 always.include = NULL, # pre-filtered to LACs
                 include.intersecting = FALSE,
                 hwy.types = NULL, # (intersecting types)
                 drop.NA = T,
                 buffer.meters = 300,
                 # save params
                 include.map = T
                 )



# final interstates-only --------------------------------------------------

int.remaining <- remaining$`Rays-interstates-v1.csv`$plc.id


divM::Count.rays(place.geoid = int.remaining[1],
                 hwys,
                 plc,
                 # spatial clean params
                 trim2LAC = FALSE,
                 remove.holes = TRUE,
                 minimum.segment.length = 10,
                 minimum.hwy.length = 1000,
                 fill.gaps = T,
                 # hwy-inclusion params
                 always.include = c("I"), # hwy types to include (interstates)
                 include.intersecting = FALSE,
                 hwy.types = NULL, # (intersecting types)
                 drop.NA = T,
                 buffer.meters = 300
                 )


slurm.ray.wrapper(
  # to iterate thru
  place.geoid = int.remaining[1],
  # spatial clean params
  trim2LAC = FALSE,
  remove.holes = TRUE,
  minimum.segment.length = 10,
  minimum.hwy.length = 1000,
  fill.gaps = T,
  # hwy-inclusion params
  always.include = c("I"), # hwy types to include (interstates)
  include.intersecting = FALSE,
  hwy.types = NULL, # (intersecting types)
  drop.NA = T,
  buffer.meters = 300,
  # save params
  save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
  save.name = "Rays-interstates-v1"
)
'

save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/"
(fns <- list.files(save.dir, full.names = T))
cbsa.rays <-
  map(fns,
      vroom::vroom)

names(cbsa.rays) <- list.files(save.dir)

# which not in each .csv?
(remaining <- cbsa.rays %>%
    map( ~{plc %>%
        filter(! plc.id %in% .x$plc.geoid)
    })
)



# did i make duplicates? --------------------------------------------------

cbsa.rays$`rays-interstates-and-intersecting-v1.csv` %>% nrow()
cbsa.rays %>%
  map( nrow )

cbsa.rays %>%
  map( ~filter(., plc.geoid %in%
                 .$plc.geoid[duplicated(plc.geoid)]
               )
       )

cbsa.rays %>%
  map(distinct) %>%
  map(nrow)

plc$geoid %>% unique()

# remove duplicates
cbsa.rays <- cbsa.rays %>%
  map(distinct)


# add cbsa ids ------------------------------------------------------------

cbsa.rays <-
  cbsa.rays %>%
  map( ~mutate(., plc.geoid =
                 stringr::str_pad(
                   as.character(plc.geoid), 7,
                   "left", "0"))
  )

cbsa.rays <-
  cbsa.rays %>%
  map( ~left_join( . ,
                  tibble(plc)[ , c(1:2)],
                  by = c("plc.geoid" = "plc.id"))
       )


# rewrite
write.names = list.files(save.dir, full.names = T)

map2(cbsa.rays, write.names,
      ~write.csv(.x, .y,
                 row.names = F)
     )


