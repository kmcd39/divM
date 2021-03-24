# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
rm(list=ls())
devtools::load_all()
# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"


# test call ----------------------------------------------------------------

#list.files("/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/rays/")
slurm.ray.wrapper_czs(plc$geoid[1],
                  always.include = list("I", "U", "S"),
                  save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/rays/",
                  save.name = "ray test")


# send job ----------------------------------------------------------------


iuc.ray.params1 <-
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
    always.include =
      list(c("I", "U", "S"))
    , # pre-filtered to LACs
    include.intersecting = FALSE,
    hwy.types = NULL, # (intersecting types)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/rays/",
    save.name = "cz-ius-rays"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper_czs,
  params = iuc.ray.params1,
  jobname = "cz-ius-rays",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),
  add_objects = c("czs", "hwys", "plc")
)
