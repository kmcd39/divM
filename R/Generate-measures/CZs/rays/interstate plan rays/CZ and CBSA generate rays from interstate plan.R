# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
# rm(list=ls())
devtools::load_all()
# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"


# get cleaned interstate hwy plan ---------------------------------------------------

plan.dir <- "/scratch/gpfs/km31/other-local-data/1947plan/most addl cleans/"
hwyplan <- list.files(plan.dir, pattern = "\\.shp", full.names = T)

#hwyplan = st_read("~/R/shapefiles/1947plan/addl cleans 2/cleaner-hwy-plan.shp")
hwyplan <- st_read(hwyplan)
# mimic structure of NHPN built hwys using plan data
# makes using the same functions easier.
hwyplan$SIGNT1 = "plan"
hwyplan$SIGN1 = paste0(hwyplan$SIGNT1,hwyplan$id)

# spatial clean hwy data --------------------------------------------------
# FIX NOLA & houston (check)
'library(mapedit)
library(leaflet)
mapview(hwyplan)
hpc <- hwyplan %>% filter(id != 161)
hpc %>% mapview()
hpc2 <- mapedit::editFeatures(hpc)
hpc2 %>% mapview()
intpl <- st_transform(hwyplan
                      ,st_crs(plc))

library(lwgeom)
sum(!st_is_valid(intpl))
intpl <- st_set_precision(intpl, 1000)
intpl <- st_make_valid(intpl)
st_write(hpc2, "~/R/shapefiles/1947plan/addl cleans 2/cleaner-hwy-plan.shp")'

# associate with place geoids ---------------------------------------------
intpl <- st_transform(hwyplan
                      ,st_crs(plc))

# possibility of a buffer to handle noise for low-reso hwy plan better?
#plc <- st_buffer(plc, 8046.72)

# mapview(plc)
# generate -----------------------------------------------------------
# setup
#plc <- plc %>% filter(STATEFP  == 36) # (for test state)
plc.ids <- plc$geoid
names(plc.ids) <- plc$plc.name

# test case: austin, TX; planned vs. built
'Count.rays(
  plc.ids[grepl("Austin.*Tex", names(plc.ids))]
           ,intpl
           ,plc
           ,always.include = "plan"
           ,min.segment.length = 10
           ,include.map = T
           )

Count.rays(
  plc.ids[grepl("Austin.*Tex", names(plc.ids))]
  ,hwys
  ,plc
  ,always.include = "I"
  ,min.segment.length = 10
  ,include.map = T
)
'

# send to slurm -----------------------------------------------------------

# replace hwys object with plan hwys (instead of updated hardcodedness of
# wrapper fcns)
hwys <- intpl

# czs ---------------------------------------------------------------------

# generation fcns:
slurm.ray.wrapper_czs

cz.plan.params <-
  tibble(
    # to iterate thru
    place.geoid = plc$geoid,
    # spatial clean params
    trim2LAC = F,
    remove.holes = TRUE,
    minimum.segment.length = 10,
    minimum.hwy.length = 1000,
    fill.gaps = T,
    # hwy-inclusion params
    always.include = "plan",
    include.intersecting = FALSE,
    hwy.types = NULL, # (intersecting types)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CZs/rays/",
    save.name = "czs-1947-plan-rays"
  )

# hwy sf
library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper_czs,
  params = cz.plan.params,
  jobname = "cz-plan-rays",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),

  add_objects = c("czs", "hwys", "plc")
)


# -------------------------------------------------------------------------

# CBSAs -------------------------------------------------------------------

# redefine plc
# replace plc list from cz ray ws
plc <-
  readRDS(here::here("R/Generate-measures/CBSAs/rays/largest.plc.in.cbsa.rds"))

plc <- st_sf(plc) %>% divM::conic.transform()
plc$geoid <- plc$plc.id
plc$name <- plc$plc.name
# refresh crs -------------------------------------------------------------
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

# generation fcns:
slurm.ray.wrapper_cbsas

cbsa.plan.params <-
  tibble(
    # to iterate thru
    place.geoid = plc$geoid,
    # spatial clean params
    trim2LAC = F,
    remove.holes = TRUE,
    minimum.segment.length = 10,
    minimum.hwy.length = 1000,
    fill.gaps = T,
    # hwy-inclusion params
    always.include = "plan",
    include.intersecting = FALSE,
    hwy.types = NULL, # (intersecting types)
    drop.NA = T,
    buffer.meters = 300,
    # save params
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/CBSAs/rays/",
    save.name = "cbsas-1947-plan-rays"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper_cbsas,
  params = cbsa.plan.params,
  jobname = "cbsa-plan-rays",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
                       "mem-per-cpu" = "5G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),

  add_objects = c("czs", "hwys", "plc")
)



