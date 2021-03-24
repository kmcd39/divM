# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

# devtools::load_all()
# source(here::here("R/Generate-measures/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# replace plc list from cz ray ws
plc <-
  readRDS(here::here("R/Generate-measures/CBSAs/rays/largest.plc.in.cbsa.rds"))

plc <- st_sf(plc) %>% divM::conic.transform()
plc$geoid <- plc$plc.id
plc$name <- plc$plc.name
# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

# Wrapper fcn for slurm ---------------------------------------------------

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
  f = slurm.ray.wrapper_cbsas,
  params = interstate.ray.params1,
  jobname = "cbsa-rays-interstates-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
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
    save.name = "cbsa-rays-limitedaccess-v1"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper_cbsas,
  params = lac.ray.params1,
  jobname = "rays-LACs-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
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
    save.name = "cbsa-rays-interstates-and-intersecting-v1"
  )


library(rslurm)

job <- rslurm::slurm_apply(
  f = slurm.ray.wrapper_cbsas,
  params = int.intersecting.params1,
  jobname = "int.intersecting.params1-v1",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "1:00:00",
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


