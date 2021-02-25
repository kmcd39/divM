# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

devtools::load_all()
# source(here::here("R/Generate measures/rays/setup ray ws.R"))
load(here::here("R/Generate measures/rays/ray ws.Rdata"))



# refresh crs -------------------------------------------------------------

# sometimes gets unbundled from object when moving across systems
st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

# test run ---------------------------------------------------------------------
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
  place.geoid = int.eligible[1:2], #2 for test run
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

  load("~/all/divM/R/Generate measures/rays/ray ws.Rdata")

  # refresh crs(sometimes gets unbundled from object when moving across systems)
  st_crs(hwys) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
  st_crs(plc) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"

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


slurm.ray.wrapper(int.eligible[1],
                  save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/",
                  save.name = "ray test")
# interstates -------------------------------------------------------------

#plc <- plc %>% filter(STATEFP  == 42) # (for test state)

# count rays with named paramters instead of ..., for rslurm compatibility(?)
#Count.rays.Wrapper

# ?Count.rays
interstate.ray.params1 <-
  tibble(
    place.geoid = int.eligible[1:2], #2 for test run
    hwy.sf = hwys,
    place.sf = plc,
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
  )

library(rslurm)

job <- rslurm::slurm_apply(
  f = Count.rays,
  params = interstate.ray.params1,
  jobname = "Interstate_ray_test",
  nodes = 2,
  cpus_per_node = 1,
  slurm_options = list(time = "0:10:00",
                       "mem-per-cpu" = "3G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu')
  )


# only interstates
rays <-
  furrr::future_imap( int.eligible[1:3],
                      ~ Count.rays(.,
                                   hwys, plc,
                                   ,min.segment.length = 10
                                   ,ray.node.distance.threshold= 100
                                   ,include.map = FALSE
                                   ,verbose = T)
                      ,.progress = TRUE)
names(rays)
saveRDS(rays, "outputs/natl-rays_interstates-only_v2.RDS")



# -------------------------------------------------------------------------


# eligible places -------------------------------------------------------------

sbgp <- st_intersects(plc, filter(hwys, SIGNT1 == "I"))
int.eligible <- plc$geoid[lengths(sbgp) > 0]
int.eligible <- plc.ids[plc.ids %in% int.eligible] # rejoin to names



# count rays --------------------------------------------------------------




# limited-access (approx) -------------------------------------------------

?divM::lac_codes

sbgp <- st_intersects(plc, lac)
lac.eligible <- plc$geoid[lengths(sbgp) > 0]
lac.eligible <- plc.ids[plc.ids %in% lac.eligible]


# generate rays -----------------------------------------------------------


# include intersecting
rays.variation <-
  furrr::future_imap( plc.ids,
                      ~ count.rays(., plc, hwys
                                   ,include.intersecting = T
                                   ,min.segment.length = 10
                                   ,ray.node.distance.threshold= 100
                                   ,include.map = FALSE
                                   ,verbose = T)
                      ,.progress = TRUE)
saveRDS(rays.variation, "outputs/natl-rays_interstates-and-intersections_v2.RDS")
#rays$Philadelphia
#rays.variation$Philadelphia

'rays[names(rays) == "Portland"]
plc %>% filter(NAME == "Portland")
rays.variation$Portland'

# compile csv -------------------------------------------------------------
tbl <- tibble( name = names(plc.ids)
               ,geoid = plc.ids
               ,rays_interstate_only = map_dbl(rays, ~`[[`(., "n.rays"))
               ,rays_include_intersecting = map_dbl(rays.variation, ~`[[`(., "n.rays")))

#map_dbl(PA.rays, ~`[[`(., "n.rays"))

tbl

tbl2 <- plc %>%
  tibble() %>%
  select(GEOID, NAME, cz, cz_name, place.pop = population, cz.pop) %>%
  #mutate(GEOID=as.numeric(GEOID)) %>%
  left_join(tbl,
            by=c("GEOID" = "geoid"))

tbl2 %>%
  select(-c(name)) %>%
  rename(place_name = NAME) %>%
  write.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/rays-interstates-only-1.csv")


