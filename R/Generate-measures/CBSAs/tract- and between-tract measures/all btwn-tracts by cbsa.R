#' (all except water)

# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

#devtools::load_all()
library(divM)

rm(list=ls())

# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# trim input datasets -----------------------------------------------------

# only generate metro areas (not micro)
# https://www2.census.gov/geo/pdfs/reference/LSADCodes.pdf
cbsas <- cbsas %>%
  filter(lsad %in% "M1") %>% select(-lsad)

ints <- nhpn %>%
  filter(SIGNT1 %in% "I")

# interstates and US routes
ints.and.us  <- nhpn %>%
  filter(SIGNT1 %in% c("I", "U")) %>%
  filter(!st_is_empty(.$geometry))


# test call ---------------------------------------------------------------
test.id <- cbsas$cbsa[1]


#' note: the sf/s2 updates seem to have messed up my spatial overlap fcn, in a
#' way that I don't understand (when I'm using the ellipsoid geometry s2 stuff).
#' I should change the get.spatial.overlap in xwalks so it can be more flexible
#' with the crs, because it still seems to work with planar library.
# devtools::load_all()
'
t <- Wrapper_gen.tract.div.measures(cbsa = test.id,
                               divs =
                                 list(int = ints
                                      ,ints.and.us = ints.and.us
                                      ),
                               year = 2019,
                               clean.nhpn = T
                               )
t
'
# wrangle divs ------------------------------------------------------------

# hwys
#hwys <- hwys %>% filter(!is.na(SIGNT1)) %>% denode.lines() %>% Fix.all.hwys()

# interstates only
#ints <- ints %>% denode.lines() %>% Fix.all.hwys()

# Places
state_list <- xwalks::ctx %>% pull(statefp) %>% unique()
plcs <-
  map_dfr(state_list,
          tigris::places)
plcs  <-
  plcs %>%
  conic.transform() %>%
  st_boundary()

# counties
counties <-
  counties %>%
  conic.transform() %>%
  st_boundary()

# school districts
sds <-
  map_dfr(state_list,
          tigris::school_districts)

sds  <-
  sds %>%
  conic.transform() %>%
  st_boundary()


sds <- sds %>%
  select(school.dist = GEOID)

plcs <- plcs %>%
  select(plc.id = GEOID)

counties <- counties %>%
  select(countyfp = GEOID)



# function to gen measures and write --------------------------------------

# hardcoded parameters and list of many division types to generate measures for.
# note slurm fcns don't deal w/ ... well -- will
# have to just add explicit arguments to pass on if i
# want variations, although the wrapper fcn handles a lot of options
gen_and_save <- function(cbsa,
                         save.dir,
                         cutout.water = F) {
  require(sf)
  require(tidyverse)
  require(divM)

  safe_call <- possibly(Wrapper_gen.tract.div.measures,
                        otherwise = NA,
                        quiet = F)

  divs <-
    list( int = ints
         ,ints.and.us = ints.and.us
         ,county = counties
         ,plc = plcs
         ,school.dist = sds)

  clean.nhpn <- c(T,T,F,F,F)

  divs <- map(divs,
              ~st_set_crs(.x, "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"))

  ctdivm <-
    safe_call(cbsa = cbsa,
              divs = divs,
              clean.nhpn = clean.nhpn,
              cutout.water = cutout.water,
              year = 2019)

  if(is.na(ctdivm)) {
    message(paste0("error at region", cbsa, "\n"))
    return(-1)
  }

  # create save directory if it doesn't exist
  if(!file.exists(save.dir))
    dir.create(save.dir, recursive = T)

  save.name <-
    paste0("cbsa-", as.character(cbsa), ".csv")

  save.path <- paste0(save.dir,
                      save.name)

  # write
  write.csv(ctdivm,
            save.path,
            row.names = F)

  return(ctdivm)
}


#cbsas

# seems to fail where there is no division of type..


# generate via slurm ------------------------------------------------------
'
library(rslurm)
btwn.ct.param <-
  tibble(
    cbsa = cbsas$cbsa,
    save.dir = "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/",
    cutout.water = F
  )


job <-
  rslurm::slurm_apply(
  f =
    gen_and_save,
  params = btwn.ct.param,
  jobname = "btwn-tract-divm-cts_bycbsa",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "12:00:00",
                       "mem-per-cpu" = "24G",
                       "mail-type" = list("begin", "end", "fail"),
                       "mail-user" = "km31@princeton.edu"),
  add_objects = c("cbsas",
                  "ints", "ints.and.us", "plcs",
                  "counties", "sds"
                  )
)

job
'

# loading della rds -------------------------------------------------------

fns <- list.files("_rslurm_btwntractdivmcts_bycbsa/",
                  pattern = "results.*RDS", full.names = T)
#out <- readRDS(fns[1])

# check generation ---------------------------------------------

#sdir <- "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/water-trimmed/"
sdir <- "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/"

gend <- list.files(sdir, #full.names = T,
           pattern = ".csv$")
gend
length(gend)
nrow(cbsas)

genid <- gend %>%
  stringr::str_extract("[0-9]+")

ungend <-
  cbsas[!cbsas$cbsa %in% genid,]
ungend

# regen those after bug fix
library(rslurm)
btwn.ct.param <-
  tibble(
    cbsa = ungend$cbsa,
    save.dir = "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/",
    cutout.water = F
  )


job <-
  rslurm::slurm_apply(
    f =
      gen_and_save,
    params = btwn.ct.param,
    jobname = "btwn-tract-divm-cts_bycbsa2",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "12:00:00",
                         "mem-per-cpu" = "24G",
                         'mail-type' = list('begin', 'end', 'fail'),
                         'mail-user' = 'km31@princeton.edu'),
    add_objects = c("cbsas",
                    "ints", "ints.and.us", "plcs",
                    "counties", "sds"
    )
  )

job



# troublshooting ----------------------------------------------------------


# I think these are place missing interstates. yes, the fix hwys was throwing an
# error (instead of giving 0 poly divs) when no divs of type intersected.
tmpr <- ungend[1,] %>%tibble() %>%  geoseg::region.reorg("cbsa")
tmpr <- tmpr %>% rename(region.name = cbsa_name) %>% st_sf()
tmp <- ints %>%
  st_crop(tmpr)

polygonal.div(tmpr,
              tmp, return.sf = T)
