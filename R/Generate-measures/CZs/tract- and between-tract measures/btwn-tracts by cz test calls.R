# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
rm(list=ls())
library(divM)
# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

ints <- hwys %>%
  filter(SIGNT1 %in% "I")

counties
plc
# test call ---------------------------------------------------------------
czs
tmpr <- get.region.identifiers(cz = "00302")

Wrapper_gen.tract.div.measures(cz = "00302",
                               divs =
                                 list(int = ints,
                                      hwy = hwys),
                               year = 2019
                               )

# test call when no div passes thru the area

int.eligible <- st_intersects(
  czs, ints)
int.eligible <-
  czs[lengths(int.eligible) > 0, ]
czs[!czs$cz %in% int.eligible$cz, ]

# devtools::load_all()
Wrapper_gen.tract.div.measures(cz = "00402",
                               divs =
                                 list(int = ints,
                                      hwy = hwys),
                               year = 2019
                               )

# test call where ct geometries can become invalid  (topology error)
Wrapper_gen.tract.div.measures(cz = "10700",
                               divs =
                                 list(int = ints),
                               cutout.water = T,
                               year = 2019
                               )

devtools::load_all()

czs %>% filter(as.numeric(cz) > 10600)
tmpr <- get.region.identifiers(cz = "05201")
tmpct <- tracts.from.region(tmpr)
download.and.cutout.water(tmpct)

# wrangle divs ------------------------------------------------------------

# hwys
hwys

# interstates only
ints

# Places
state_list <- xwalks::co2cz %>% pull(statefp) %>% unique()
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
gen_and_save <- function(cz,
                         save.dir,
                         save.name) {
  require(sf)
  require(tidyverse)
  require(divM)

  safe_call <- possibly(Wrapper_gen.tract.div.measures,
                        otherwise = NA,
                        quiet = F)

  ctdivm <-
    safe_call(cz = cz,
              divs =
                list(int = ints
                     ,hwy = hwys
                     ,county = counties
                     ,plc = plcs
                     ,school.dist = sds
                ),
              cutout.water = T,
              year = 2019)


  if(is.na(ctdivm)) {
    message(paste0("error at cz", cz))
    return(-1)
  }

  # create save directory if it doesn't exist
  if(!file.exists(save.dir))
    dir.create(save.dir, recursive = T)

  save.name <-
    paste0("cz-", as.character(cz), ".csv")

  save.path <- paste0(save.dir,
                      save.name)

  # write (append to running list of measures)
  write.csv(ctdivm,
            save.path,
            row.names = F)

  return(ctdivm)
}

czs

'
gen_and_save(cz = "00302",
             save.dir = "tests/"
             )
'
# seems to fail where there is no division of type..


# generate via slurm ------------------------------------------------------

library(rslurm)
btwn.ct.param <-
  tibble(
    cz = czs$cz,
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/water-trimmed/"
  )


job <-
  rslurm::slurm_apply(
  f =
    gen_and_save,
  params = btwn.ct.param,
  jobname = "btwn-tract-divm-cts_watertrimmed",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "8:00:00",
                       "mem-per-cpu" = "12G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),
  add_objects = c("czs",
                  "ints", "hwys", "plcs",
                  "counties", "sds"
                  )
)

job


# check where didn't generate ---------------------------------------------

# looks like out-of-memory exception
sdir <- "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/water-trimmed/"

gend <- list.files(sdir, #full.names = T,
           pattern = ".csv$")
length(gend)
gend <- gend %>%
  stringr::str_extract("[0-9]+")

ungend <-
  czs[!czs$cz %in% gend,]

ungend %>% tibble() %>%
  filter(grepl("Los", cz_name))

tmpr <- get.region.identifiers(cz = "03300")

# re-send to slurm w/ more memory
library(rslurm)

btwn.ct.param <-
  tibble(
    cz = ungend$cz,
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/water-trimmed/"
  )

job <-
  rslurm::slurm_apply(
    f =
      gen_and_save,
    params = btwn.ct.param,
    jobname = "btwn-tract-divm-cts_watertrimmed2",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "8:00:00",
                         "mem-per-cpu" = "25G",
                         'mail-type' = list('begin', 'end', 'fail'),
                         'mail-user' = 'km31@princeton.edu'),
    add_objects = c("czs",
                    "ints", "hwys", "plcs",
                    "counties", "sds"
    )
  )

rslurm::get_job_status(job)


# addl test calls ---------------------------------------------------------

# some topology exceptions -- as in Santa Rosa, as well (w/ hwys and water
# trimmed only)
# adding "abs.validate parameter to handle these
czs[czs$cz == "37700",]
devtools::load_all()
Wrapper_gen.tract.div.measures(cz = "37700",
                               divs =
                                 list(hwy = hwys
                                      ),
                               cutout.water = T,
                               year = 2019,
                               clean.nhpn = T,
                               validate = T
                               )

