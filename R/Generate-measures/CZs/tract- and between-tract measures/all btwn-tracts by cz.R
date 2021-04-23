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

devtools::load_all()
Wrapper_gen.tract.div.measures(cz = "00402",
                               divs =
                                 list(int = ints,
                                      hwy = hwys),
                               year = 2019
                               )


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

  ctdivm <-
    divM::Wrapper_gen.tract.div.measures(cz = cz,
                                   divs =
                                     list(int = ints
                                          ,hwy = hwys
                                          ,county = counties
                                          ,plc = plcs
                                          ,school.dist = sds
                                     ),
                                   year = 2019)



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
             save.dir = "tests/",
             save.name = "ctdiv-test"
)
'
# seems to fail where there is no division of type..


# generate via slurm ------------------------------------------------------

library(rslurm)
btwn.ct.param <-
  tibble(
    cz = czs$cz,
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/",
    save.name = "ctdivm"
  )



job <-
  rslurm::slurm_apply(
  f = gen_and_save,
  params = btwn.ct.param,
  jobname = "btwn-tract-divm-cts",
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
