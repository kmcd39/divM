# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
rm(list=ls())
library(divM)
# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

ints <- nhpn %>%
  filter(SIGNT1 %in% "I")

# interstates and US routes
ints.and.us  <- nhpn %>%
  filter(SIGNT1 %in% c("I", "U"))

counties
cbsas


# test call ---------------------------------------------------------------
cbsas
tmpr <- get.region.identifiers(cbsa = "12060")
tmpr
#  devtools::load_all()
t <- Wrapper_gen.tract.div.measures(cbsa = "12060",
                               divs =
                                 list(int = ints
                                      #,hwy = hwys
                                      ),
                               year = 2019,
                               clean.nhpn = T
                               )
t
# wrangle divs ------------------------------------------------------------

# hwys
#hwys <- hwys %>% filter(!is.na(SIGNT1)) %>% denode.lines() %>% Fix.all.hwys()

# interstates only
#ints <- ints %>% denode.lines() %>% Fix.all.hwys()

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
                         cutout.water = F) {
  require(sf)
  require(tidyverse)
  require(divM)

  safe_call <- possibly(Wrapper_gen.tract.div.measures,
                        otherwise = NA,
                        quiet = F)

  divs <-
    list( int = ints
         ,hwy = hwys
         ,county = counties
         ,plc = plcs
         ,school.dist = sds)

  clean.nhpn <- c(T,T,F,F,F)

  divs <- map(divs,
              ~st_set_crs(.x, "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"))

  ctdivm <-
    safe_call(cz = cz,
              divs = divs,
              clean.nhpn = clean.nhpn,
              cutout.water = cutout.water,
              year = 2019)

  if(is.na(ctdivm)) {
    message(paste0("error at cz", cz, "\n"))
    return(-1)
  }

  # create save directory if it doesn't exist
  if(!file.exists(save.dir))
    dir.create(save.dir, recursive = T)

  save.name <-
    paste0("cz-", as.character(cz), ".csv")

  save.path <- paste0(save.dir,
                      save.name)

  # write
  write.csv(ctdivm,
            save.path,
            row.names = F)

  return(1)
}

czs

'
gen_and_save(cz = "00100",
             save.dir = "tests/"
             )
'
# seems to fail where there is no division of type..


# generate via slurm ------------------------------------------------------

library(rslurm)
btwn.ct.param <-
  tibble(
    cz = czs$cz,
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/",
    cutout.water = F
  )


job <-
  rslurm::slurm_apply(
  f =
    gen_and_save,
  params = btwn.ct.param,
  jobname = "btwn-tract-divm-cts",
  nodes = 10,
  cpus_per_node = 1,
  slurm_options = list(time = "8:00:00",
                       "mem-per-cpu" = "20G",
                       'mail-type' = list('begin', 'end', 'fail'),
                       'mail-user' = 'km31@princeton.edu'),
  add_objects = c("czs",
                  "ints", "hwys", "plcs",
                  "counties", "sds"
                  )
)

job


# check where didn't generate ---------------------------------------------

#sdir <- "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/water-trimmed/"
sdir <- "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/"

gend <- list.files(sdir, #full.names = T,
           pattern = ".csv$")
length(gend)
gend <- gend %>%
  stringr::str_extract("[0-9]+")

ungend <-
  czs[!czs$cz %in% gend,]

ungend %>% tibble() %>%
  filter(grepl("Los", cz_name))

# re-send to slurm w/ more memory
library(rslurm)

gen_and_save

btwn.ct.param <-
  tibble(
    cz = ungend$cz,
    save.dir = "/scratch/gpfs/km31/Generated_measures/dividedness-measures/tract-level/by-cz/",
    cutout.water = F
  )


job <-
  rslurm::slurm_apply(
    f =
      gen_and_save,
    params = btwn.ct.param,
    jobname = "btwn-tract-divm-cts2",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "8:00:00",
                         "mem-per-cpu" = "18G",
                         'mail-type' = list('begin', 'end', 'fail'),
                         'mail-user' = 'km31@princeton.edu'),
    add_objects = c("czs",
                    "ints", "hwys", "plcs",
                    "counties", "sds"
    )
  )

rslurm::get_job_status(job)
#rslurm::cancel_slurm(job)

# addl test calls ---------------------------------------------------------

ungend

#' a recurrent error: cz - 10102 - Thomasville Error: no applicable method for
#' 'st_collection_extract' applied to an object of class "NULL" (when wasn't
#' handling no-div-in-region cases correctly)

# some topology exceptions -- as in Santa Rosa, as well (w/ hwys and water
# trimmed only)
# adding "abs.validate parameter to handle these

# additionally, sometimes points are created created when cropping divs..
czs[czs$cz == "37700",]

tmpr <- get.region.identifiers(cz = "12402")
tmpct <- tracts.from.region(tmpr) %>% divM::conic.transform()
tmpr <- tmpr %>%
  cbind(geometry = st_union(tmpct)) %>%
  st_sf()
tmpr

tmpsd <- subset.polys.divs(tmpr, sds)
tmpsd %>% mapview::mapview(zcol = "school.dist")

sds %>% count.geo()
tmpsd %>% count.geo()
# devtools::load_all()
Wrapper_gen.tract.div.measures(cz = "12402",
                               divs =
                                 list(school.dist = sds)
                               ,
                               cutout.water = F,
                               year = 2019,
                               clean.nhpn = F, #c(T, T, F, F, F),
                               validate = F
                               )

