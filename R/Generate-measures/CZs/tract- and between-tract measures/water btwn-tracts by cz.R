#' Water areas are different from all the other division types, which are all
#' linear (or can be transformed to linear with st_boundary).
#' This script gets water divisions and then

#' (all except water)

# I turn off spherical geometry for now... Not sure if they're ironing out
# kinks, there's an incompatibility with the GEOS on della, or if I just have to
# learn it and update my fcns

# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
sf_use_s2(F)
# also do this for the caching?
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")

devtools::load_all()
#library(divM)

rm(list=ls())

# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

all.cos <- tigris::counties()
colnames(all.cos) <-
  tolower(colnames(all.cos))
all.cos <- all.cos %>% st_transform(st_crs(czs))

# test runs ----------------------------------------------------------------
'
bw <- tracts.across.water(cz = "12580",
                    .cos = all.cos
                    )

tst = tracts.across.water(cz = czs$cz[1],
                    .cos = all.cos
                    )
tst'

# map thru ----------------------------------------------------------------
'
trxw <- map_dfr(czs$cz,
                ~tracts.across.water(cz = .,
                                     .cos = all.cos)
                )
'
quick.cz.della.wrapper <- function(cz,
                                     save.dir) {

  require(sf)
  require(tidyverse)
  require(divM)

  cat("generating cz", cz)

  write.path <-
    paste0(save.dir,
           "all-tracts-x-water.csv")

  safe_call <- possibly(tracts.across.water,
                        otherwise = NA,
                        quiet = F)

  safe_call(cz = cz,
            .cos = all.cos,
            write.path = write.path
            )
}

j <-
  rslurm::slurm_apply(
    quick.cz.della.wrapper,
    params = tibble(cz = czs$cz,
                    save.dir =
                      "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cz/"
    ),
    jobname = "cts-btwn-water_bycz",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "12:00:00",
                         "mem-per-cpu" = "10G",
                         "mail-type" = list("begin", "end", "fail"),
                         "mail-user" = "km31@princeton.edu"),
    add_objects = c("all.cos", "czs")
  )



# checking ----------------------------------------------------------------


g <- list.files(
  "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cz/",
  pattern = "water.*csv", full.names = T)
g

g <- vroom::vroom(g)
gend <- g %>% count(cz) %>% pull(cz)


ungend <- czs[!czs$cz %in% gend,]

quick.cz.della.wrapper(ungend$cz[2],
                       "/scratch/gpfs/km31/tests/"
                       )


divM::tracts.from.region(
  divM::get.region.identifiers(cz = "00100")
)

xwalks::co2
