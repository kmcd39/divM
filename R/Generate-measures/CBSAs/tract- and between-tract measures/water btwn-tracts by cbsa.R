#' Water areas are different from all the other division types, which are all
#' linear (or can be transformed to linear with st_boundary).
#' This script gets water divisions and then

#' (all except water)


# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

# I turn off spherical geometry for now... Not sure if they're ironing out
# kinks, there's an incompatibility with the GEOS on della, or if I just have to
# learn it and update my fcns
sf_use_s2(F)
# also do this for the caching?
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")

devtools::load_all()
#library(divM)

rm(list=ls())

# source(here::here("R/Generate-measures/rays/setup ray ws.R"))
load(here::here("R/Generate-measures/ray-ws.Rdata"))

# only generate metro areas (not micro)
# https://www2.census.gov/geo/pdfs/reference/LSADCodes.pdf
cbsas <- cbsas %>%
  filter(lsad %in% "M1") %>% select(-lsad)

all.cos <- tigris::counties()
colnames(all.cos) <-
  tolower(colnames(all.cos))

# test runs ----------------------------------------------------------------
'
bw <- tracts.across.water(cbsa = "12580",
                    .cos = all.cos
                    )

tst <- tracts.across.water(cbsa = "15180"#cbsas$cbsa[10]
                    ,.cos = all.cos
                    )
tst
quick.cbsa.della.wrapper("15180",
                         "/scratch/gpfs/km31/tests/")

'
# map thru ----------------------------------------------------------------
'
trxw <- map_dfr(cbsas$cbsa,
                ~tracts.across.water(cbsa = .,
                                     .cos = all.cos)
                )
'
quick.cbsa.della.wrapper <- function(cbsa,
                                     save.dir) {

  # setup
  require(sf)
  require(tidyverse)
  require(divM)

  sf_use_s2(F)
  options(tigris_use_cache = TRUE)
  Sys.setenv("VROOM_SHOW_PROGRESS"="false")

  # run
  cat("generating cbsa", cbsa, "\n")

  write.path <-
    paste0(save.dir,
           "all-tracts-x-water.csv")

  safe_call <- possibly(tracts.across.water,
                        otherwise = NA,
                        quiet = F)

  safe_call(cbsa = cbsa,
            .cos = all.cos,
            write.path = write.path
  )
}

# send to della -----------------------------------------------------------

# to remove previous file.
# file.remove("/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/all-tracts-x-water.csv")

j <-
  rslurm::slurm_apply(
    quick.cbsa.della.wrapper,
    params = tibble(cbsa = cbsas$cbsa,
                    save.dir =
                      "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/"
    ),
    jobname = "cts-btwn-water_bycbsa",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "12:00:00",
                         "mem-per-cpu" = "10G",
                         "mail-type" = list("begin", "end", "fail"),
                         "mail-user" = "km31@princeton.edu"),
    add_objects = c("all.cos", "cbsas")
  )



# check generation ----------------------------------------------------------------

g <- list.files(
  "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/",
  pattern = "water.*csv", full.names = T)
g

g <- vroom::vroom(g)
gend <- g %>% count(cbsa) %>% pull(cbsa)
gend

ungend <- cbsas[!cbsas$cbsa %in% gend,]
ungend


# resend ungenerated ------------------------------------------------------

j <-
  rslurm::slurm_apply(
    quick.cbsa.della.wrapper,
    params = tibble(cbsa = ungend$cbsa,
                    save.dir =
                      "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/"
    ),
    jobname = "cts-btwn-water_bycbsa3",
    nodes = 10,
    cpus_per_node = 1,
    slurm_options = list(time = "2:00:00",
                         "mem-per-cpu" = "10G",
                         "mail-type" = list("begin", "end", "fail"),
                         "mail-user" = "km31@princeton.edu"),
    add_objects = c("all.cos", "cbsas")
  )


# troubleshooting ---------------------------------------------------------

devtools::load_all()

tst <-
  tracts.across.water(cbsa =
                        cbsas$cbsa[3],
                      .cos = all.cos
                      )
tst %>% count(poly.id)


tst <- quick.cbsa.della.wrapper(cbsa =
                                  cbsas$cbsa[3],
                                  #ungend$cbsa[4],
                                save.dir = "/scratch/gpfs/km31/tests/")

list.files("/scratch/gpfs/km31/tests/",
           pattern = "water.*csv", full.names = T) %>%
  read.csv()
