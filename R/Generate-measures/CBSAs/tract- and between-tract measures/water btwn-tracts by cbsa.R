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

sfg.seg::write.running.table()
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

  require(sf)
  require(tidyverse)
  require(divM)

  cat("generating cbsa", cbsa)

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
  pattern = "water", full.names = T)
g
g <- vroom::vroom(g)

gend <- g %>% count(cbsa) %>% pull(cbsa)
cbsas
gend
to.gen <- cbsas[!cbsas$cbsa %in% gend, ]


devtools::load_all()

tst  <- tracts.across.water(cbsa =
                              to.gen$cbsa[15],
                          .cos = all.cos
                          )
