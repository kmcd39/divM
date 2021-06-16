#' Water areas are different from all the other division types, which are all
#' linear (or can be transformed to linear with st_boundary).
#' This script gets water divisions and then

#' (all except water)

# I turn off spherical geometry for now... Not sure if they're ironing out
# kinks, there's an incompatibility with the GEOS on della, or if I just have to
# learn it and update my fcns
sf_use_s2(F)

# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)

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

()
# test runs ----------------------------------------------------------------
'
bw <- tracts.across.water(cbsa = "12580",
                    .cos = all.cos
                    )

tst = tracts.across.water(cbsa = cbsas$cbsa[1],
                    .cos = all.cos
                    )
tst'

# map thru ----------------------------------------------------------------

trxw <- map_dfr(cbsas$cbsa,
                ~tracts.across.water(cbsa = .,
                                     .cos = all.cos)
                )
rsl
?rslurm::slurm_apply(
  tracts.across.water
)

# write
save.dir <-
  "/scratch/gpfs/km31/dividedness-measures/tract-level/by-cbsa/"

write.csv(trxw,
          paste0(save.dir,
                 "all-tracts-x-water.csv")
          )
