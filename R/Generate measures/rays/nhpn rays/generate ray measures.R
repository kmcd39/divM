# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
devtools::load_all()
# source(here::here("R/Generate measures/rays/setup ray ws.R"))
load(here::here("R/Generate measures/rays/ray ws.Rdata"))



# test run ---------------------------------------------------------------------
Count.rays(plc.ids["Providence"],
           hwys
           ,plc
           ,min.segment.length = 10
           ,include.map = T
           ,verbose = T)


# elligiple places -------------------------------------------------------------

sbgp <- st_intersects(plc, filter(hwys, SIGNT1 == "I"))
int.eligible <- plc$geoid[lengths(sbgp) > 0]

sbgp <- st_intersects(plc, lac)
lac.eligible <- plc$geoid[lengths(sbgp) > 0]

# rejoin to names
int.eligible <- plc.ids[plc.ids %in% int.eligible]
lac.eligible <- plc.ids[plc.ids %in% lac.eligible]

# generate rays -----------------------------------------------------------
#plc <- plc %>% filter(STATEFP  == 42) # (for test state)

options(future.globals.maxSize= 891289600)
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


