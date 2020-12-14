# setup workspace ---------------------------------------------------------
source("data-raw/rays/setup ray ws.R")

# generate rays -----------------------------------------------------------
#plc <- plc %>% filter(STATEFP  == 42) # (for test state)

options(future.globals.maxSize= 891289600)
# only interstates
rays <-
  furrr::future_imap( plc.ids,
                      ~ count.rays(., plc, hwys
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



# illustrations of edge cases ---------------------------------------------

# S99 comes to close to interstate. Should I buffer node to circle to do st_intersects?
'rb %>%
  filter(SIGN1 %in% c("S30", "S99")) %>%
  mapview(zcol = "SIGN1") +
  mapview(intb2)
'




# arbitray checks ---------------------------------------------------------
'tibble( name = names(plc.ids)
        ,geoid = plc.ids
        ,rays_interstate_only = map_dbl(rays, ~`[[`(., "n.rays")))

count.rays(plc.ids["Philadelphia"], plc, hwys
             ,min.segment.length = 10
             ,ray.node.distance.threshold= 100
             ,include.map = TRUE
             ,verbose = T)
rays$`State College`
count.rays(plc.ids["Scranton"], plc, hwys
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)

'
count.rays(plc.ids["Oklahoma City"], plc, hwys
           ,min.segment.length = 10
           ,include.intersecting = T
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)
