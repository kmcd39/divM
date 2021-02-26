# setup ws ----------------------------------------------------------------
library(sf)
library(tidyverse)
devtools::load_all()
source(here::here("R/Generate measures/rays/setup ray ws.R"))

# view selected areas -----------------------------------------------------


# interstates only
tmpi <-
  Count.rays(plc.ids["Chicago"] #Philadelphia
             , hwys
             , place.sf = plc
             ,min.segment.length = 5
             ,ray.node.distance.threshold= 100
             ,length.floor = 2000
             ,include.intersecting = FALSE
             ,include.map = TRUE
             ,verbose = T
             )

# include intersecting
Count.rays(plc.ids["Chicago"] #Philadelphia
           , hwys
           , place.sf = plc
           ,min.segment.length = 5
           ,length.floor = 2000
           ,ray.node.distance.threshold= 100
           ,include.intersecting = T
           ,include.map = TRUE
           ,verbose = T)


# gerrymandered place question mark:
count.rays(plc.ids["San Diego"]
           , hwys
           , place.sf = plc
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)



count.rays(plc.ids["Oklahoma City"]
           , hwys
           , plc
           ,remove.holes = F # TRUE
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)
# ^^ 15 vs 1 rays depending on whether holes are kept

# peripheral ray questionmark:
count.rays(plc.ids["Emporia"]
           , hwys
           , plc
           ,remove.holes = F
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)





# duplicated name indexing example
names(plc.ids)[duplicated(names(plc.ids))]
plc.ids[names(plc.ids)=="Portland"]



# illustrations of edge cases ---------------------------------------------

# S99 comes to close to interstate. Should I buffer node to circle to do st_intersects?
'rb %>%
  filter(SIGN1 %in% c("S30", "S99")) %>%
  mapview(zcol = "SIGN1") +
  mapview(intb2)
'




# arbitrary checks ---------------------------------------------------------
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
