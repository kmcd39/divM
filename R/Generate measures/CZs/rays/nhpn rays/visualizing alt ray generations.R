source("places & rays/setup ray ws.R")


# current output: ---------------------------------------------------------
# 31 rays
count.rays(plc.ids["Houston"]
           , plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)


# Removing holes from Houston ----------------------------------------------

hst <- plc[plc.ids == plc.ids["Houston"],]

deholed <- hst %>% nngeo::st_remove_holes()
deholed <- rename(deholed, geometry = geom)
# initial prep / trim hwys to area
hwy <- initial.hwy.subset(deholed, hwys)

# pass prepped hwys onto generation fcn
prepped.hwys.to.rays(deholed, hwy
                     ,include.intersecting = T
                     ,min.segment.length = 10
                     ,ray.node.distance.threshold= 100)

mapview(dehol)

st_intersection(buffered.hull(hst)
                ,hwys) %>%
  mapview(lwd = 3.5, zcol = "SIGNT1") +
  mapview(st_boundary(hst))



# from Oklahoma City ------------------------------------------------------

# Removing holes from Houston ----------------------------------------------

okc <- plc[plc.ids == plc.ids["Oklahoma City"],]

deholed <- okc %>% nngeo::st_remove_holes()
deholed <- rename(deholed, geometry = geom)
# initial prep / trim hwys to area
hwy <- initial.hwy.subset(deholed, hwys)

# pass prepped hwys onto generation fcn
prepped.hwys.to.rays(deholed, hwy
                     ,include.intersecting = T
                     ,min.segment.length = 10
                     ,ray.node.distance.threshold= 100)
# 14 rays w/ holes filled

# vs un-modified place boundary:
hwy <- initial.hwy.subset(okc, hwys)
prepped.hwys.to.rays(okc, hwy
                     ,include.intersecting = T
                     ,min.segment.length = 10
                     ,ray.node.distance.threshold= 100)
# 15 rays


st_intersection(buffered.hull(okc)
                ,hwys) %>%
  mapview(lwd = 3.5, zcol = "SIGNT1") +
  mapview(st_boundary(okc))

