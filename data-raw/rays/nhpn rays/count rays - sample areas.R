# setup ws ----------------------------------------------------------------
source("places & rays/setup ray ws.R")



# view selected areas -----------------------------------------------------

?divFcns::count.rays

# interstates only
count.rays(plc.ids["Chicago"] #Philadelphia
           , plc, hwys
           ,min.segment.length = 5
           ,ray.node.distance.threshold= 100
           ,length.floor = 2000
           ,include.intersecting = FALSE
           ,include.map = TRUE
           ,verbose = T)

# include intersecting
count.rays(plc.ids["Chicago"] #Philadelphia
           , plc, hwys
           ,min.segment.length = 5
           ,length.floor = 2000
           ,ray.node.distance.threshold= 100
           ,include.intersecting = T
           ,include.map = TRUE
           ,verbose = T)


# gerrymandered place question mark:
count.rays(plc.ids["San Diego"]
           , plc, hwys
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)



count.rays(plc.ids["Oklahoma City"]
           , plc, hwys
           ,remove.holes = F # TRUE 
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)
# ^^ 15 vs 1 rays depending on whether holes are kept

# peripheral ray questionmark:
count.rays(plc.ids["Emporia"]
           , plc, hwys
           ,remove.holes = F 
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100
           ,include.map = TRUE
           ,verbose = T)





# duplicated name indexing
names(plc.ids)[duplicated(names(plc.ids))]
plc.ids[names(plc.ids)=="Portland"]
