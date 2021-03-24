# setup workspace ---------------------------------------------------------
source("places & rays/setup ray ws.R")


# get cleaned interstate hwy plan
#hwyplan = st_read("~/R/shapefiles/1947plan/addl cleans 2/cleaner-hwy-plan.shp")
hwyplan = st_read("~/R/shapefiles/1947plan/most addl cleans/cleaner-hwy-plan.shp")
# mimic structure of NHPN built hwys using plan data
# makes using the same functions easier.
hwyplan$SIGNT1 = "plan"
hwyplan$SIGN1 = paste0(hwyplan$SIGNT1,hwyplan$id)


# spatial clean hwy data --------------------------------------------------
# FIX NOLA & houston (check)
'library(mapedit)
library(leaflet)
mapview(hwyplan)
hpc <- hwyplan %>% filter(id != 161)
hpc %>% mapview()
hpc2 <- mapedit::editFeatures(hpc)
hpc2 %>% mapview()
intpl <- st_transform(hwyplan
                      ,st_crs(plc))

library(lwgeom)
sum(!st_is_valid(intpl))
intpl <- st_set_precision(intpl, 1000)
intpl <- st_make_valid(intpl)
st_write(hpc2, "~/R/shapefiles/1947plan/addl cleans 2/cleaner-hwy-plan.shp")'

# associate with place geoids ---------------------------------------------
intpl <- st_transform(hwyplan
                      ,st_crs(plc))


plc <- st_buffer(plc, 8046.72)

# mapview(plc)
# generate -----------------------------------------------------------
# setup
#plc <- plc %>% filter(STATEFP  == 36) # (for test state)
plc.ids <- plc$GEOID
names(plc.ids) <- plc$NAME

plan.rays <-
  purrr::map(plc.ids,
  #furrr::future_imap( plc.ids,
                      ~ count.rays(., plc, intpl
                                   ,always.include = "plan"
                                   ,min.segment.length = 10
                                   , filter.colinear.node.threshold = 50)
  )
   #                   ,.progress = TRUE)
#plan.rays
saveRDS(plan.rays, "outputs/PLAN-rays_buffered-places.RDS")



# checks ------------------------------------------------------------------

plan.rays$`New Orleans`
plan.rays$Philadelphia
plan.rays$`San Diego`
plan.rays$Chicago

intpl %>%
  st_crop(st_bbox(plc[grepl("San Diego",plc$NAME), ])) %>%
  mapview(zcol = "id") +
  mapview(plc[grepl("San Diego",plc$NAME), ], color = "red")


# pull baum-snow's measures -----------------------------------------------
# baum-snow's plan rays
bspr =foreign::read.dta(paste0("~/R/all sharkey geoseg work/examining others replication data/political consequences of spatial policies/data/snow/data/",
                               "rays_plan.dta"))

head(bspr)



# compile csv -------------------------------------------------------------
tbl <- tibble( name = names(plc.ids)
               ,geoid = plc.ids
               ,plan_rays = map_dbl(plan.rays, ~`[[`(., "n.rays"))
               #,rays_include_intersecting = map_dbl(rays.variation, ~`[[`(., "n.rays"))
               )

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
  write.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/1947-plan-rays_buffered-places.csv")


# map for checking results ------------------------------------------------

plcs <- tbl2 %>%
           left_join(plc[,c("GEOID", "geometry")]) %>% st_sf() %>% st_transform(4326)
mapview(hwyplan$geometry) +
  mapview(plcs
          , color = "#008080"
          , alpha.regions = .1, lwd =2.5)


# scratch ------------------------------------------------------------------


count.rays(3673000, plc, intpl
           ,always.include = "plan"
           ,min.segment.length = 300
           ,include.map = T
           , filter.colinear.node.threshold = 100)


clean.plan_all.intst.single.place(syra.int, threshold = units::set_units(2900, "m"))
syra.int

count.rays(3673000, plc, intpl
           ,always.include = "plan"
           ,min.segment.length = 300
           ,include.map = T
           , filter.colinear.node.threshold = 100)

plan.rays$Syracuse
plan.rays$`New York`


# overlay points:
county.hwy <- st_read("~/R/all sharkey geoseg work/examining others replication data/political consequences of spatial policies/data/hwyassn/joins/hwysegpointswcounties.shp")
# drop columns from Nall merging with counties
plan.raw <- county.hwy %>%
  select(1:13) %>% conic.transform()
rm(county.hwy)


overlay.raw.plan <- function(plc.geoid, base = plan.rays, raw=plan.raw) {
  basemap = base[]


}
plan.rays["New York"]
plan.rays$`New York`$map +
  mapview(st_intersection(plan.raw, plc[plc$GEOID==plc.ids["New York"],]))


# checking roanoke
st_intersection(plan.raw, plc[plc$GEOID==plc.ids["Roanoke"],]) %>% mapview() +
  mapview(
    st_intersection(intpl, plc[plc$GEOID==plc.ids["Roanoke"],])) +
  mapview( st_boundary (plc[plc$GEOID==plc.ids["Roanoke"],]))



initial.hwy.subset(plc[plc$GEOID==plc.ids["Roanoke"],]
                   , intpl )


'count.rays("0455000",
           plc, intpl
           ,always.include = "plan"
           ,min.segment.length = 300
           , filter.colinear.node.threshold = 100) # Phoenix
count.rays("0177256", plc, intpl
           , always.include = "plan"
           , min.segment.length = 300
           , filter.colinear.node.threshold = 100) '


# generate ----------------------------------------------------------------

#planned!
count.rays(plc.ids["New York"]
           ,plc, plan
           ,min.segment.length = 0
           ,always.include = "plan"
           ,include.intersecting = F
           ,ray.node.distance.threshold = 500
           ,include.map = T)

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
