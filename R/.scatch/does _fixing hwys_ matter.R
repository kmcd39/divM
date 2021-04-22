# does applying `Fix.all.hwys` matter in terms of polygon measure? I just tested with
# 4 cities and dif hwy subsets. It changed the results by 1 for 1 city (46 to 47
# polys in philly; same elsewhere). The results are "more correct" in this one case
# after the fcns are applied, based on looking at the map.


library(sf)
hwys <- st_read("~/R/shapefiles/National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp")
hwys <- hwys %>% divM::conic.transform()

xwalks::co2cz %>% filter(grepl("St. L", cz_name))
xwalks::co2cz %>% filter(grepl("Minnea", cz_name))
xwalks::co2cz %>% filter(grepl("Atlanta", cz_name))
tmp <-
  get.region.identifiers(cz =
                           # 24701 # st louis
                           #21501 # minneapolis
                         # "09100" # atlanta
                         "19700" # philly

                         )

tmp
cts <- tracts.from.region(tmp)
cts <- cts %>% divM::conic.transform()

tmp$geometry <- st_union(cts)
tmp <- st_sf(tmp)

tmphw <- subset.polys.divs(tmp, hwys, "SIGNT1", c("I", "U"))

tmphw_F <- tmphw %>%
  divM::denode.lines() %>%
  divM::Fix.all.hwys()


polygonal.div(
  tmp,
  tmphw,
  return.sf = T
) %>%
  mapview::mapview(zcol = "id")

polygonal.div(
  tmp,
  tmphw_F
  ,return.sf = T
) %>%
  mapview::mapview(zcol = "id")
)



