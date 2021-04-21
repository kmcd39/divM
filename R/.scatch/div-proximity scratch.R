library(tidyverse)
library(sf)
# sample area
load("atl.rdata")

atl %>% st_sf()

hwys <-
  atl.hwys

plot(st_sf(atl)[1])
plot(st_sf(hwys)[1])


sbgp <-
  st_intersects(st_sf(atl),
                filter(hwys, SIGNT1 == "I")
                )

atl$touches.hwy <-
  lengths(sbgp) > 0

st_sf(atl)["touches.hwy"] %>% plot(main = "ATL touches interstate")



?geosphere::dist2Line



hwys <- divM::denode.lines(hwys)

ints <- filter(hwys, SIGNT1 == "I")

sbgp <-
  st_is_within_distance(
    st_sf(atl)[1:5,],
    ints,
    400)

sbgp

nngeo::st_nn(
  st_sf(atl)[1:5,],
  ints,
  maxdist = 400
)
