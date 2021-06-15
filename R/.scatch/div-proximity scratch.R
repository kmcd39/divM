library(tidyverse)
library(sf)


# santa rosa hwy topology exception ---------------------------------------

# piecewise santa rosa
srr <- get.region.ident
srct <- tracts.from.region(region,
                           cutout.water = cutout.water,
                           year = 2019)

srh <- hwys %>% st_intersection(czs[czs$cz == "37700",]$geometry)
srh <- srh %>% filter(!is.na(SIGNT1))
srh %>% tibble() %>% count(SIGNT1)

srh %>% mapview::mapview(zcol = "SIGNT1", lwd = 3)

# work out nphn fix bug
tmpn <-
  srh %>%
  split(.$SIGN1) %>%
  purrr::imap( ~denode.lines(.) )

tmpn %>%
  purrr::map2(names(tmpn),
              ~{cat(paste.y)
                fill.gaps(.x, threshold = 200,
                          return.gap.map = F)
              } )

# the problem area:
library(mapview)
tmpn$S121 %>%
  mapview::mapview(zcol = "SIGNT1", lwd = 3) +
  mapview(
    st_sf(
      geometry = st_sfc(st_point(x = c(-2769161.6780800503,
                                       5030064.9430927793)))
      , crs = st_crs(tmpn$S1))
    ,color = "red",
    cex = 3)



tmp <-
  tmpn$S121

fill.gaps(tmp)

#tmp <-
tmp %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  fill.gaps()

fill.gaps(tmp)

'group_by(SIGNT1, SIGN1) %>%
  summarise(., do_union = T) %>%
  st_line_merge() %>%
  st_cast("LINESTRING") %>%
'

srh <- srh %>%
  Fix.all.hwys()

srh[2] %>% plot()

map(divs,
    ~do.call(
      subset.polys.divs,
      c(list(ctsf, .x),
        params))
)

divs <-
  map( divs,
       ~{.x %>%
           st_set_precision(1e5) %>%
           st_make_valid() })
divs


# -------------------------------------------------------------------------


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
