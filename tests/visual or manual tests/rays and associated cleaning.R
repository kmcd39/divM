source("data-raw/rays/setup ray ws.R")
devtools::document()
devtools::load_all()


#sample cz
divDat::czs %>% filter(cz == "19700") %>% divM::region.reorg("cz")
# sample place
tmp <- plc %>% filter(name == "Philadelphia")


# initial.hwy2ray.subset -------------------------------------------------------
hwys %>% tibble() %>% count(SIGNT1)
lac <- hwys %>% filter(FCLASS %in% divM::lac_codes |
                         SIGNT1 %in% "I")
hwy.prepped <-
  initial.hwy2ray.subset(tmp, lac, always.include = NULL,
                       include.intersecting = F)

library(ggplot2)
ggplot() +
  geom_sf(data = hwy.prepped,
          aes(color = "SIGNN1")) +
  geom_sf(data = st_boundary(tmp))
lac
tmp


# get rays ---------------------------------------------------------------------
phl.rays <- Get.bundled.ray.output(tmp,
                                   hwy.prepped)

# trim.to.length.floor ---------------------------------------------------------

hwys2ray.nodes

