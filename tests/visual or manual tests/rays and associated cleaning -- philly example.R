source("data-raw/rays/setup ray ws.R")
devtools::document()
devtools::load_all()


#sample cz
divDat::czs %>% filter(cz == "19700") %>% divM::region.reorg("cz")
# sample place
tmp <- plc %>% filter(name == "Philadelphia")


# initial.hwy2ray.subset -------------------------------------------------------
hwys %>% tibble() %>% count(SIGNT1)

hwy.prepped <-
  initial.hwy2ray.subset(tmp,
                         #lac,
                         hwys,
                         always.include = c("I","U","S"),
                       include.intersecting = F)

lac <- hwys %>% filter(FCLASS %in% divM::lac_codes |
                         SIGNT1 %in% "I")
lac.prepped <-
  initial.hwy2ray.subset(tmp,
                         lac,
                         include.intersecting = F)
mapview(hwy.prepped, zcol = "FCLASS", lwd = 3) +
  mapview(st_boundary(tmp), color = "red")


# get rays ---------------------------------------------------------------------

phl.rays <- Get.bundled.ray.output(tmp,
                                   hwy.prepped)
phl.lac.rays <- Get.bundled.ray.output(tmp,
                                       lac.prepped)

phl.rays
phl.lac.rays

# Or, using wrapper ---------------------------------------------------------

# use place name-id index
plc.ids


Count.rays(plc.ids[["Philadelphia"]],
           hwys,
           always.include = c("I","U","S")
           )
