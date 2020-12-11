tmp.czs


tmp <- xwalks::co2cz %>% filter(cz %in% tmp.czs$region.id)
tmp$countyfp

states <- substr(tmp$countyfp, 1,2)
counties <- substr(counties, 3,5)

tmp.water <- map_dfr(c("001","009","011"),
               ~tigris::area_water(state = "25",
                                   county = .,
                                   year = 2019))

counties %>% filter(statefp == "25" &
                      countyfp %in% c("001","009","011")) %>%
  st_boundary() %>%
  mapview(color = "red") +
  mapview(tmp.water)




devtools::document()
devtools::load_all()

# test w/ debug
#tmp.czs$region.id
divM::get.clean.tigris.water(czs = c("20500", "20800"),
                             out.crs = 4326)
