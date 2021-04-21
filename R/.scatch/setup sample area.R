
atl.x <- xwalks::co2cz %>%
  filter(grepl("Atlanta", cz_name))


(race.vars <- paste0("B03002_",
                     stringr::str_pad(1:12, 3, "left", "0")))

# get values
atl <-
  map2_dfr(substr(atl.x$countyfp,1,2),
           substr(atl.x$countyfp,3,5),
           ~tidycensus::get_acs(geography = "tract",
                                variables = race.vars,
                                state = .x,
                                county = .y,
                                year = 2019,
                                geometry = T)
  )


# get human-legible names
acs <- tidycensus::load_variables(2019, "acs5")
rvs <- acs %>% filter(name %in% race.vars)
rvs$label <-
  with(rvs, stringr::str_extract(label, "([^!!]*)$"))

rvs

# make wide
colnames(atl) <- tolower(colnames(atl))
atl <- atl %>%
  select(geoid, variable, estimate, geometry) %>%
  tibble() %>%
  pivot_wider(id_cols =
                c(geoid, geometry),
              names_from = variable,
              values_from = estimate)

# drop double-counting colms (all non-hispanic; muliracial bkdwn)
atl <- atl %>%
  select(-c(B03002_002 ,B03002_010,B03002_011))
# totals now match population
atl <- atl %>% rename(pop = B03002_001)
atl[4:11] %>% rowSums() %>% head()
atl[["pop"]] %>% head()

sum(!{rowSums(atl[4:11])} == {atl[["pop"]]})
atl[4:11]

# colnames dd
dd <- rvs %>% filter(name %in% colnames(atl))
dd

# clean labels
dd$label <-
  dd$label %>%
  gsub("!!", " ", .) %>%
  gsub("Estimate Total|:", "", .) %>%
  gsub("alone", "", .) %>%
  trimws()

rm(acs, atl.x)
atl


# get hwys/for sample area -----------------------------------------------------

hwys <- st_read("~/R/shapefiles/National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp")

hwys <- st_transform(hwys, st_crs(st_sf(atl)))

atl.hwys <- st_intersection(hwys,
                            st_union(st_sf(atl)))

#rm(hwys)

# save environment -------------------------------------------------------------
save.image("atl.rdata")
