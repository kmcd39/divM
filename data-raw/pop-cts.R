# creates dataframe with population by census tract
# this is used for filtering sub-polygons by population in polygon subdivision

# (2015 cts)


# populations from census api thru tidycensus
# ACS doesn't have info for territories except DC and PR
states <- setdiff(xwalks::state2div$statefp, c(60, 66, 69, 78))
ct.pops <- purrr::map_dfr(states,
                          ~{cat("state", .,"\n")

                            tidycensus::get_acs(
                              geography = "tract",
                              variables = "B01003_001",
                              year = 2015,
                              state = .,
                              cache_table = T,
                              survey = "acs5")}
                          )

ct.pops <- ct.pops %>%
  select(geoid = GEOID,
         population = estimate)

# add xwalks to czs
(ct.pops <- left_join(xwalks::ctx[,c(4:6)],
                      ct.pops))



# write ------------------------------------------------------------------------
usethis::use_data(ct.pops,
                  overwrite = T)

