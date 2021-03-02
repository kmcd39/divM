devtools::load_all()

# initialize dividedness indices with region boundaries attr.
# (drop geometry column)
#div.inds <- imap(geo.list, ~abv_out(.))
div.inds <- divM::abv_out(divDat::czs) %>% divM::regi


# get lengths for all division types --------------------------------------
#
# aggregate each road by road type within each division.
# count groups the geometries as well as counting features
grouped.hwys <- hwys %>%
  count(region.type, region.id, region.name, SIGNT1)

grouped.hwys <- grouped.hwys %>%
  mutate(len = geod.length(.))


# total length type I, U, S
length_totals <-
  divFcns::abv_out(grouped.hwys) %>%
  ungroup() %>%
  group_by(region.type, region.id, region.name) %>%
  summarise(total.len = sum(len))

div_lengths <-
  ungroup(grouped.hwys) %>%
  arrange(region.type ,region.id, SIGNT1) %>%
  full_join(length_totals)


div.inds <- do.call("rbind", div.inds)

div.inds <-
  left_join(div.inds,div_lengths
            ,by = c("region.id"
                    ,"region.name"
                    ,"region.type") )

div.inds <- divFcns::abv_out(div.inds) %>%
  select(-n)

# get ratio indices ------------------------------------------------
# ratio divisions to boundary areas and populations
div.inds <-
  div.inds %>%
  mutate(length.to.area = as.numeric(len) / as.numeric(region_area)
         ,length.to.pop = as.numeric(len) / population)

# write -------------------------------------------------------------------
head(div.inds)

write.csv(div.inds, "outputs/length-ratios_v2.csv")
