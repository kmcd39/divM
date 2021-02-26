
# setup workspace ---------------------------------------------------------
library(dplyr)
library(sf)
library(purrr)
library(lwgeom)
rm(list = ls())

#hwys <- divDat::hwys
hwy.plan <- divDat::hwy.plan
geo.list <- divDat::geo.list


source("division-index-fcns.R")
source("places & rays/places & rays fcns.R")
# initialize dividedness indices with region boundaries attr.
# (drop geometry column)
div.inds <- imap(geo.list,
                 ~abv_out(.))
div.inds <- do.call("rbind", div.inds)
# initialize dividedness indices with region boundaries attr.
# (drop geometry column)

# check file completeness -----------------------------------------
abv_out(hwy.plan) %>% count(region.type)



# get lengths for all division types (plan, hwy.type, total) --------------------------------------
# aggregate each road by road type within each division.
# count groups the geometries as well as counting features
grouped.plan <- hwy.plan %>%
  count(region.type, region.id, region.name)

# get length by division type
grouped.plan <- grouped.plan %>%
  mutate( planned.len = geod.length(.)
         ,hwy.type = "plan.interstate")


plan.div_lengths <- 
  divFcns::abv_out(grouped.plan) %>%
  ungroup() %>%
  select(-n)

# get actual hwy length measures ------------------------------------------

div.ind <- read.csv("outputs/length-ratios_v2.csv") %>%
  select(-X)
# drop ratio cols from div.inds (will re-calculate w/ all)
div.ind <-
  div.ind %>%
  select(-c(contains("length.to.")))

# put in uniform format & bind ---------------------------------------------------
colnames(div.ind)
totals <- div.ind %>%
  select(c(1:8,"total.len")) %>%
  unique()
totals <- totals %>%
  rename(len = total.len) %>%
  mutate(hwy.type = "total.len_built")

div.ind <- div.ind %>%
  rename(hwy.type = SIGNT1) %>%
  select(-total.len) %>%
  rbind(totals) %>%
  arrange(region.id, region.name, region.type)

colnames(plan.div_lengths)
div.ind <- plan.div_lengths %>%
  left_join(unique(div.ind[,1:8])) %>% 
  rename(len = planned.len) %>%
  rbind(div.ind) %>%
  arrange(region.id, region.name, region.type)
  

div.ind <- div.ind %>%
  select(c(1:3, state,
           population, households, cts, 
           region_area, hwy.type, len))

div.ind %>% head(10)


# checks ------------------------------------------------------------------
div.inds %>% head()
div.ind %>% filter(hwy.type == "plan.interstate")

div.ind %>%
  arrange(region.type,region.id,hwy.type) %>%
  head(10)

# get ratio indices ------------------------------------------------
# ratio divisions to boundary areas and populations
div.ind <-
  div.ind %>%
  mutate(length.to.area = as.numeric(len) / as.numeric(region_area)
         ,length.to.pop = as.numeric(len) / population)


div.ind %>% summary()
div.ind %>% filter(is.na(len))

# clean -------------------------------------------------------------------

write.csv(div.ind, "outputs/length-ratios-all_v2.csv")
