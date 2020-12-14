# setup workspace ---------------------------------------------------------
source("places & rays/setup ray ws.R")

# get cleaned and trimmed to area intst plan
#intpl <- readRDS("intermediate saves/cleaned-1947-plan.RDS") # (3km buffer)
intpl <- readRDS(file = "intermediate saves/cleaned-1947-plan_6km-buffer.RDS")
#intpl <- readRDS(file = "intermediate saves/cleaned-1947-plan_6km-buffer_2807-threshold.RDS")
intpl = do.call("rbind",intpl)

# mimic structure of NHPN built hwys using plan data
# makes using the same functions easier.
intpl$SIGNT1 = "plan"
intpl$SIGN1 = paste0(intpl$SIGNT1,intpl$intst)

# load plan rays
plan.rays <- readRDS("outputs/PLAN-rays_needs-checking.RDS")

# get raw intsts from replication data ------------------------------------
county.hwy <- st_read("~/R/all sharkey geoseg work/examining others replication data/political consequences of spatial policies/data/hwyassn/joins/hwysegpointswcounties.shp")
# drop columns from Nall merging with counties
plan.raw <- county.hwy %>% 
  select(1:13) %>% conic.transform()
rm(county.hwy)


# caveats -----------------------------------------------------------------

# the plan data is so messy that it's hard to get this 100%. for example
# Syracuse is missing a ray in these results (I81 should extend southward). I
# can fix this by adjusting the parameters in the data-cleaning functions, but
# that introduces other errors elsewhere. It may make sense to go through and
# adjust manually where rays are missing.



# make dataframe from ray counts ------------------------------------------

ptbl <- tibble( name = names(plc.ids)
               ,geoid = plc.ids
               ,plan_rays = map_dbl(plan.rays, ~`[[`(., "n.rays")))
#map_dbl(PA.rays, ~`[[`(., "n.rays"))

# write results joined with cz data
plc %>%
  tibble() %>%
  select(GEOID, NAME, cz, cz_name, place.pop = population, cz.pop) %>%
  #mutate(GEOID=as.numeric(GEOID)) %>%
  left_join(ptbl,
            by=c("GEOID" = "geoid")) %>%
  write.csv(file = "outputs/plan-rays_Preliminary-results.csv")



# which places have any interstate intersecting w/ them? ------------------
poss.places <- st_intersection(plan.raw,
                buffered.hull(plc,3000) ) %>%
  count(GEOID, NAMELSAD, intst)

# there's 332 of them...
length(unique(poss.places$GEOID))

# setup ez overlay --------------------------------------------------------

plan.ray.check.overlay <- function(geoid, raw.pts = poss.places,
                                   cleaned.plan = intpl, 
                                   places = plc) 
  {
  area = places[places$GEOID == geoid,]
  cleaned.plan = st_intersection(buffered.hull(area,3000)
                                 ,cleaned.plan)
  raw.pts = poss.places[poss.places$GEOID == geoid,]
  mapview(st_boundary(area), color = "#800030") +
    mapview(cleaned.plan, zcol = "intst") +
    mapview(raw.pts, zcol = "intst")
  }



# iterate and check all the places ----------------------------------------

for(i in 1:length(unique(poss.places$GEOID))) {
  geoid = unique(poss.places$GEOID)[i]
  cat("place",i,"of",length(unique(poss.places$GEOID)),"\n")
  cat("showing", unique(poss.places[poss.places$GEOID == geoid,]$NAMELSAD), "| geoid: ",geoid,"\n")
  cat("calculed",ptbl[ptbl$geoid == geoid,]$plan_rays,"plan rays for this area\nShowing map\n")
  print(plan.ray.check.overlay(geoid))
  readline(prompt="Press [enter] to continue")
}
plan.ray.check.overlay(plc.ids["Syracuse"])

plan.rays$Syracuse
plan.rays$`New York`

# so far 1 to 123 are check; 124-332 remain.



# checking individual places ----------------------------------------------
plan.ray.check.overlay(plc.ids["San Diego"])
plan.ray.check.overlay(plc.ids["Tucson"])


