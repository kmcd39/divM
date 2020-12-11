# this does mostly visual and manual tests.
devtools::document()
devtools::load_all()

czs <- divDat::czs %>% divM::region.reorg("cz")
phl <- czs %>% filter(region.name == "Philadelphia")


# get hwys for testing ---------------------------------------------------------
shp.dir <- "~/R/shapefiles/"
hwys <- st_read(paste0(shp.dir, "National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))
hwys <- hwys %>%
  select(c(div.id = 1, div.name = LNAME,
           county = CTFIPS,
           SOURCE,  # data source
           F_SYSTEM, FCLASS, # addl hwy classification
           LRSKEY, # Uniquely identifies a route within a state
           SIGNT1, SIGNN1, SIGN1,
           MILES, KM, state = STFIPS, geometry))

# make uniform metered crs -----------------------------------------------
czs <- czs %>% divM::conic.transform()
hwys <- hwys %>% divM::conic.transform()


# create hwy polygons ----------------------------------------------------------

?divM::Polys.wrapper
polyp = divM::Polys.wrapper(phl, hwys,
                    fill.gaps =T,
                    min.size = 0,
                    return.sf = T
                    )
# View
library(mapview)
polyp %>% mapview(zcol = "id")


# create a version with water removed ------------------------------------------
source(".auth/.auth.R")

con <- dblinkr::db.connect(db.usr, db.pw)
phl.nowater <-
  divM::query.and.cutout.water(con, phl, trim.unnamed = T)

plot(phl.nowater['region.name'])

polyp.nowater = divM::Polys.wrapper(phl.nowater, hwys,
                                    fill.gaps =T,
                                    min.size = 0,
                                    return.sf = T
)
# View
library(mapview)
polyp.nowater %>% mapview(zcol = "id")


# test population interpolation from tracts ------------------------------------

# setup pop cts
phcts <- divM::ct.pops %>%
  filter(cz_name == "Philadelphia") %>%
  divM::attach.tract.geometry()

# view
phcts %>% mapview(zcol = "population")
# huh. bizarre to me that that one tract has >16,000 population.

# check interpolation
poly.pops = areal::aw_interpolate( polyp, tid = id
                                   ,phcts, sid = geoid
                                   ,weight = "sum"
                               ,extensive = c("population")
                               ,output = "sf" )

poly.pops$population <- round(poly.pops$population)

# View & peak
poly.pops %>% mapview(zcol="population")
poly.pops$population %>% summary()
poly.pops$population %>% sort()



# check w/ water removed
poly.pops.nowater = areal::aw_interpolate( polyp.nowater, tid = id
                                   ,phcts, sid = geoid
                                   ,weight = "sum" # "total" # tracts are co-terminous w/ czs
                                   ,extensive = c("population")
                                   ,output = "sf" )

poly.pops.nowater$population <- round(poly.pops.nowater$population)

# View & peak
poly.pops.nowater %>% mapview(zcol="population")
poly.pops.nowater$population %>% summary()
poly.pops.nowater$population %>% sort()

# after filter is applied:
tmp <- poly.pops.nowater %>%
  filter(population > 100)
nrow(tmp)
tmp %>% mapview(zcol = "population")

# do they match totals? -------------------------------------------------------------

sum(phcts$population) %>% appHelpers::q.format()

sum(poly.pops$population) %>% appHelpers::q.format()

sum(poly.pops.nowater$population, na.rm = T) %>% appHelpers::q.format()



# automated test ---------------------------------------------------------------

#test_that("population filtering works for polygon measure", {
#  expect_equal(
#    (poly.pops %>% filter(population > 100) %>% nrow()),
#    (divM::Polys.wrapper(phl, hwys, fill.gaps = T,
#                        min.population.count = 100) %>% nrow())
#  )})



# scratch for redeveloping fcn -------------------------------------

poly.pops
tmp <- poly.pops %>% select(-population)

divM::trim.polys.by.pop(tmp, region.id = "19700",
                  return.sf = F)

poly.pops

