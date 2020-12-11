rm(list = ls())
library(sf)
library(dplyr)
library(lwgeom)
library(mapview)
# load divM
devtools::load_all()

# get CZs -------------------------------------------------------------------
czs <- divDat::czs %>% divM::re


# load administrative boundaries -------------------------------------
counties <- divDat::counties
sds <- divDat::school.dists
plc <- divDat::plc

colnames.tolower <- function(x) {
  colnames(x) <- tolower(colnames(x))
  return(x)
}
admin.geos = list(  "county" = counties
                    , "place" = plc
                    , "school.dist" = sds)

admin.geos <- purrr::imap( admin.geos,
                           ~colnames.tolower(.))

# trim divs to sample areas
# sample.states = c("36")
#sample.admins <- admin.geos %>%
#  imap( ~filter(., statefp %in% sample.states))


# dissolve to boundaries (Turn polygons into linear boundaries)
sample.admins <- map2( sample.admins
                       ,names(sample.admins)
                       , ~divM::dissolve.to.boundaries.w.ID(.x, .y, T)
)
sample.admins <- do.call("rbind", sample.admins)
# View
plot(sample.admins['dtype'], lwd = 2, bg = "#444444")


# get phys divisions (-water) --------------------------------------------------
# (get divs from database)
source(".auth/.auth.R") # credentials for db not sent to github
con <- dblinkr::db.connect(db.usr, db.pw)
dblinkr::tbls.in.schema(con, "divs")

hwys <- dblinkr::query.division(con, tmp.czs, "divs.hwys")
rails <- dblinkr::query.division(con, tmp.czs, "divs.rails_bts")
rails.nona = rails %>% filter(!is.na(DIRECTION))

# Just clean all hwys outside of fcn (for example filling hwy gaps), we can do that
# here (may have to do split or for loop structure to deal w/ memory limit)
hwys <- divM::Fix.all.hwys(hwys, verbose = F)


# put these into the same format as the administrative boundares.
phys.divs <- list( "rails" = rails.nona
                   ,"hwys" = hwys
)

phys.divs <- phys.divs %>%
  imap( ~divFcns::conic.transform(.) )

phys.divs <-
  phys.divs %>%
  imap( ~pull(., geometry) ) %>%
  map2( names(phys.divs),
        ~st_sf(  dtype = .y
                 , admin = F
                 , geometry = .x )
  )
phys.divs <- do.call('rbind', phys.divs)

# combine into single sf
all.divs <- rbind(phys.divs,
                  sample.admins)

# check
all.divs %>% tibble() %>% count(dtype)

# cut out water areas ----------------------------------------------------------

# has to happen w/in fcn to deal with memory limit-- or can generate and save in
# separate script

czs.no.water <-
  purrr::map_dfr(
    split(tmp.czs, tmp.czs$region.id),
    ~divM::db.query.and.cutout.water(con, .,
                                     trim.unnamed = T, trim.by.area = 2e6))



# final steps and generate -----------------------------------------------------

spf.divs <- st_intersection( all.divs,
                             st_buffer(spf, 100) )
spf.divs <- st_collection_extract(spf.divs
                                  , "LINESTRING")

spf.polys <- polygonal.div(   spf
                              ,spf.divs
                              ,return.sf = T
                              ,min.size = 1e4 # m^2 (very small)
                              ,min.population.count = 0 # for illustration
)

# area summaries.. -------------------------------------------------------------

spf.polys$area <- st_area(spf.polys$geometry)
plot(spf.polys['id'], main = "Springfield polygons")

# summaries of sizes/interpolated populations for each sub-polygon
spf.polys[,c("population", "pop.perc", "area")] %>% summary()
# population
quantile(spf.polys$population, seq(0,1, .1)  )
# area
quantile(spf.polys$area, seq(0,1, .1)  )
nrow(spf.polys) # total polygonal divisions (given parameters)


# write ------------------------------------------------------------------------



