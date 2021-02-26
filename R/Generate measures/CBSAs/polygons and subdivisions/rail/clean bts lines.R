library(dplyr)
library(sf)
library(lwgeom)

# bureau of transit statistics rails
raildir <- "~/R/shapefiles/rails/"
bts <- st_read( paste0(raildir,
                       "BTS rail lines/North_American_Rail_Lines.shp") )

# "O" --- non-mainline active track & "Y" for railyard, presumably,
# can be removed for our purposes. "S" is maybe "side" ????? From google maps,
# "T" seems to be transmission lines... that can go too. I think they're relics
# of abandoned rail tracks, where the rail has been buried. Otherwise it's
# marked with "A" for abandoned or "F" is ferry -- which can def go.
# idk what "R" is but from map doesn't seem to be a division
to.remove <- c("O", "S", "T", "F", "R") # , "Y"
keep.cols <- c("OBJECTID", "RROWNER1", "SUBDIV", "TRACKS", "DIRECTION", "NET")
bts <- bts %>% select(all_of(keep.cols))

bts <- bts %>% filter(! NET %in% to.remove)
bts <- bts %>% denode.lines(keep.cols)

st_write(bts,
         paste0(raildir,
                "BTS rail lines/cleaned BTS/cleaned-bts.shp"))

# also exists in db as divs.rails_bts
'source("data-raw/.auth/.auth.R")
con <- dblinkr::db.connect(db.usr, db.pw)
dblinkr::tbls.in.schema(con, "divs")
bts <- dblinkr::q_db2sf(con, "divs.rails.")'
# bts["NET"] %>% plot()
