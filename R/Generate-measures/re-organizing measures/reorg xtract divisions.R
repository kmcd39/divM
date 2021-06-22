#' I used different code for water vs. other cross-tract division measures, and
#' generated them in slightly different formats.
#'
#' The scripts to generate those things for all non-water divisions generate
#' files separated by area, wide by division type.
#'
#' For water, there is one file for each area type and only the one division
#' type.
#'
#' This reshapes the as-generated output to be uniform/tidy: One file for
#' everything, long by division type and region type (cz/cbsa)


# load cbas ---------------------------------------------------------

xct.dir <- "/scratch/gpfs/km31/dividedness-measures/tract-level/"

cbsa.wtr <-
  list.files(paste0(xct.dir,"by-cbsa/"),
             full.names = T,
             pattern = "water.*csv") %>%
  vroom::vroom(col_types =
                 cols(geoid = "c",
                      cbsa = "c"))

cbsa.wtr

cbsa.divs <-
  list.files(paste0(xct.dir,"by-cbsa/"),
             full.names = T,
             pattern = "^cbsa") %>%
  #`[`(2) %>%
  map_dfr(
    ~{tibble(cbsa =  # my hack-y way of using mapped object for row identifiers
               str_extract(.x, "[0-9]{5}"),
             vroom::vroom(.x,
                          col_types =
                            cols(geoid = "c")))
      })

cbsa.divs %>% count(cbsa)
cbsa.divs


# org cbsas ---------------------------------------------------------------

cbsa.wtr$geoid <- cbsa.wtr$geoid %>% geoseg::fix.geoid()
cbsa.divs$geoid <- cbsa.divs$geoid %>% geoseg::fix.geoid()


cbsa.all <-
  cbsa.wtr %>%
  select(geoid, cbsa,
         wtr.poly = poly.id) %>%
  left_join(cbsa.divs)

# (i left off some cbsas that have no interstate/hwy intersection for non-water
# divisons)
cbsa.all %>% map_dbl( ~sum(is.na(.)))
cbsa.all[1000:1010,]

# load cz ---------------------------------------------------------

cz.wtr <-
  list.files(paste0(xct.dir,"by-cz/"),
             full.names = T,
             pattern = "water.*csv") %>%
  vroom::vroom(col_types =
                 cols(geoid = "c",
                      cz = "c"))

cz.wtr

cz.divs <-
  list.files(paste0(xct.dir,"by-cz/"),
             full.names = T,
             pattern = "^cz") %>%
  #`[`(2) %>%
  map_dfr(
    ~{tibble(cz =  # my hack-y way of using mapped object for row identifiers
               str_extract(.x, "[0-9]{5}"),
             vroom::vroom(.x,
                          col_types =
                            cols(geoid = "c")))
    })

cz.wtr %>% count(cz)
cz.divs %>% count(cz)
cz.divs

# org czs ---------------------------------------------------------------

cz.wtr$geoid <- cz.wtr$geoid %>% geoseg::fix.geoid()
cz.divs$geoid <- cz.divs$geoid %>% geoseg::fix.geoid()


cz.all <-
  cz.wtr %>%
  select(geoid, cz,
         wtr.poly = poly.id) %>%
  left_join(cz.divs)

# (i left off some czs that have no interstate/hwy intersection for non-water
# divisons)
cz.all %>% map_dbl( ~sum(is.na(.)))
cz.all[1000:1010,]


# long by region type -----------------------------------------------------

cz.all <- cz.all %>% geoseg::region.reorg("cz")
cbsa.all <- cbsa.all %>% geoseg::region.reorg("cbsa")

colnames(cz.all) == colnames(cbsa.all)
# hwys for one and ints.and.us for the other.. I want to standardize to the
# latter, will keep both for now and make long


divs <- list(cz = cz.all, cbsa = cbsa.all)
divs <- divs %>%
  map( ~rename_with(.,
                    ~gsub("school.dist",
                          "school_dist", .x)))

reorg.long <- function(x) {

  x %>%
    select(1:3, matches("poly$")) %>%
    pivot_longer(cols = matches("poly$"),
                 names_to = "div.type",
                 values_to = "poly.id") %>%
    mutate(div.type =
             gsub("\\.poly", "", div.type))

}

divs <- divs %>%
  map( reorg.long )

divs <- divs %>% do.call("rbind", .)

# check ;p
divs %>%
  select(1,2) %>%
  distinct() %>%
  count(region.type)

divs %>%
  map_dbl( ~sum(is.na(.)))
# i think a few may be missing besides those that I didn't generate for... but
# not sure.


# write -------------------------------------------------------------------

# to dividedness measures - will also leave in dropbox w/ more prepped scripts
write.csv(divs,
          file = "/scratch/gpfs/km31/dividedness-measures/tract-level/all-xtract-divs.csv")
