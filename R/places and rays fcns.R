
# ray generation fcns ---------------------------------------------------------

# these fcns are written as specialized fcns to generate the ray measures. The
# fcn Count.rays wraps the necessary steps and is the only thing that has to be
# called directly to generate.


#' Count.rays
#'
#' given the geoid of a place and sf objects containing hwy information and place
#' information, this calls each step to count the rays. Returns the number of rays
#' for the place and a map of them. Arguments are passed to wrapped fcns. Additional
#' arguments can also be passed to mapview::mapview to change appearance of output
#' map.
#'
#' @param place.geoid 7-character identifier for place to generate rays for.
#' @inheritParams initial.hwy2ray.subset
#' @param place.sf Sf object with `geoid` column to subset to.
#' @param remove.holes Remove holes from places before counting rays. If a hwy
#'   starts/ends in a hole, it will be counted as a ray unless this is set to TRUE
#' @param verbose Display additional text output in console. Makes explicit some
#'   parameters that are passed to wrapped fcns and will say where ineligible rays
#'   are removed due to issue described above.
#'
#' @inheritDotParams trim.to.length.floors
#' @inheritDotParams hwys2endpoints
#' @inheritDotParams initial.hwy2ray.subset
#' @inheritDotParams Get.bundled.ray.output
#'
#' @importFrom nngeo st_remove_holes
#'
#' @export Count.rays
Count.rays <- function(place.geoid,
                       hwy.sf,
                       place.sf = divM::largest.plc.in.cz,
                       remove.holes = FALSE, ...) {
  require(sf)
  require(dplyr)
  # filter to specified place
  place <- place.sf %>% filter(geoid == place.geoid)

  # initial prep / trim hwys to area
  hwy <- suppressMessages( initial.hwy2ray.subset(place, hwy.sf, ...)
                           , classes = c("message", "warning"))

  if(remove.holes)
    place <- nngeo::st_remove_holes(place)

  # pass prepped hwys onto generation fcn
  suppressMessages( Get.bundled.ray.output(place, hwy, ...)
                    , classes = c("message", "warning"))
}


#' initial.hwy2ray.subset
#'
#' Sets up road objects for ray measures. Ensures uniform CRS, trims hwys to those
#' surrounding the Place, and parses arguments around which hwys to include.
#' @param place Single polygon, likely representing a Place, for which to generate
#'   rays.
#' @param hwy.sf Sf object that represents highways with NHPN `SIGNT1` & `SIGN1`
#'   columns
#' @param always.include Type(s) of highways to always include for ray measure if
#'   \code{include.intersecting} is false. By default only interstates
#' @param include.intersecting Whether or not to include additional highway types if
#'   they intersect with one or more of the core/always included hwy types.
#' @param hwy.types Other types of hwys to include if and only if they intersect with
#'   the core, "always.included" type(s). Only relevant if
#'   \code{include.intersecting} is true. No filter applied if left as NULL
#'   (default).
#' @param drop.NA Whether to drop rows with NA in the `SIGNT` hwy-type identifier
#'   column.
#' @param buffer.meters Amount of padding around the Place to retain when trimming
#'   highways to place and surrounding area. Defaults to 300 meters.
#' @export initial.hwy2ray.subset
initial.hwy2ray.subset <- function(place, hwy.sf,
                                   always.include = c("I"),
                                   include.intersecting = FALSE,
                                   hwy.types = NULL,
                                   drop.NA = TRUE,
                                   buffer.meters = 300, ...) {


  # ensure common crs
  hwy <- hwy.sf %>% st_transform(st_crs(place))

  # trim to buffered hull containing place
  hwy <- st_intersection(buffered.hull(place,
                                       buffer = buffer.meters)
                         ,hwy)

  if(drop.NA)
    hwy <- hwy %>% filter(!is.na(SIGN1))

  if(!is.null(hwy.types))
    hwy <- hwy %>% filter(SIGNT1 %in% hwy.types)

  # denode / spatial clean
  hwy <- denode.lines(hwy, group.cols = c("SIGNT1", "SIGN1"))

  # parse include.intersecting and others: ------------

  # core highway types
  if(!is.null(always.include))
    hwyP <- hwy %>% filter(SIGNT1 %in% always.include)
  else
    hwyP <- hwy

  # end if appropriate
  if(!include.intersecting)
    return(hwyP)

  # What is not already included?
  rt <- hwy %>% filter( !SIGNT1 %in% always.include )

  # check if they intersect
  touches.core <- st_filter(rt, hwyP)

  # get routes that "touch core"
  touches.core <- rt %>% filter(SIGN1 %in% touches.core$SIGN1)

  # add to prepped if any found
  if(nrow(touches.core) != 0)
    out <- rbind(hwyP, touches.core)
  else
    out <- hwyP

  # return ------------------------------------------------
  return(out)
}



#' trim.to.length.floors
#'
#' Trims a set of highways to those for which the whole stretch of highway inside the
#' region has minimum length \code{length.floor} in meters. 1 km by default.
#' @param minimum.segment.length Minimum length (meters) that each highway ~segment~
#'   must have in order to be eligible for rays, This differs from
#'   `minimum.hwy.length` in that it filters by each separate segment, rather
#'   than overall hwy length
#' @param minimum.hwy.length Minimum length of each ~highway~ that must be inside
#'   place boundaries for it to be ray-eligible. Differs from
#'   `minimum.segment.length` in that at least one segment of hwy must meet this
#'   threshold for ~any~ of the segments to be eligible. Only relevant if larger than
#'   `minimum.segment.length`.
trim.to.length.floors <- function(region, divisions,
                                 minimum.segment.length = 10,
                                 minimum.hwy.length = 1000, ...) {
  require(lwgeom)

  # add/reset division id column
  divisions$id <- 1:nrow(divisions)

  # get portions of supplied hwys contained in region. Use 0 buffer trick so it
  # doesn't fail for weird-shaped/gerrymandered places.
  div.in.region <- st_intersection(divisions,
                                   st_buffer(region, 0)) %>%
    mutate(length.in.area =
             as.numeric(geod.length(.)))

  # get hwys and segment IDs that exceed floors
  hwys.to.keep <- div.in.region %>%
    filter(length.in.area >= minimum.hwy.length) %>%
    pull(SIGN1)

  segments.to.keep <- div.in.region %>%
    filter(length.in.area >= minimum.segment.length) %>%
    pull(id)

  # filter to eligible hwys/segments
  divisions <- divisions %>%
    filter(SIGN1 %in% hwys.to.keep &
             id %in% segments.to.keep)

  return(divisions)
}


#' hwys2endpoints
#'
#' From the branch in above fcn where it checks hwy types to include, this
#' generates ray-constituting nodes. Can filter out very small segments; by
#' default removes those w/ less than .5km in place boundary
#' @inheritParams Count.rays
#' @param fill.gaps whether or not to fill gaps between highway segments. Argument
#'   \code{threshold} can also be supplied specify maximum gap distance to fill gap
#'   btwn.
#' @inheritDotParams trim.to.length.floors
#' @inheritDotParams fill.gaps
hwys2endpoints <- function(place, trimmed.hwys,
                           fill.gaps = T,
                           verbose = T, ...) {

  if (nrow(trimmed.hwys) == 0)
    return(NULL)

  # denode / spatial clean. kinda a redundant call but, really hate those extra nodes
  hwys <- denode.lines(trimmed.hwys, group.cols = c("SIGNT1", "SIGN1"))

  if(fill.gaps)
    hwys <- Fix.all.hwys(hwys, ...) # code for this is with spatial clean fcns

  # filter to length by hwy/segment
  hwys <- trim.to.length.floors(place, hwys, ...)

  # find nodes and count based on coverage
  hw.nodes <- find.endpoint.nodes(hwys)

  if(is.null(hw.nodes) ||
     nrow(hw.nodes) == 0)
    return(NULL)

  # check coverage. do tiny negative buffer for international border issue.
  coverage <- st_covered_by(hw.nodes,
                            st_buffer(place, -10))

  # rays are recognized when a start/endpoint of a hwy segment lies outside the
  # place boundary
  hw.nodes$coverd = as.logical(lengths(coverage))
  hw.nodes$outside = !hw.nodes$coverd

  return(hw.nodes)
}




#' Get.bundled.ray.output
#'
#' Calls \code{hwys2endpoints}, which finds all line start/endpoints t whether each
#' constitutes a ray. This function counts rays up and bundles with a map, if
#' \code{include.map} is TRUE
#' @inheritParams Count.rays
#' @param trimmed.hwys hwys prepped for supplied place, as with
#'   \code{initial.hwy2ray.subset}
#' @param ... additional parameters passed onto mapview, if map is being returned.
#' @export
Get.bundled.ray.output <- function(place, trimmed.hwys, include.map = TRUE, ...) {

  # fcn if no rays are found-- multiple lines so map can still be outputted
  return.null.early <- function(ray.nodes, include.map, trimmed.hwys) {
    if(include.map) {
      out$map <- mapview::mapview(st_boundary(place), color = "#800030")
      if(nrow(trimmed.hwys) > 0)
        out$map <- out$map + mapview::mapview(trimmed.hwys, zcol = "SIGN1")
    }
    out$n.rays <- 0
    return(out)
  }

  # ID ray nodes -----------------------------------------------------------------

  # prep returned object to contain both ray count & mapview object.
  out <- list()

  # this finds all line start/endpoints and identifies whether each constitutes a ray
  hw.nodes <- hwys2endpoints(place, trimmed.hwys, ...)

  # are any of them outside Place area (ray-constituting)?
  hw.nodes <- hw.nodes[hw.nodes$outside, ]

  # If no ray nodes found, return 0
  if( is.null(hw.nodes) || nrow(hw.nodes) == 0)
    return(return.null.early(hw.nodes, include.map, trimmed.hwys))

  # otherwise, organize output
  hw.nodes$n <- factor(1:nrow(hw.nodes))

  node.counts <- hw.nodes %>%
    group_by(SIGNT1, SIGN1) %>%
    summarise(endpoints.outside = sum(outside))

  # sum nodes outside of coverage area & return
  out$n.rays <- sum(node.counts$endpoints.outside)

  if(include.map) {
    out$map <-
      mapview::mapview(st_jitter(hw.nodes), zcol = "SIGN1",
                       cex = 11.5, color = "#e042f5"
                       #,col.regions = RColorBrewer::brewer.pal(8, "Dark2")
      ) +
      mapview::mapview(st_boundary(place), color = "#800030"
                       #,alpha.regions = .2
      ) +
      mapview::mapview(trimmed.hwys, lwd = 3.5, zcol = "SIGN1", ...)
  }
  return(out)
}


# tests / sample calls / debugs -------------------------------------------

'
pvd  <- plc[plc$geoid == plc.ids["Providence"], ]
pvd.hwy <- initial.hwy2ray.subset(pvd, hwys)

mapview(st_boundary(pvd), color = "#800000") +
  mapview(pvd.hwy, lwd = 2) +
  mapview(filter(st_intersection(hwys,
                          pvd),
                 SIGNT1 == "I")
          , color = "#008020")

# this finds all line start/endpoints and identifies whether each constitutes a ray
hw.nodes <-
  hwys2endpoints(pvd, pvd.hwy)

mapview(st_boundary(pvd), color = "#800000") +
  mapview(pvd.hwy, lwd = 2) +
  mapview(hw.nodes)

# test run ---------------------------------------------------------------------

Count.rays(plc.ids["Providence"],
           hwys
           ,plc
           ,min.segment.length = 10
           ,include.map = T
           ,verbose = T)

'
'
tmp  <- plc[plc$geoid == plc.ids["Ogallala"], ]
tmp.hwy <- initial.hwy2ray.subset(tmp, hwys,
                                  include.intersecting = T)
library(mapview)
mapview(st_boundary(tmp), color = "#800000") +
  mapview(tmp.hwy, lwd = 5)

#+
  mapview(filter(st_intersection(hwys,
                                 tmp))
          , zcol="SIGNT1")
'
