
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
#' @param always.include Type(s) of highways to always include for ray measure if
#'   \code{include.intersecting} is false. By default only interstates
#' @param include.intersecting Whether or not to include additional highway types if
#'   they intersect with one or more of the core/always included hwy types.
#' @param hwy.types Other types of hwys to include if and only if they intersect with
#'   the core, "always.included" type(s). Only relevant if
#'   \code{include.intersecting} is true. No filter applied if left as NULL
#'   (default).
#' @param buffer.meters Amount of padding around the Place to retain when trimming
#'   highways to place and surrounding area. Defaults to 300 meters.
#' @param minimum.segment.length Minimum length (meters) that each highway ~segment~
#'   must have in order to be eligible for rays, This differs from
#'   \code{minimum.hwy.length} in that it counts each separate
#' @param minimum.hwy.length Minimum length of each ~highway~ that must be inside
#'   place boundaries for it to be ray-eligible. Differs from minimum.segment.length
#'   in that at least one segment of hwy must meet this threshold for ~any~ of the
#'   segments to be eligible. Only relevant if larger than minimum.segment.length and
#' @param fill.gaps whether or not to fill gaps between highway segments. Argument
#'   \code{threshold} can also be supplied specify maximum gap distance to fill gap
#'   btwn.
#' @param remove.holes Remove holes from places before counting rays. If a hwy
#'   starts/ends in a hole, it will be counted as a ray unless this is set to TRUE
#' @param verbose Display additional text output in console. Makes explicit some
#'   parameters that are passed to wrapped fcns and will say where ineligible rays
#'   are removed due to issue described above.
#' @inheritDotParams
#' @importFrom nngeo st_remove_holes
#' @export
Count.rays <- function(place.geoid, hwy.sf, remove.holes = FALSE,
                       place.sf = largest.plc.in.cz, ...) {
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
#' @inheritParams Count.rays
#' @export
initial.hwy2ray.subset <- function(place, hwy.sf, always.include = c("I"),
                                   include.intersecting = FALSE,
                                   hwy.types = NULL,
                                   drop.NA = TRUE,
                                   buffer.meters = 300, ...) {

  # ensure common crs
  hwy <- hwy.sf %>% st_transform(st_crs(place))

  # trim to buffered hull containing place
  hwy <- st_intersection(buffered.hull(place, buffer = buffer.meters)
                         ,hwy)

  if(drop.NA)
    hwy <- hwy %>% filter(!is.na(SIGN1))

  if(!is.null(hwy.types))
    hwy <- hwy %>% filter(SIGNT1 %in% hwy.types)


  # parse include.intersecting and others: ------------

  # core highway types
  if(!is.null(always.include))
    hwy <- hwy %>% filter(SIGNT1 %in% always.include)

  # end if appropriate
  if(!include.intersecting)
    return(hwy)

  # What is not already included?
  rt <- hwy %>% filter( !SIGNT1 %in% always.include )

  # check if they intersect
  sgbp <- st_intersects(rt, hwy)
  touches.core <- rt[ lengths(sgbp) > 0, ]

  # add to prepped if any found
  if(nrow(touches.core) != 0)
    hwy <- rbind(hwy, touches.core)

  # return ------------------------------------------------
  return(hwy)
}



#' trim.to.length.floors
#'
#' Trims a set of highways to those for which the whole stretch of highway inside the
#' region has minimum length \code{length.floor} in meters. 1 km by default.
#' @inheritParams Count.rays
trim.to.length.floors <- function(region, divisions,
                                 minimum.segment.length = 10,
                                 minimum.hwy.length = 1000, ...) {
  require(lwgeom)

  # add/reset division id column
  divisions$id <- 1:nrow(divisions)

  # get portions of supplied hwys contained in region. Use 0 buffer trick so it doesn't fail for
  # weird-shaped/gerrymandered places.
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
#' @param
#' @inheritParams Count.rays
#' @inheritDotParams trim.to.length.floors
#' @inheritDotParams fill.gaps
#' @param ...
hwys2endpoints <- function(place, trimmed.hwys,
                           fill.gaps = T,
                           verbose = T, ...) {

  if (nrow(hwys) == 0)
    return(NULL)

  # denode / spatial clean
  hwys <- denode.lines(trimmed.hwys)

  if(fill.gaps)
    hwys <- Fix.all.hwys(hwys, ...) # code for this is with polygon fcns

  # filter to length by hwy/segment
  hwys <- trim.to.length.floors(place, hwys, ...)

  # find nodes and count based on coverage
  hw.nodes <- find.endpoint.nodes(hwys)

  # check coverage. do tiny negative buffer for international border issue. (Need to check this!)
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

  # this finds all line start/endpoints and identifies whether each constitutes a ray
  hw.nodes <- hwys2endpoints(place, trimmed.hwys, ...)

  # prep returned object ---------------------------------------------------

  # both ray count & mapview object.
  out <- list()

  # If no hw nodes found, return 0
  if( is.null(hw.nodes) )
    return(return.null.early(hw.nodes, include.map, trimmed.hwys))

  # do the work if not NULL
  hw.nodes <- hw.nodes[hw.nodes$outside, ]


  if( is.null(hw.nodes) ) #(filtering nodes may make null again esp. for interstate plan..)
    return(return.null.early(hw.nodes, include.map, trimmed.hwys))

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

