
#' largest.centers.in.region
#'
#' Takes population centers, represented as a shapefile (probably of places or
#' sub-county divisions), with a column for population. For each larger region
#' in the second argument, finds the largest population center of that larger area.
#' Note do not include duplicate columns in the two supplied dataframes.
#' Returns the largest population center in each larger region as a dataframe with point geometries
#' @export
largest.centers.in.region <- function(centers, region) {
  # first puts in conic projection
  centers <- conic.transform(centers)
  region <- conic.transform(region)

  # then, to account for different resolutions, clipping, irregular shapes, etc.
  # it gets a "centroid on surface" for smaller areas to spatial join by.
  centers <- st_point_on_surface(centers)

  # joins with larger areas, groups by larger areas, and filters to largest
  # small area by population w/ each larger area
  out <- st_join(centers,
                 region) %>%
    group_by_at(all_of( setdiff(colnames(region), "geometry") )) %>%
    filter(population == max(population, na.rm = T)) %>%
    ungroup()
  return(out)
}

# ray generation fcns ---------------------------------------------------------

# these fcns are written as specialized fcns to generate the ray measures. The
# fcn count.rays wraps the necessary steps and is the only thing that has to be
# called directly to generate.



#' count.rays
#'
#' given the GEOID of a place and sf objects containing hwy information and
#' place information, this calls each step to count the rays. Returns the number
#' of rays for the place and a map of them. Arguments are passed to wrapped
#' fcns. Additional arguments can also be passed to mapview::mapview to change appearance
#' of output map.
#' @param always.include Type(s) of highways to always include for ray measure.
#'   By default only interstates
#' @param include.intersecting Whether or not to include additional highway
#'   types if they intersect with one or more of the core/always included hwy
#'   types.
#' @param hwy.types Other types of hwys to include if and only if they intersect
#'   with the core, "always.included" type(s). Includes all non-NA routes if
#'   NULL (default). Only relevant if \code{include.intersecting} is true.
#' @param buffer.meters Amount of padding around the Place to retain when
#'   trimming highways to place and surrounding area. Defaults to 300 meters.
#' @param length.floor Filters highways by route if the total length of all
#'   portions of the highway within the area is below this threshold. Default is
#'   1 km.
#' @param min.segment.length Filters highway segments by length. I.e., if a
#'   highway route branches into multiple segments, this will filter segments
#'   that have less than this threshold within the area. Default is 500m.
#' @param ray.node.distance.threshold Sometimes there is a small gap in a
#'   highway at an interchange or something. If this happens outside of a city,
#'   we should make sure both sides of the gap aren't counted as extra rays.
#'   This defines the minimum distance between segment endpoints that, when they
#'   belong to the some route, would mean that neither is counted as a ray.
#'   Defaults to 100m. If it's NULL, skips this step.
#' @param remove.holes Remove holes from places before counting rays. If a hwy
#'   starts/ends in a hole, it will be counted as a ray unless this is set to
#'   TRUE
#' @param filter.colinear.node.threshold if this is not NULL (default), remove
#'   ray nodes in given threshold proximity of one another. May be useful if two
#'   highways are colinear or parallel and nearby and you don't want to count
#'   multiple rays.
#' @param include.map whether or not to generate a map along with the measure.
#'   Defaults to TRUE; set to FALSE to save time.
#' @param verbose Display additional text output in console. Makes explicit some
#'   parameters that are passed to wrapped fcns and will say where ineligible
#'   rays are removed due to issue described above.
#' @export
count.rays <- function(place.geoid, place.sf, hwy.sf, remove.holes = FALSE, ...) {
  require(sf)
  require(dplyr)
  # filter to specified place
  place <- place.sf %>% filter(GEOID == place.geoid)

  # initial prep / trim hwys to area
  hwy <- suppressMessages( initial.hwy2ray.subset(place, hwy.sf, ...)
                           , classes = c("message", "warning"))

  if(remove.holes)
    place <- nngeo::st_remove_holes(place)

  # pass prepped hwys onto generation fcn
  suppressMessages( prepped.hwys.to.rays(place, hwy, ...)
                    , classes = c("message", "warning"))
}


#' initial.hwy2ray.subset
#'
#' Sets up road objects for ray measures. Takes intersection of hwys with the
#' place's buffered convex hull, trims NA routes and aggregates by type, trims to length
#' floor, so each route must have a minimum distance within place boundary.
#' @export
initial.hwy2ray.subset <- function(place, hwy.sf, hwy.types = NULL, buffer.meters = 300, ...) {
  # ensure common crs
  hwy <- hwy.sf %>% st_transform(st_crs(place))
  # trim to buffered hull containing place
  hwy <- st_intersection(buffered.hull(place, buffer = buffer.meters)
                         ,hwy)
  # filter to appropriate hwy types & union by route/type
  hwy <- hwy %>% filter(!is.na(SIGNT1))
  if(!is.null(hwy.types))
    hwy <- hwy %>% filter(SIGNT1 %in% hwy.types)
  hwy <- hwy %>% count(SIGNT1, SIGN1)

  # trim to length floor by hwy -- this fcn is called again to trim by segment.
  # I think very few cases where this first call would make a difference (and it's ambiguous which one would be correct)
  hwy <- trim.to.length.floor(place, hwy, ...)

  return(hwy)
}



#' trim.to.length.floor
#'
#' Trims a set of highways to those for which the whole stretch of highway
#' inside the region has minimum length \code{length.floor} in meters. 1 km by
#' default.
#' @param id.col specifies what column must have minimum \code{length.floor}
#'   within the region. I.e., it should be SIGN1 when filtering out grouped sf by routes
#'   and it should be "id" if filtering exploded sf by segments.
trim.to.length.floor <- function(region, divisions, length.floor = 1000, id.col = "SIGN1", verbose = T, ...) {
  require(lwgeom)
  if(verbose)
    cat("Trimming to hwys w/ minimum length of", length.floor, "in", region$NAME,"\n")

  boundary <- st_boundary(region)

  # get hwys in region. 0 buffer stops potential topology exceptions for gerrymandered places
  div.in.region <- st_intersection(divisions,
                                   st_buffer(region, 0))

  length.floor <- units::set_units(length.floor, "m")

  to.keep <- div.in.region %>%
    mutate(length.in.area = geod.length(.)) %>%
    filter(length.in.area >= length.floor) %>%
    pull(!!id.col)

  # filter original divisions to those that weren't filtered outc
  # (Unlike intersections, this keep parts outside of boundary)
  out <- divisions %>% filter(!!rlang::sym(id.col) %in% to.keep)
  return(out)
}


#' prepped.hwys.to.rays
#'
#' Does most of the work to generate rays. Wrapped by count.rays but it's kept
#' separate so it can be called more quickly with just a place boundary and
#' prepped hwy sf. Uses just hwy types included in "always.include" or also
#' includes add'l hwys that intersect those core hwy types.
#' @export
prepped.hwys.to.rays <- function(place, hwy, always.include = c("I"), include.intersecting = FALSE, include.map = TRUE, ...) {

  return.null.early <- function(ray.nodes, include.map, trimmed.hwys) {
    if(include.map) {
      out$map <- mapview::mapview(st_boundary(place), color = "#800030")
      if(nrow(trimmed.hwys)>0)
        out$map <- out$map + mapview::mapview(trimmed.hwys, zcol = "SIGN1")
    }
    out$n.rays <- 0
    return(out)
  }

  # Setup include.intersecting --------------------------------
  # split by road type to allow the two versions of the measure
  int <- hwy %>% filter(SIGNT1 %in% always.include)
  if(!include.intersecting) {
    trimmed.hwys <- int
  } else {
    rt <- hwy %>% filter(!SIGNT1 %in% always.include)

    sgbp <- st_intersects(rt, int)
    touches.int <- rt[ lengths(sgbp) > 0, ]

    if(nrow(touches.int) == 0)
      trimmed.hwys <- int
    else
      trimmed.hwys <- rbind(int,touches.int)
  }
  # hwys2ray.nodes call -----------------------------------------------------
  ray.nodes <- hwys2ray.nodes(place, trimmed.hwys, ...)

  # prep returned object ---------------------------------------------------
  # both ray count & mapview object.
  out <- list()
  # set to 0 if NULL (no eligible hwys in area)
  if( is.null(ray.nodes) )
    return(return.null.early(ray.nodes, include.map, trimmed.hwys))
  # do the work if not NULL
  ray.nodes <- ray.nodes[ray.nodes$outside, ]
  ray.nodes <- filter.ineligible.ray.nodes(place, ray.nodes, ...)
  ray.nodes <- filter.colinear.nodes(place, ray.nodes, ...)
  if( is.null(ray.nodes) ) #(filtering nodes may make null again esp. for interstate plan..)
    return(return.null.early(ray.nodes, include.map, trimmed.hwys))

  ray.nodes$n <- factor(1:nrow(ray.nodes))

  node.counts <- ray.nodes %>%
    group_by(SIGNT1, SIGN1) %>%
    summarise(endpoints.outside = sum(outside))

  # sum nodes outside of coverage area & return
  out$n.rays <- sum(node.counts$endpoints.outside)

  if(include.map) {
    out$map <-
      mapview::mapview(st_jitter(ray.nodes), zcol = "SIGN1",
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


#' hwys2ray.nodes
#'
#' From the branch in above fcn where it checks hwy types to include, this
#' generates ray-constituting nodes. Can filter out very small segments; by
#' default removes those w/ less than .5km in place boundary
hwys2ray.nodes <- function(boundary, trimmed.hwys, min.segment.length = 10, verbose = T, ...) {

  # denode / spatial clean
  hwys <- denode.lines(trimmed.hwys)
  if (nrow(hwys) == 0)
    return(NULL)

  # filter to length by segment
  hwys$segment.len <- geod.length(hwys) # (hwys is exploded from denode.lines)
  if (min.segment.length > 0)
    hwys <- trim.to.length.floor(boundary, hwys, min.segment.length, id.col = "id")

  # find nodes and count based on coverage
  hw.nodes <- find.endpoint.nodes(hwys)

  # check coverage. does tiny minimum buffer for international border issue. (Need to check this!)
  coverage <- st_covered_by(hw.nodes,
                            st_buffer(boundary, -10))

  hw.nodes$coverd = as.logical(lengths(coverage))
  hw.nodes$outside = !hw.nodes$coverd

  return(hw.nodes)
}



#' filter.ineligible.ray.nodes
#'
#' Sometimes there is a break or gap in a hwy route just outside of a city,
#' causing direct endpoint analysis to count extra rays. This filters out ray
#' nodes if they belong to the same highway route and are within a distance
#' threshold of each other. #(realizing i probably made this redundant w/ the filtering segments by length floor)
filter.ineligible.ray.nodes <- function(place, potential.ray.nodes, ray.node.distance.threshold = 100, verbose = F, ...) {

  if(is.null(ray.node.distance.threshold)) return(potential.ray.nodes)

  # Split by route and filter out nodes if they're in extremely close proximity to another of same route
  pl.centroid <- geosphere::centroid(st_coordinates(st_transform(place,4326))[,c("X","Y")])
  split.potential.ray.nodes <-
    potential.ray.nodes %>%
    local_equidistant_project(projection_center = pl.centroid) %>%
    st_buffer(ray.node.distance.threshold) %>%
    split(.$SIGN1)

  # (objects are split by hwy route; map through and find which hwy routes have ineligible nodes)
  sgbpL <- purrr::imap( split.potential.ray.nodes, ~ st_intersects(.) )

  ineligible.ray.node.index <-
    sgbpL %>%
    purrr::imap( ~ lengths(.) > 1)

  if(verbose) {
    message = ineligible.ray.node.index %>% imap( ~sum(.))
    message = message[message > 0]
    if(length(message) > 0)
      cat("Removing ineligible nodes from",place$NAME,"\n",
          names(message),"had",unlist(message),"ineligible nodes.\n")
  }
  # index potential ray nodes and filter out ineligible ones, then collapse
  filtered.ray.nodes <-
    purrr::map2_dfr(split.potential.ray.nodes, ineligible.ray.node.index,
         ~`[`(.x , !.y, ))

  return(filtered.ray.nodes)
}

#' filter.colinear.nodes
#' Runs if \code{filter.colinear.node.threshold} is set to not NULL, in which
#' case it removes ray nodes in given threshold proximity of one another. In
#' general, this should be left as NULL when using NHPN data.
filter.colinear.nodes <- function(place, potential.ray.nodes, filter.colinear.node.threshold = NULL, verbose = F, ...) {

  # skip filter if appropriate
  if(is.null(filter.colinear.node.threshold)
     | is.null(potential.ray.nodes)) return(potential.ray.nodes)

  # otherwise find those w/in distance threshold and filter
  sgbp <- potential.ray.nodes %>% st_buffer(filter.colinear.node.threshold) %>% st_intersects()

  eligible.ray.nodes <- potential.ray.nodes[!duplicated(sgbp),]
  return(eligible.ray.nodes)
}

# tests / sample calls / debugs -------------------------------------------
'PA.rays <-
  imap( plc.ids,
        ~ count.rays(., plc, hwys,
                     verbose = T))'

#count.rays("3728000", plc, hwys, #(Greensboro)
#           include.intersecting = T,
#           verbose = T)

'
plc %>%
  filter(STATEFP == 25)
tmp <- plc %>%
  filter(GEOID %in% "2553960")

pfhw <- initial.hwy2ray.subset(tmp, hwys)
mapview(pfhw ,zcol= "SIGN1")

# pittsfield
#count.rays("2553960", plc, hwys, return.map = T, verbose = F)
#prepped.hwys.to.rays(tmp, pfhw)
# boston
count.rays("2507000", plc, hwys, return.map = T, verbose = T, include.intersecting = T, hwy.types = c("I", "U","S"))



plc %>% filter(STATEFP == "25")
count.rays("2507000", plc, hwys, verbose = F)

count.rays("2553960", plc, hwys, verbose = F)

# springfield
count.rays("2567000", plc, hwys, verbose = F)
count.rays("2567000", plc, hwys, include.intersecting = T)

tmp <- plc %>%
  filter(GEOID %in% "2567000")

pfs <- st_intersection(hwys, buffered.hull(tmp))
mapview(pfs ,zcol= "SIGN1") +
  mapview(st_boundary(tmp))
'
'
# U29 and... I85?
count.rays(plc.ids["Greensboro"],
           plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= 100#NULL
           )
'

'

#count.rays("0455000", plc, intpl, always.include = "plan", min.segment.length = 300) # Phoenix
count.rays("0177256", plc, intpl
           , always.include = "plan"
           , min.segment.length = 300
           , filter.colinear.node.threshold = 100) # tuscaloosa--- have to filter rays on colinear segments -check

#plc.planned.intst[plc.planned.intst$GEOID == "0177256",]
'

'count.rays(plc.ids["Roanoke"],
           plc, intpl
           ,always.include = "plan"
           ,min.segment.length = 300
           , filter.colinear.node.threshold = 100)'
