

#' absolute.validate
#'
#' This cycles through all the "weird tricks" I've found to ensure that
#' geometries are not only valid, but remain so after st_intersection or other
#' manipulations. Buffer 0 trick is useful for these case but can alter
#' geometry: https://github.com/r-spatial/sf/issues/347
#' @export
absolute.validate <- function(x) {
  require(sf)
  require(lwgeom)
  x <- x %>% st_set_precision(1e5)
  x <- st_make_valid(x)
  x <- st_buffer(x, 0)

  return(x)
}

# nodes ---------------------------------------------------------------

#' denode.lines
#'
#' Dissolves constitutive line segments into longer lines wherever possible.
#' Specify in \code{group.cols} which columns should be kept in output. Workflow
#' of function is: Union -> merge -> explode -> add ids.
#' @export
denode.lines <- function(x, group.cols = c("SIGNT1", "SIGN1")) {

  if (nrow(x) == 0) return(x)

  # union/merge -- dissolve all differences except due to grouping colms
  if(!is.null(group.cols))
    grpd.x <- x %>%
      group_by_at(vars(all_of(group.cols))) %>%
      summarise(., do_union = TRUE)
  else
    grpd.x <- x %>% st_union() %>% st_sf()


  xploded.x <- x %>%
    st_cast("LINESTRING")

  # sometimes add'l linemerging requied (i forget exactly logic, but remember there
  # was a weird case to catch; should've documented better then)
  if (nrow(grpd.x) == nrow(xploded.x)) {
    x$id = 1:nrow(x)
    return(x)
  } else
    grpd.x %>%
    ungroup() %>%
    st_cast("MULTILINESTRING") %>%
    st_line_merge() %>%
    st_collection_extract("LINESTRING") %>%
    st_cast("LINESTRING") %>%
    mutate(id = row_number())
}



#' find.endpoint.nodes
#'
#' Gets all endpoints of all lines of a LINESTRING sf object. Returns an sf object
#' with points representing nodes, with a column "n" representing the number of
#' line segments that start or end with that node.
#' Requires an "id" column in segment sf.
#' adopts workflow from https://www.r-spatial.org/r/2019/09/26/spatial-networks.html
#' @export
find.endpoint.nodes <- function(x, line.ids = c("SIGNT1", "SIGN1")) {

  # no endpoints if no lines
  if(nrow(x) == 0)
    return(NULL)
  # ensure id columns in supplied sf
  if(!all(line.ids %in% colnames(x)))
    stop("missing id columns")

  # add id cols to linestrings
  x$id <- 1:nrow(x)

  # get start/endpoints
  nodes <-
    x %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edge.id = L1) %>%
    group_by(edge.id) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2))

  # group by unique node
  nodes <-
    nodes %>%
    mutate(xy = paste(.$X, .$Y)) %>%
    group_by(xy) %>%
    mutate(nodeID = cur_group_id()) %>%
    ungroup() %>%
    select(-xy)

  # spatialize
  stnodes <- nodes %>%
    st_as_sf(coords = c("X", "Y")
             ,crs = st_crs(x)) #%>%
  #count(nodeID)

  # add hwy identifiers back to nodes
  line.ids <- x %>%
    select(c("id", tidyselect::all_of(line.ids)))

  stnodes <- left_join(stnodes,
                       abv_out(x)
                       ,by = c("edge.id" = "id"))
  #st_join(stnodes, line.ids)
  return(stnodes)
}




# connecting segments -----------------------------------------------------

#' fill.single.gap
#'
#' Uses segment endpoints and connects those within threshold distance. Creates
#' continuous line that I think is more appropriate. First two arguments
#' are taken from call within fill.gaps.
#' @param threshold Threshold in crs units. Fill gap if distance between endpoint of
#'   given edge and another in \code{lines} is < this threshold.
fill.single.gap <- function(edge, nodes, threshold = 200) {

  # add meters to threshold
  threshold = units::set_units(threshold, "m")

  # for given edge, find nearest start/endpoint that is not a part of given edge
  eligible.nodes = nodes[!nodes$nodeID %in% edge$nodeID,]
  neasest.to.i.nodes = edge %>%
    st_nearest_feature(eligible.nodes)

  # get nearest node(s) on other edges
  nn = eligible.nodes[neasest.to.i.nodes,]$nodeID

  # lines to nearest nodes...
  to.nn <- c(st_nearest_points(edge[1,],
                               eligible.nodes[eligible.nodes$nodeID == nn[1],])
             ,st_nearest_points(edge[2,],
                                eligible.nodes[eligible.nodes$nodeID == nn[2],]))

  to.nn <- unique(to.nn) %>% st_sfc(crs = st_crs(edge))

  edge$nn = nn
  edge$to.nn = to.nn
  edge$dist2nn = geod.length(to.nn)
  # filter those below threshold & make geometry the line
  new.seg = edge[edge$dist2nn < threshold,]
  if(nrow(new.seg) == 0) {
    edge$geometry = NULL
    return(new.seg)
  }

  new.seg = abv_out(new.seg) %>%
    rename(geometry = to.nn) %>% st_sf()

  return(new.seg)
}

#' fill.gaps
#'
#' maps fill.single.gap across all subsections of a single hwy. Takes ~denoded~ lines;
#' expects nhpn hwy data. The purpose is that if a highway has a short break
#' (<threshold) in the middle, this allows the polygon measure to ignore a short break.
#' Call with \code{return.gap.map=T} to visualize what it does.
#' @param hwy a single hwy, i.e., with single unique SIGN1, after divM::denode.lines
#'   is run on it
#' @param return.gap.map Return mapview leaflet to visualize output of fcn
#' @inheritDotParams fill.single.gap
fill.gaps <- function(hwy, return.gap.map = F, threshold = 200, ...) {

  require(lwgeom)

  hwy.type <- unique(hwy$SIGNT1)
  hwy.id <- unique(hwy$SIGN1)

  # subset to non-loops
  # (loops are rare but problematic. One example -- S213 in Boston.
  delooped <- hwy[st_startpoint(hwy) != st_endpoint(hwy), ]
  if(nrow(delooped) != nrow(hwy)) cat("Loop found in", hwy.id)

  # if no more than 1 non-loop segment, return early (no gaps to fill)
  if( nrow(delooped) <= 1 ) {
    out <- hwy %>% select(SIGN1, SIGNT1, geometry) %>% mutate(id = 1)
    return(out)
  }

  # to fill gaps, first get endpoints of existing segments
  hwy.n = find.endpoint.nodes(hwy)

  fillers = hwy.n %>%
    split(.$edge.id) %>%
    purrr::map(~fill.single.gap(., hwy.n, threshold = threshold))

  # return early if no gaps are filled
  if( all(purrr::map_lgl(fillers, ~(nrow(.) == 0))) )
    return(hwy)

  # alert user gap being filled if verbose
  # cat("filling gap in",hwy.id,"\n")

  fillers <- do.call("rbind", fillers)

  # return map if appropriate
  if( return.gap.map )
    return(mapview::mapview(hwy) + mapview::mapview(fillers, color = "red"))

  # otherwise join segments with gap-fillers
  continuous.hwy = st_union(hwy,
                            fillers) %>%
    st_union() %>%
    st_line_merge() %>%
    st_sf(SIGNT1 = hwy.type
          ,SIGN1 = hwy.id
          ,geometry = .)

  continuous.hwy$id <- 1:nrow(continuous.hwy)
  return(continuous.hwy)
}


#' Fix all hwys
#'
#' Fixes all hwys in place from raw data from subset of raw nhpn data. Note these
#' functions expect NHPN format, i.e., SIGNT1 and SIGNN1 columns as identifiers.
#' @inheritParams fill.gaps
#' @import purrr
#'
#' @export Fix.all.hwys
Fix.all.hwys <- function(hwys, threshold = 200, return.gap.map = F, ...) {

  dn.hwy <- hwys %>%
    split(.$SIGN1) %>%
    purrr::imap( ~denode.lines(.) )

  hwy <- dn.hwy %>%
    purrr::imap( ~fill.gaps(., threshold = threshold,
                            return.gap.map = return.gap.map))

  if(return.gap.map)
    return(hwy[purrr::map_lgl(hwy, ~("mapview" %in% class(.)))])

  # explode to linestring
  hwy <- do.call("rbind", hwy) %>%
    st_collection_extract("LINESTRING") %>%
    st_cast("LINESTRING")

  return(hwy)
}


# troubleshooting ---------------------------------------------------------
