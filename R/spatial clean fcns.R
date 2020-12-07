#' handle.overlaps
#'
#' This cuts out areas so all sub polygons are non-overlapping. Trims out
#' overlaps from larger shapes, while leaving overlapping shape on its own.
#' Relevant when a smaller polygon is 100% covered by larger one(s) and you want
#' non-overlapping polygons.
#' @export
handle.overlaps <- function(x) {
  # get polygons formed from overlaps
  overlaps <- x %>% st_intersection() %>% filter(grepl("POLYGON", st_geometry_type(.$geometry)))

  overlaps %>% select(-c(n.overlaps, origins))
}



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
  x <- st_make_valid(x)
  x <- st_buffer(x, 0)
  x <- x %>% st_set_precision(1e5)
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

  if(!is.null(group.cols))
    grpd.x <- x %>%
      group_by_at(vars(all_of(group.cols))) %>%
      summarise(., do_union = TRUE)
  else
    grpd.x <- x %>% st_union() %>% st_sf()

  xploded.x <- x %>%
    ez.explode()

  if (nrow(grpd.x) == nrow(xploded.x)) {
    x$id = 1:nrow(x)
    return(x)
  } else
    grpd.x %>%
    ungroup() %>%
    st_cast("MULTILINESTRING") %>%
    st_line_merge() %>%
    ez.explode() %>%
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
  if(nrow(x) == 0) return(NULL)
  if(!all(line.ids %in% colnames(x))) stop("missing id columns")

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
    select(c("id", all_of(line.ids)))

  stnodes <- left_join(stnodes,
                       divFcns::abv_out(x)
                       ,by = c("edge.id" = "id"))
  #st_join(stnodes, line.ids)
  return(stnodes)
}




# connecting segments -----------------------------------------------------

#' fill.single.gap
#'
#' Uses segment endpoints and connects those within threshold distance. Creates
#' continuous line that I think is as appropriate as possible.
#' @export
fill.single.gap <- function(edge, nodes, threshold = 50, verbose = T, ...) {

  # add meters to threshold
  threshold = units::set_units(threshold, "m")

  # for each edge, find nearest edge on other node
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
  #cat("addressed gaps at ", unique(edge$SIGN1), "\n")

  new.seg = abv_out(new.seg) %>%
    rename(geometry= to.nn) %>% st_sf()

  return(new.seg)
}

#' fill.hwy.gaps
#'
#' maps fill.single.gap across output of previous functions. Takes ~denoded~
#' lines; expects nhpn hwy data.
#' @export
fill.gaps <- function(hwy, threshold = 200, return.gap.map = F, ...) {
  hwy.type <- unique(hwy$SIGNT1)
  hwy.id <- unique(hwy$SIGN1)

  # subset to non-loops
  # (loops are very rare but problematic. Found one-- S213 in Boston. Use cat to locate others
  delooped <- hwy[st_startpoint(hwy) != st_endpoint(hwy), ]
  if(nrow(delooped) != nrow(hwy)) cat("Loop found in", hwy.id)

  # if no more than 1 non-loop segment, return HWYS early
  if( nrow(delooped) <= 1 ) {
    out <- hwy %>% select(SIGN1, SIGNT1, geometry) %>% mutate(id = 1)
    return(out)
  }

  cat("fixing",hwy.id,"\n")

  # to fill gaps, first get endpoints of existing segments
  hwy.n = find.endpoint.nodes(hwy)

  fillers = hwy.n %>%
    split(.$edge.id) %>%
    purrr::map(~fill.single.gap(., hwy.n, threshold = threshold), ...)

  # return early if no gaps filled
  if( all(map_lgl(fillers, ~(nrow(.) == 0))) )
    return(hwy)

  fillers <- do.call("rbind", fillers)

  # return map if appropriate
  if( return.gap.map )
    return(mapview(hwy) + mapview(fillers, color = "red"))

  # join segments with gap-fillers
  continuous.intst = st_union(hwy,
                              fillers) %>%
    st_union() %>%
    st_line_merge() %>%
    st_sf(SIGNT1 = hwy.type
          ,SIGN1 = hwy.id
          ,geometry = .)

  continuous.intst$id <- 1:nrow(continuous.intst)
  return(continuous.intst)
}



#' Fix all hwys
#'
#' Fixes all hwys in place from raw data from subset of raw nhpn data. Applies
#' \code{denode.lines} then \code{fill.gaps}; returns exploded clean NHPN hwy data.
#' @export
Fix.all.hwys <- function(hwy, return.gap.map = F, ...) {

  dn.hwy <- hwy %>%
    split(.$SIGN1) %>%
    purrr::imap( ~denode.lines(.) )

  hwy <- dn.hwy %>%
    purrr::imap( ~fill.gaps(., return.gap.map = return.gap.map), ...)

  if(return.gap.map)
    return(hwy[purrr::map_lgl(hwy, ~("mapview" %in% class(.)))])

  hwy <- do.call("rbind", hwy) %>%
    ez.explode()

  return(hwy)
}