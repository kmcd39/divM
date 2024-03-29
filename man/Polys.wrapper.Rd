% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polys fcns.R
\name{Polys.wrapper}
\alias{Polys.wrapper}
\title{Polys.wrapper}
\usage{
Polys.wrapper(region, div.sf, fill.gaps = T, ...)
}
\arguments{
\item{region}{\code{sf} object representing neighborhoods to allocate to subdivision}

\item{...}{
  Arguments passed on to \code{\link[=subset.polys.divs]{subset.polys.divs}}, \code{\link[=Fix.all.hwys]{Fix.all.hwys}}, \code{\link[=polygonal.div]{polygonal.div}}
  \describe{
    \item{\code{div.sf}}{sf object with division.}
    \item{\code{div.identifier.column}}{If 'div.sf' has an identifier column to be used to
filter by, supply here. Must be supplied for other subset arguments to work;
\code{NULL} by default.}
    \item{\code{always.include}}{Always include divisions that have this value in the
identifier column}
    \item{\code{include.intersecting}}{Include additional divisions that don't have above
identifier, but which intersect with divs that do.}
    \item{\code{remove.NA.divs}}{Whether or not to remove all divisions with an NA in the
identifier column.}
    \item{\code{return.gap.map}}{Return mapview leaflet to visualize output of fcn}
    \item{\code{divs}}{linear \code{sf} representing divisions.}
    \item{\code{negative.buffer}}{Shrink region by this amount only for calculating polygons.
Useful for handling shpfiles w/ different resolutions, especially for regions
along an international border. Defaults to 100m.}
    \item{\code{min.size}}{Minimum (area) size for population, in meters. Defaults to 5e5, or
1/2 a km^2. Set to NULL to not filter by size.}
    \item{\code{return.sf}}{If true, returns an sf object, w/ one row per polygon
subdivision, that can easily be mapped or allocated to neighborhoods. If false
(default), returns number of polygons.}
  }}
}
\description{
Wraps all the functions used to generate the polygonal subdivision measure.
}
