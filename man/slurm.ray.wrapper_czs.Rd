% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gen-measures-fcns.R
\name{slurm.ray.wrapper_czs}
\alias{slurm.ray.wrapper_czs}
\title{slurm.ray.wrapper_czs}
\usage{
slurm.ray.wrapper_czs(
  place.geoid,
  trim2LAC = FALSE,
  remove.holes = TRUE,
  minimum.segment.length = 10,
  minimum.hwy.length = 1000,
  fill.gaps = T,
  always.include = "I",
  include.intersecting = FALSE,
  hwy.types = NULL,
  drop.NA = T,
  buffer.meters = 300,
  save.dir,
  save.name
)
}
\description{
combines Count.rays call with save.csv instructions and more slurm-friendly
parameters.
}
