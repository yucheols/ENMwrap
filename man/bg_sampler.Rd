% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bg_sampler.R
\name{bg_sampler}
\alias{bg_sampler}
\title{Sample background points for (multiple) species to calibrate ecological niche models}
\usage{
bg_sampler(
  envs,
  n,
  occs_list = NULL,
  buffer_list = NULL,
  bias.grid = NULL,
  excludep = T,
  method = "random"
)
}
\arguments{
\item{envs}{RasterLayer of environmetal variable from which to draw background points}

\item{n}{Numerical value for the desired number of background points to sample}

\item{occs_list}{A list containing occurrence datasets. Each occurrence set needs to have two columns named "long" and "lat", and in that order. Required for the methods "random" and "buffer"}

\item{buffer_list}{A list of spatial buffers. Only needed when "method = 'buffer'"is used}

\item{bias.grid}{RasterLayer of bias grid to correct for spatial sampling bias. Only needed when "method = 'bias.grid'" is used}

\item{excludep}{Logical. Decides whether or not to exclude occurrence points from background sampling. Only used for "random" and "buffer" methods. By default, excludep = TRUE}

\item{method}{Character. Method to use for background sampling. If "random" is used, the background points are sampled randomly from the provided raster layer.
If "buffer" is used, the background points are sampled from within the boundary of provided buffer(s). If "bias.grid" is used, sample bias-corrected background points
from the provided bias grid. The method is "random" by default}
}
\value{
A list containing sampled background points
}
\description{
Sample background points for (multiple) species to calibrate ecological niche models
}
\examples{
bg <- bg_sampler(envs = envs, n = 10000, occs_list = occs_list, method = 'random')
}
