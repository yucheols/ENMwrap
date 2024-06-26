% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fold_maker.R
\name{fold_maker}
\alias{fold_maker}
\title{Generate spatial blocks based on the estimated block size}
\usage{
fold_maker(occs, bg.list, envs, k, block.size)
}
\arguments{
\item{occs}{A data.frame of species occurrence points containing two columns, in the order of "long" and "lat"}

\item{bg.list}{A list of background datasets, each dataset in data.frame format. The occurrence and background datasets must have matching column names}

\item{envs}{RasterStack of environmental variables}

\item{k}{Integer. The number of spatial folds to generate}

\item{block.size}{A list of block sizes output from the "block_size" function}
}
\value{
A list with two slots: one for occurrence partitions, another for background partitions
}
\description{
Generate spatial blocks based on the estimated block size
}
\examples{
folds <- fold_maker(occs = occs, bg.list = list(bg1, bg2, bg3), envs = envs, k = 4, block.size = block.size)

occs.folds <- folds[[1]]
bg.folds <- folds[[2]]
}
