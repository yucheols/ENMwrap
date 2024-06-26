% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_maker.R
\name{bin_maker}
\alias{bin_maker}
\title{Convert continuous ENM predictions into binary presence/absence maps}
\usage{
bin_maker(preds, th)
}
\arguments{
\item{preds}{RasterStack object containing continuous ENM predictions}

\item{th}{A list of numeric threshold values}
}
\value{
RasterStack of binary maps
}
\description{
Convert continuous ENM predictions into binary presence/absence maps
}
\examples{
get.binary <- bin_maker(preds = enm.preds, th = list(0.1, 0.23, 0.43))
}
