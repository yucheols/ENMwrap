% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_thresh.R
\name{get_thresh}
\alias{get_thresh}
\title{Get MaxEnt threshold values from a single prediction or multiple continuous ENM predictions}
\usage{
get_thresh(preds, occs.list, type = "mtp")
}
\arguments{
\item{preds}{RasterLayer (single prediction) or RasterStack (multiple predictions) object containing continuous ENM predictions}

\item{type}{The type of thresholding method to use. If "mtp" is used, calculate the minimum presence (= zero omission) threshold. If "p10" is used, calculate the 10 percent presence threshold.}

\item{occs}{A list containing two column data frames of species occurrence points. The length of this list should correspond to the number of layers contained in the "preds" object.
For thresholding a single prediction for one species, input "occs.list = list(occs.sp1)". For thresholding multiple predictions for a single species, input "occs.list = list(occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1)".
For multiple species, input "occs.list = list(occs.sp1, occs.sp2, occs.sp3, occs.sp4, occs.sp5, occs.sp6)"}
}
\value{
A list containing the calculated threshold values
}
\description{
Get MaxEnt threshold values from a single prediction or multiple continuous ENM predictions
}
\examples{
# For a single prediction for one species
thresh <- get_thresh(preds = pred, occs.list = list(occs), type = 'p10')

# For multiple predictions for a single species
thresh <- get_thresh(preds = preds, occs.list = list(occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1), type = 'p10')

# For multiple predictions for multiple species
thresh <- get_thresh(preds = preds, occs.list = list(occs.sp1, occs.sp2, occs.sp3, occs.sp4, occs.sp5, occs.sp6), type = 'p10')
}
