% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_predictr.R
\name{model_predictr}
\alias{model_predictr}
\title{Predict ecological niche models to multiple datasets}
\usage{
model_predictr(model, preds.list, preds.names)
}
\arguments{
\item{model}{A fitted ENM of the class MaxEnt. Can be extracted by accessing the "models" slot output from "test_multibg" or "test_multisp"}

\item{preds.list}{A list containing RasterStack objects of projection datasets}

\item{preds.names}{A vector of names corresponding to each dataset in "preds.list". Can be the name of a particular time period (e.g. LGM) or climate change scenario (e.g. SSP26)}
}
\value{
RasterStack of ENM predictions
}
\description{
Predict ecological niche models to multiple datasets
}
\examples{
pred.enms <- model_predictr(model = mx_model, preds.list = list(lgm, mh, current), preds.names = c('LGM', 'Mid-Holocene', 'Current'))
}
