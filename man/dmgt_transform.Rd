% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmgt_transform.R
\name{dmgt_transform}
\alias{dmgt_transform}
\title{Transform DataPlus raw data fram to standard data frame}
\usage{
dmgt_transform(
  .site_data,
  .rare_data = NULL,
  .ntf_data = NULL,
  .water_data = NULL,
  .rare_scan_data = NULL,
  .nnf_ct_data = NULL
)
}
\arguments{
\item{.site_data}{A site data frame (required)}

\item{.rare_data}{A rare data frame (optional)}

\item{.ntf_data}{A ntf data frame (optional)}

\item{.water_data}{A water quality data frame (optional)}

\item{.rare_scan_data}{A rare scan data frame (optional)}

\item{.nnf_ct_data}{A non native count data frame (optional)}
}
\value{
A list of data frames restructured to the data standard
}
\description{
Transform DataPlus raw data fram to standard data frame
}
