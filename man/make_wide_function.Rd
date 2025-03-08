% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_wideformat.R
\name{make_wide_function}
\alias{make_wide_function}
\title{Convert dataset to wide format while aligning Time = 0}
\usage{
make_wide_function(dataset)
}
\arguments{
\item{dataset}{A data frame containing electrophysiology data.}
}
\value{
A data frame reshaped into wide format with missing values padded.
}
\description{
This function takes a dataset with multiple time points for each \code{animal_cell_id}
and expands the dataset to include a full range of time points, ensuring that
missing values are filled with \code{NA}.
}
