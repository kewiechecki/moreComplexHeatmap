% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getUpset.R
\name{getUpset}
\alias{getUpset}
\title{Writes UpSet plot to file}
\usage{
getUpset(ls, file, N = NA, combColFn = function(x) "black", ..., setClust = F)
}
\arguments{
\item{ls}{A list of sets to be plotted}

\item{file}{The output file}

\item{N}{The unverse size. Defaults to the number of unique elements.}

\item{combColFn}{A function defining the color mapping of intersections.}

\item{...}{Additional arguments to \link{\code{ComplexHeatmap::make_comb_mat()}}.}
}
\value{
A Heatmap object.
}
\description{
Writes UpSet plot to file
}
