% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyper.R
\name{condHyper}
\alias{condHyper}
\title{Hypergeometric test for enrichment of conditions in a cluster.}
\usage{
condHyper(id, conds, clusts, padj.method = "fdr")
}
\arguments{
\item{id}{A vector of sample IDs.}

\item{conds}{A vector of the same length as ID giving the condition of each sample.}

\item{clusts}{A vector of the same length as ID giving the cluster ID of each sample.}

\item{padj.method}{Method passed to \code{\link{p.adjust}} for multiple hypothesis correction.}
}
\description{
Hypergeometric test for enrichment of conditions in a cluster.
}
\seealso{
\code{\link{phyper2}}, \code{\link{p.adjust}}
}
