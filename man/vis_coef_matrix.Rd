% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis.r
\name{vis_coef_matrix}
\alias{vis_coef_matrix}
\title{vis_coef_matrix}
\usage{
vis_coef_matrix(coefL, reorderList = NULL)
}
\arguments{
\item{coefL}{list of numeric matrix [models x coefficients]}

\item{reorderList}{an ordered gene list that makes all the plots have the same gene order}
}
\value{
ggplot object of the coefficients
}
\description{
Visualize a prototypical model from a matrix of coefficients
}
