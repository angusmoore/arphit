% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colpal.R
\docType{data}
\name{RBA}
\alias{RBA}
\title{RBA colour palette}
\format{An object of class \code{character} of length 117.}
\usage{
RBA
}
\description{
A list of RBA-standard colours to use. Use them by using RBA["Red1"] in a
colour declaration. For instance:
}
\details{
arphitgg(data.frame(x = 1:10, y = 1:10), agg_aes(x, y)) +
  agg_line(colour = RBA["Red2"])

To see a list of all the available colours, type `vignette("rbacolours")`
}
\keyword{datasets}
