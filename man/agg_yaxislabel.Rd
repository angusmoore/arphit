% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg-constructors.R
\name{agg_yaxislabel}
\alias{agg_yaxislabel}
\title{Add an axis label to the y axis}
\usage{
agg_yaxislabel(axislabel, panel = NULL)
}
\arguments{
\item{axislabel}{A string specifying the axis label}

\item{panel}{(optional) Specify a panel identifier to add to a specific
panel. If blank, axis label will be applied to all panels. You can specify a
vector of panels (e.g. `panel = c("1","3")`) to apply the axis label to
multiple panels at once.}
}
\description{
Add an axis label to the y axis
}
\examples{
arphitgg(data) + agg_yaxislabel("Some y axis label")

}
\seealso{
\code{vignette("plotting-options", package = "arphit")} for a
detailed description of all the plotting options
}
