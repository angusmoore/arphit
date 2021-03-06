% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg-constructors.R
\name{agg_col}
\alias{agg_col}
\title{Add a col layer to an arphit plot.}
\usage{
agg_col(aes = NULL, data = NULL, colour = NULL, barcol = NULL,
  panel = "1", stacked = TRUE, reorder_bars = NULL, color)
}
\arguments{
\item{aes}{The aesthetic that defines the layer. Will inherit (or parts
thereof) if omitted.}

\item{data}{The data to be used. Will inherit from parent if missing.}

\item{colour}{A colour to be applied to all of the series, or (if your
aesthetic has a group), a vector of colours that will be cycled through to
consecutive group elements.}

\item{barcol}{(optional) Outline colours for each bar series}

\item{panel}{(default = "1") Which panel of the graph to place this layer on.
You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the
layer to multiple panels at once.}

\item{stacked}{(default = TRUE) Stack the bars, or group them?}

\item{reorder_bars}{A vector (of values of your grouping variable) to set the
order bars should appear in. First element is closest to the axis. Does
nothing for non-grouped layers. (And doesn't affect adding subsequent layers,
which will always appear after earlier layers, regardless of `reorder_bars`)}

\item{color}{(Deprecated; use colour instead) Alias for colour}
}
\description{
Add a col layer to an arphit plot.
}
\examples{
data  <- data.frame(unemployment = rnorm(20),
                    state = c(rep("A", 10), rep("B", 10)),
                    date = seq.Date(from = as.Date("2017-01-10"),
                                    length.out = 10,
                                    by = "quarter"))
arphitgg(data) +
  agg_col(agg_aes(x = date, y = unemployment, group = state), panel = "1")

}
\seealso{
\code{vignette("plotting-options", package = "arphit")} for a
detailed description of all the plotting options
}
