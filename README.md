[![Build Status](https://travis-ci.org/angusmoore/arphit.svg?branch=master)](https://travis-ci.org/angusmoore/arphit)
[![Coverage Status](https://coveralls.io/repos/github/angusmoore/arphit/badge.svg?branch=master)](https://coveralls.io/github/angusmoore/arphit?branch=master)

# arphit

`arphit` is an R package to make it easy to quickly create RBA-style time series plots. Once you have your data, creating a plot is as simple as:
```
arphit.tsgraph(data)
```
<p align="center">
  <img src="https://angusmoore.github.io/arphit/images/simple_example.png" width="400px" />
</p>

`arphit` is also extremely customisable. You can create complex charts, with multiple panels, bars, titles, annotations, shading, sources and footnotes:
```
arphit.tsgraph(data,
               layout = "2b2",
               series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3", "x4"), "4" = c("x5")),
               bars = c("x1"),
               title = "arphit Makes Graphing Easy",
               scaleunits = list("1" = "index", "2" = "ppt", "3" = "$", "4" = "000s"),
               shading = list(list(from = "x3", to = "x4", color= "lightgrey")),
               labels = list(list(x = 2001, y = 2, text = "A label", panel = 1, color = "red")),
               lines = list(list(x = 2004, panel = "2")),
               bgshading = list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 3, panel = "4")))
```
<p align="center">
  <img src="https://angusmoore.github.io/arphit/images/complex_example.png" width="400px" />
</p>

# Installation and getting started

## Installation

Install the package using the R `devtools` package:
```
library(devtools)
install_github("angusmoore/arphit", build_vignettes = TRUE)
```

You may need to first install the `devtools` package if you don't already have it (`install.packages("devtools")`).

Installing may fail if `devtools` cannot correctly determine your proxy server. If so, you'll get the following error message when you try to install:
```
Installation failed: Timeout was reached: Connection timed out after 10000 milliseconds
```
If you get this message, try setting your proxy server with the following command, and then running the install again:
```
httr::set_config(httr::use_proxy(curl::ie_get_proxy_for_url("http://www.google.com")))
```

## Getting started

The very first step to using arphit is to import it into your workspace:
```
library(arphit)
```

The second step is to get some data. The easiest way to plot is to have your data as `ts` object. (However, `tibble`s and `data.frame`s are also supported, and you can plot non-time-series graphs too). For these simple examples, we'll randomly construct three years worth of quarterly time series data, starting from Q1 2000:
```
T <- 12
data <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10), x4 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 4)
```

To make the simplest plot, all you need to do is call:
```
arphit.tsgraph(data)
```
<p align="center">
  <img src="https://angusmoore.github.io/arphit/images/nooptions.png" width="400px" />
</p>

That chart is serviceable, but not very well labelled. Let's make it a bit nicer, by putting each of the four series into its own panel, and giving the chart and panels titles, and assign a source.
```
arphit.tsgraph(data,
               series = list("1" = "x1", "2" = "x2", "3" = "x3", "4" = "x4"),
               title = "A Randomly Created Chart",
               subtitle = "A short example",
               scaleunits = "index",
               paneltitles = list("1" = "Series 1", "2" = "Series 2", "3" = "Series 3", "4" = "Series 4"),
               sources = c("Randomly generated data"))
```
<p align="center">
  <img src="https://angusmoore.github.io/arphit/images/lotsofoptions.png" width="400px" />
</p>

`arphit` has a lot of plotting options to control how your chart looks. These are all explained, with examples, in the [plotting options vignette](https://angusmoore.github.io/arphit/plotting-options.html).

## ``ggplot2``-like interface

`arphit` also has an alternative, `ggplot2`-like way of creating graphs. This may be useful for those more familiar with that style of plotting. For instance:
```
data <- tibble(dates = seq.Date(from = as.Date("2000-01-01"), length.out = 20, by = "quarter"), y = rnorm(20))
data %>% 
  arphitgg(agg_aes(x = dates, y = y)) + agg_line()
```
```{r, out.width = "50%", echo = FALSE, fig.align="center"}
knitr::include_graphics("ggplot.png")
```
The interface supports groups, so that you can easily plot 'long' data. It can plot scatter, line and bar graphs. 

See the [gg-interface vignette](https://angusmoore.github.io/arphit/gg-interface.html) for more information.

# Documentation

A list of all the plotting options and examples of how to use them can be found in the [plotting options vignette](https://angusmoore.github.io/arphit/plotting-options.html). It is a good first place to start.

Package documentation can be found [here](https://angusmoore.github.io/arphit/arphit.pdf). It is technical and in most cases will be less useful than the plotting options vignette.

If these sources don't answer your problems, or you encounter a bug, please open an issue (see below).

# Bugs and planned features

A list of known drawbacks, planned features and other to-do tasks can be found [here](https://angusmoore.github.io/arphit/todo.html).

Please report any bugs you find on the github [issue tracker](https://github.com/angusmoore/arphit/issues).
