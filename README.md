# arphit <a href='https://angusmoore.github.io/arphit/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
![CI](https://github.com/angusmoore/arphit/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/angusmoore/arphit/branch/master/graph/badge.svg)](https://codecov.io/gh/angusmoore/arphit)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

`arphit` is a ggplot-inspired R package to easily create RBA-style graphs in R. Once you have your data, you can quickly create a plot with:
```
arphitgg(data, agg_aes(x=date,y=x1)) + agg_line()
```
<p align="center">
  <img src="man/figures/README-simple_example.png" width="400px" />
</p>

`arphit` is also customisable. You can create complex charts, with multiple panels, bars, titles, annotations, shading, sources and footnotes:
```
arphitgg(data, agg_aes(x = date), layout = "2b2") +
  agg_col(agg_aes(y=x1), panel = "1") +
  agg_line(agg_aes(y=x2), panel = "2") +
  agg_line(agg_aes(y=x3), panel = "3") +
  agg_line(agg_aes(y=x4), panel = "3") +
  agg_line(agg_aes(y=x4), panel = "4") +
  agg_title("arphit Makes Graphs in R") +
  agg_units("index", panel = "1") +
  agg_units("ppt", panel = "2") +
  agg_units("$", panel = "3") +
  agg_units("'000", panel = "4") +
  agg_shading(from = x4, to = x3) +
  agg_label("A label", x = 2001.5, y = 1, panel = "1", colour = "red") +
  agg_abline(x = 2002, panel = "2") +
  agg_bgshading(y1 = -1, y2 = 3, panel = "4")
```
<p align="center">
  <img src="man/figures/README-complex_example.png" width="400px" />
</p>

`arphit` also has a quick plotting function, which plots all (or a subset of) the columns in your data. This is particularly helpful for time series data, or other 'wide' data:
```
data <- ts(data.frame(y=rnorm(10)), frequency = 4, start = 200)
agg_qplot(data)
```
<p align="center">
  <img src="man/figures/README-qplot.png" width="400px" />
</p>

# Installation and getting started

## Installation

Install the latest stable release of `arphit` using the R `remotes` package:
```
library(remotes)
install_github("angusmoore/arphit", ref = "stable", build = TRUE, build_opts =  c("--no-resave-data", "--no-manual"))
```

You may need to first install the `remotes` package if you don't already have it (`install.packages("remotes")`).

Installing may fail if `remotes` cannot correctly determine your proxy server. If so, you'll get one of the following error message when you try to install:
```
Installation failed: Timeout was reached: Connection timed out after 10000 milliseconds

Failed to connect to api.github.com port 443: Timed out
```
If you get this message, try setting your proxy server with the following command, and then running the install again:
```
Sys.setenv(https_proxy = curl::ie_get_proxy_for_url("https://www.google.com"))
```

## Getting started

(For more detail see [getting started guide](https://angusmoore.github.io/arphit/articles/arphit.html).)

The very first step to using arphit is to import it into your workspace:
```
library(arphit)
```

The second step is to get some data. For these simple examples, we'll randomly construct three years worth of quarterly time series data, starting from Q1 2000:
```
data <- data.frame(date = seq.Date(from = as.Date("2001-01-01"),
                                   by = "quarter",
                                   length.out = 10),
                   x1 = rnorm(10),
                   x2 = rnorm(10),
                   x3 = rnorm(10, sd = 10),
                   x4 = rnorm(10, sd = 5))
```

To make a simple line plot, all we need to do is specify the `x` and `y` variables and tell arphit we want a layer:
```
arphitgg(data, agg_aes(x=date, y = x1)) + agg_line()
```
<p align="center">
  <img src="man/figures/README-nooptions.png" width="400px" />
</p>

That chart is serviceable, but not very well labelled. Let's make it a bit nicer, by putting each of the four series into its own panel, and giving the graph and panels titles, and a source.
```
arphitgg(data, layout = "2b2") +
  agg_line(agg_aes(x=date, y = x1), panel = "1") + 
  agg_line(agg_aes(x=date, y = x2), panel = "2") + 
  agg_line(agg_aes(x=date, y = x3), panel = "3") + 
  agg_line(agg_aes(x=date, y = x4), panel = "4") + 
  agg_title("A Randomly Created Graph") +
  agg_subtitle("A short example") + 
  agg_units("index") + 
  agg_title("Panel 1", panel = "1") + 
  agg_title("Panel 2", panel = "2") + 
  agg_title("Panel 3", panel = "3") + 
  agg_title("Panel 4", panel = "4") + 
  agg_source("Randomly generated data")
```
<p align="center">
  <img src="man/figures/README-lotsofoptions.png" width="400px" />
</p>

The [getting started vignette](https://angusmoore.github.io/arphit/articles/arphit.html) provides more introduction on how to create graphs in `arphit`. `arphit` has a lot of plotting options to control how your chart looks. These are all explained, with examples, in the [plotting options vignette](https://angusmoore.github.io/arphit/articles/plotting-options.html).

# Documentation

The [getting started](https://angusmoore.github.io/arphit/articles/arphit.html) guide is a good place to start. A list of all the plotting options and examples of how to use them can be found in the [plotting options vignette](https://angusmoore.github.io/arphit/articles/plotting-options.html). It is more detailed than the getting started guide.

Package documentation can be found [here](https://angusmoore.github.io/reference/). It is technical and in most cases will be less useful than the plotting options vignette.

If these sources don't answer your problems, or you encounter a bug, please open an issue (see below).

# Bugs and planned features

Please report any bugs you find on the github [issue tracker](https://github.com/angusmoore/arphit/issues). Feature requests can also be made there (and you can see planned improvements as well).
