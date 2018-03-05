# Build all the images used in the readme
library(arphit)

T <- 48
data <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10), x4 = rnorm(T, sd = 5), x5 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 12)


arphit(data, series = list("1" = "x1"), filename = "simple_example.png")

arphit(data,
               layout = "2b2",
               series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3", "x4"), "4" = c("x5")),
               bars = c("x1"),
               title = "arphit Makes Graphing Easy",
               scaleunits = list("1" = "index", "2" = "ppt", "3" = "$", "4" = "000s"),
               shading = list(list(from = "x3", to = "x4", color= "lightgrey")),
               labels = list(list(x = 2001, y = 2, text = "A label", panel = 1, color = "red")),
               lines = list(list(x = 2004, panel = "2")),
               bgshading = list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 3, panel = "4")),
               filename = "complex_example.png")

T <- 12
data <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10), x4 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 4)

arphit(data, filename = "nooptions.png")

arphit(data,
               series = list("1" = "x1", "2" = "x2", "3" = "x3", "4" = "x4"),
               layout = "2b2",
               title = "A Randomly Created Chart",
               subtitle = "A short example",
               scaleunits = "index",
               paneltitles = list("1" = "Series 1", "2" = "Series 2", "3" = "Series 3", "4" = "Series 4"),
               sources = c("Randomly generated data"),
               filename = "lotsofoptions.png")

data <- tibble(dates = seq.Date(from = as.Date("2000-01-01"), length.out = 20, by = "quarter"), y = rnorm(20))
p <- data %>%
  arphitgg(agg_aes(x = dates, y = y)) + agg_line()
agg_draw(p, filename = "ggplot.png")
