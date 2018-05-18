context("Integration - index")

data <- data.frame(date = seq.Date(from = as.Date("2001-01-01"),
                                   by = "quarter",
                                   length.out = 10),
                   x1 = rnorm(10),
                   x2 = rnorm(10),
                   x3 = rnorm(10, sd = 10),
                   x4 = rnorm(10, sd = 5))


p <- arphitgg(data, agg_aes(x=date,y=x1)) + agg_line()
print(p)

p <- arphitgg(data, agg_aes(x = date), layout = "2b2") +
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
  agg_label("A label", x = 2001, y = 2, panel = "1", color = "red") +
  agg_abline(x = 2004, panel = "2") +
  agg_bgshading(y1 = -1, y2 = 3, panel = "4")
print(p)

tsdata <- ts(data.frame(y=rnorm(10)), frequency = 4, start = 200)
agg_qplot(tsdata)

p <- arphitgg(data, agg_aes(x=date, y = x1)) + agg_line()
print(p)

p <- arphitgg(data, layout = "2b2") +
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
print(p)
