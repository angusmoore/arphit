context("Autolabeller - miscellaneous tests")
foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))

p <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y)) + agg_line(agg_aes(x=x,y=y2)) + agg_autolabel()
expect_error(
  print(p),
  NA
)

# Non-numeric anchor point (#122)
data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# Factors in data for auto labeller (#123)
p <- data.frame(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10)) %>%
  arphitgg(agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

context("Autolabeller - miscellaneous tests")
foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))

p <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y)) + agg_line(agg_aes(x=x,y=y2)) + agg_autolabel()
expect_error(
  print(p),
  NA
)

# Non-numeric anchor point (#122)
data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# Factors in data for auto labeller (#123)
p <- data.frame(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10)) %>%
  arphitgg(agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# NAs in data for autolabeller (#126)
foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
foo$y[1:10] <- NA
p <- arphitgg(foo, agg_aes(x=year)) +
  agg_line(agg_aes(y=y)) +
  agg_line(agg_aes(y=y2)) +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

foo <- ts(data.frame(x1=rnorm(10),x2=rnorm(10)),start=c(2000,1),frequency=4)
p <- arphitgg(foo) +
  agg_line(agg_aes(y=x1)) +
  agg_line(agg_aes(y=x2)) +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# Multipanels
foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
p <- arphitgg(foo, agg_aes(x=year), layout = "2b2") +
  agg_line(agg_aes(y=y), panel = "3") +
  agg_line(agg_aes(y=y2), panel = "3") +
  agg_line(agg_aes(y=y), panel = "4") +
  agg_line(agg_aes(y=y2), panel = "4") +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# Left-right axes
foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
p <- arphitgg(foo, agg_aes(x=year), layout = "1") +
  agg_line(agg_aes(y=y), panel = "1") +
  agg_line(agg_aes(y=y2), panel = "2") +
  agg_autolabel()
expect_error(
  print(p),
  NA
)

# Fail to find candidate with standard grid, requiring fallback

# No viable candidate at all for y.y
foo <- data.frame(x=rep(1:20,30),y=sort(rep(1:30,20)))
expect_warning({
  p <- arphitgg(foo, agg_aes(x = x, y = y)) +  agg_point() + agg_point() +
    agg_autolabel() + agg_xlim(0, 20.5) + agg_ylim(0, 30, 5)
  print(p)
}, "Unable to find location for label for series y.y")

# Grid points structured so as to defeat standard autolabeller, but fallback succeeds easily
foo <- data.frame(x=1,y=1)
expect_error({
  p <- arphitgg(foo, agg_aes(x = x, y = y)) + agg_point() + agg_point() +
    agg_xlim(0, 2) +
    agg_ylim(0, 48, 13) +
    agg_autolabel()
  print(p)
}, NA)
