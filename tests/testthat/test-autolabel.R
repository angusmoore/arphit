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
