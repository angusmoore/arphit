context("Autolabel")

test_that("Simple labels", {
  set.seed(42)
  foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))
  p <- arphitgg(foo, showallxlabels = FALSE) +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) + agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-simple-line")
  )

  p <- arphitgg(foo, showallxlabels = FALSE) +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) + agg_autolabel(FALSE)
  expect_true(
    check_graph(p, "autolabel-simple-line")
  )

  # Non-numeric anchor point (#122)
  data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
  p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
    agg_col() +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-non-numeric-x")
  )

  # Factors in data for auto labeller (#123)
  data <- data.frame(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
  p <- arphitgg(data, agg_aes(x=x,y=y,group=g)) +
    agg_col() +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-factors")
  )

  # NAs in data for autolabeller (#126)
  set.seed(42)
  foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
  foo$y[1:10] <- NA
  p <- arphitgg(foo, agg_aes(x=year), showallxlabels = FALSE) +
    agg_line(agg_aes(y=y)) +
    agg_line(agg_aes(y=y2)) +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-nas-in-data")
  )

  # TS data
  set.seed(42)
  foo <- ts(data.frame(x1=rnorm(10),x2=rnorm(10)),start=c(2000,1),frequency=4)
  p <- arphitgg(foo) +
    agg_line(agg_aes(y=x1)) +
    agg_line(agg_aes(y=x2)) +
    agg_autolabel(TRUE) +
    agg_xlim(2000,2003)
  expect_true(
    check_graph(p, "autolabel-ts")
  )

  # Multipanels
  set.seed(42)
  foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
  p <- arphitgg(foo, agg_aes(x=year), layout = "2b2", showallxlabels = FALSE) +
    agg_line(agg_aes(y=y), panel = "3") +
    agg_line(agg_aes(y=y2), panel = "3") +
    agg_line(agg_aes(y=y), panel = "4") +
    agg_line(agg_aes(y=y2), panel = "4") +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-multipanel")
  )

  # Left-right axes
  set.seed(42)
  foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
  p <- arphitgg(foo, agg_aes(x=year), layout = "1", showallxlabels = FALSE) +
    agg_line(agg_aes(y=y), panel = "1") +
    agg_line(agg_aes(y=y2), panel = "2") +
    agg_ylim(-5,5,3,"1") +
    agg_ylim(-10,10,3,"2") +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-left-right-axes")
  )

  # Two series with same attributes in the one panel
  foo <- tibble::tibble(year = 2000:2020, y = 1:21)
  foo2 <- tibble::tibble(year = 2000:2020, y = 2:22)
  p <- arphitgg(aes = agg_aes(x=year,y=y), layout = "1", showallxlabels = FALSE) +
    agg_line(data = foo, panel = "1", color = "red") +
    agg_line(data = foo2, panel = "1", color = "red") +
    agg_autolabel(TRUE)
  expect_true(check_graph(p, "autolabel-same-series-one-panel"))
})

test_that("Autolabel with bars", {
  # bars
  set.seed(42)
  data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
  p <- arphitgg(data, agg_aes(x=x,y=y,group=g)) +
    agg_col(stacked = FALSE) +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-bars")
  )

  set.seed(42)
  data <- tibble::tibble(x = sort(rep(letters[1:5],3)), g = rep(letters[1:3],5), y = rnorm(15))
  p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
    agg_col() +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-bars2")
  )

  data <- data.frame(x=1:10,y=1,y2=-0.5,y3=2)
  p <- arphitgg(data) +
    agg_col(agg_aes(x=x,y=y)) +
    agg_col(agg_aes(x=x,y=y2)) +
    agg_col(agg_aes(x=x,y=y3)) +
    agg_autolabel(quiet = TRUE, arrow_bars = TRUE)
  expect_true(check_graph(p, "autolabel-arrow-bars"))
})

test_that("Autolabel with points", {
  set.seed(42)
  data <- data.frame(x=rnorm(20),y=rnorm(20),z=rnorm(20))
  p <- arphitgg(data) +
    agg_point(agg_aes(x=x,y=y)) +
    agg_point(agg_aes(x=x,y=z)) +
    agg_autolabel(TRUE)
  expect_true(check_graph(p, "autolabel-scatter"))
})

## Auto label fall back ====================
# Fail to find candidate with standard grid, requiring fallback
test_that("Autolabel fallback", {
  # No viable candidate at all for y.y
  foo <- data.frame(x=rep(1:20,30),y=sort(rep(1:30,20)),`y.y`=sort(rep(1:30,20)))
  expect_warning({
    p <- arphitgg(foo, agg_aes(x = x), showallxlabels = FALSE) +
      agg_point(agg_aes(y=y)) + agg_point(agg_aes(y=`y.y`)) +
      agg_autolabel(FALSE) + agg_xlim(1, 21) + agg_ylim(0, 30, 5)
    print(p)
  }, "Unable to find location for label for series y.y")

  # Grid points structured so as to defeat standard autolabeller, but fallback succeeds easily
  foo <- data.frame(x=rep(c(1,2),3),y=sort(rep(1:3,2)),g=sort(rep(1:3,2)))
  p <- arphitgg(foo, agg_aes(x=x,y=y,group=g), layout = "2b2")+
    agg_line() +
    agg_xlim(1.5,2.5) +
    agg_ylim(0.5,10.5,11) +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-fall-back")
  )
})

## multi line labels ===============

test_that("Multiline labels", {
  # Multi line labels
  set.seed(42)
  foo <-
    tibble::tibble(
      x = 1:30,
      `foo\nbar` = rnorm(30),
      `foo\nbar\nbaz` = rnorm(30)
    )
  p <- arphitgg(foo, showallxlabels = FALSE) +
    agg_line(agg_aes(x = x, y = `foo\nbar\nbaz`)) +
    agg_line(agg_aes(x = x, y = `foo\nbar`)) +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-multiline")
  )
})

## Line of sight ====================

test_that('Line of sight', {
  # No line of sight
  data <- data.frame(x=1:10,a=1:10,b=0.5:9.5,c=1.5:10.5)
  p <- arphitgg(data, agg_aes(x=x), showallxlabels = FALSE) +
    agg_line(agg_aes(y=a)) +
    agg_line(agg_aes(y=b)) +
    agg_line(agg_aes(y=c)) +
    agg_xlim(1.5,10.5) +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-no-LOS", 0.995)
  )

  # Creating los mask failing for series outside the axes (#202)
  set.seed(42)
  data <- data.frame(x=seq(as.Date("2000-03-01"),by="quarter",length.out=20),y2=rnorm(20),y=1000:1019)
  p <- arphitgg(data) +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) +
    agg_ylim(-5,5,3) +
    agg_autolabel()
  expect_true(
    check_graph(p, "autolabel-series-outside-axes")
  )
  p <- arphitgg(data) +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) +
    agg_xlim(2011,2016) +
    agg_autolabel()
  expect_true(
    check_graph(p, "autolabel-series-outside-axes-x")
  )
})

## Which panels should be autolabelled =================

test_that("Which panels should be autolabelled", {
  # Labels on one panel
  data <- data.frame(x=1:10,a=1:10,b=0.5:9.5,c=1.5:10.5)
  p <- arphitgg(data,agg_aes(x=x), layout = "2v", showallxlabels = FALSE) +
    agg_line(agg_aes(y=a), panel = "1") +
    agg_line(agg_aes(y=b), panel = "1") +
    agg_line(agg_aes(y=b), panel = "2") +
    agg_line(agg_aes(y=c), panel = "2") +
    agg_label("Test",x=2,y=4,color="black",panel="1") +
    agg_autolabel(TRUE)
  expect_true(
    check_graph(p, "autolabel-one-panel-labelled")
  )
})

## Misc tests ==================

test_that("Miscellaneous tests", {
  # Missing observations in stacked bar graphs (#217)
  set.seed(42)
  data <- data.frame(series_name = letters[1:10], value = rnorm(10), group = sample(1:3,10,TRUE))
  p <- arphitgg(data, agg_aes(x = series_name, y = value, group = group)) +
    agg_col() + agg_autolabel()
  expect_true(
    check_graph(p, "autolabel-missing-stacked-bar")
  )

  # Failing to remove labels on single-series panels properly (#249)
  p <- arphitgg(data.frame(x=1:10,y=1:10,y2=11:20,y3=21:30), layout = "2h") +
    agg_line(agg_aes(x=x,y=y), panel = "1") +
    agg_line(agg_aes(x=x,y=y2), panel = "1") +
    agg_line(agg_aes(x=x,y=y3), panel = "3") +
    agg_autolabel()
  expect_true(check_graph(p, "autolabel-remove-single-series-panel"))

  # Error for bar charts with NAs in the data (just a smoke test, don't care about output)
  foo <- data.frame(x=rep(1:5,2),y=rnorm(10),g=c(rep(1,5),rep(2,5)))
  foo$y[1] <- NA

  expect_error({
    p <- arphitgg(foo, agg_aes(x = x, y = y, group = g)) + agg_col() + agg_autolabel()
    print(p)
  },
  NA)
})
