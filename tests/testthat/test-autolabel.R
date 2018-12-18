context("Autolabeller")
foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))
p <- arphitgg(foo) +
  agg_line(agg_aes(x=x,y=y)) +
  agg_line(agg_aes(x=x,y=y2)) + agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

p <- arphitgg(foo) +
  agg_line(agg_aes(x=x,y=y)) +
  agg_line(agg_aes(x=x,y=y2)) + agg_autolabel(FALSE)
expect_error(
  print(p),
  NA
)

# Non-numeric anchor point (#122)
data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# Factors in data for auto labeller (#123)
data <- data.frame(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <- arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))

p <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y)) + agg_line(agg_aes(x=x,y=y2)) + agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# Non-numeric anchor point (#122)
data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# Factors in data for auto labeller (#123)
data <- data.frame(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <- arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel(TRUE)
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
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

foo <- ts(data.frame(x1=rnorm(10),x2=rnorm(10)),start=c(2000,1),frequency=4)
p <- arphitgg(foo) +
  agg_line(agg_aes(y=x1)) +
  agg_line(agg_aes(y=x2)) +
  agg_autolabel(TRUE)
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
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# Left-right axes
foo <- tibble::tibble(year = 2000:2020, y = rnorm(21),y2=rnorm(21))
p <- arphitgg(foo, agg_aes(x=year), layout = "1") +
  agg_line(agg_aes(y=y), panel = "1") +
  agg_line(agg_aes(y=y2), panel = "2") +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# Fail to find candidate with standard grid, requiring fallback

# No viable candidate at all for y.y
foo <- data.frame(x=rep(1:20,30),y=sort(rep(1:30,20)))
expect_warning({
  p <- arphitgg(foo, agg_aes(x = x, y = y)) +  agg_point() + agg_point() +
    agg_autolabel(FALSE) + agg_xlim(0, 20.5) + agg_ylim(0, 30, 5)
  print(p)
}, "Unable to find location for label for series y.y")

# Grid points structured so as to defeat standard autolabeller, but fallback succeeds easily
foo <- data.frame(x=1,y=1)
expect_error({
  p <- arphitgg(foo, agg_aes(x = x, y = y)) + agg_point() + agg_point() +
    agg_xlim(0, 2) +
    agg_ylim(0, 60, 16) +
    agg_autolabel(TRUE)
  print(p)
}, NA)

# Multi line labels
expect_error({
  foo <-
    tibble::tibble(
      x = 1:30,
      `foo\nbar` = rnorm(30),
      `foo\nbar\nbaz` = rnorm(30)
    )
  p <- arphitgg(foo) +
    agg_line(agg_aes(x = x, y = `foo\nbar\nbaz`)) +
    agg_line(agg_aes(x = x, y = `foo\nbar`)) +
    agg_autolabel(TRUE)
  print(p)
},
NA)

# Distance for bars
data <- tibble::tibble(x = sort(rep(letters[1:5],2)), g = rep(c("f","m"),5), y = rnorm(10))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col(stacked = FALSE) +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

data <- tibble::tibble(x = sort(rep(letters[1:5],3)), g = rep(letters[1:3],5), y = rnorm(15))
p <-  arphitgg(data, agg_aes(x=x,y=y,group=g)) +
  agg_col() +
  agg_autolabel(TRUE)
expect_error(
  print(p),
  NA
)

# No line of sight
data <- data.frame(x=1:10,a=1:10,b=0.5:9.5,c=1.5:10.5)
p <- arphitgg(data, agg_aes(x=x)) +
  agg_line(agg_aes(y=a)) +
  agg_line(agg_aes(y=b)) +
  agg_line(agg_aes(y=c)) +
  agg_xlim(1.5,10.5) +
  agg_autolabel(TRUE)
expect_error({print(p)}, NA)

# Labels on one panel
data <- data.frame(x=1:10,a=1:10,b=0.5:9.5,c=1.5:10.5)
p <- arphitgg(data,agg_aes(x=x), layout = "2v") +
  agg_line(agg_aes(y=a), panel = "1") +
  agg_line(agg_aes(y=b), panel = "1") +
  agg_line(agg_aes(y=b), panel = "2") +
  agg_line(agg_aes(y=c), panel = "2") +
  agg_label("Test",x=2,y=4,color="black",panel="1") +
  agg_autolabel(TRUE)
expect_error({print(p)}, NA)

graphics.off()

# Creating los mask failing for series outside the axes (#202)
data <- data.frame(x=seq(as.Date("2000-03-01"),by="month",length.out=20),y2=rnorm(20),y=1000:1019)

p <- arphitgg(data) +
  agg_line(agg_aes(x=x,y=y)) +
  agg_line(agg_aes(x=x,y=y2)) +
  agg_ylim(-5,5,3) +
  agg_autolabel()
expect_error(print(p), NA)

p <- arphitgg(data) +
  agg_line(agg_aes(x=x,y=y)) +
  agg_line(agg_aes(x=x,y=y2)) +
  agg_xlim(2011,2013) +
  agg_autolabel()
expect_error(print(p), NA)
