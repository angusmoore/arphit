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
