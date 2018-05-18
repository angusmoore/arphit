context("Autolabeller - miscellaneous tests")
foo <- data.frame(x = 1:50, y = rnorm(50), y2 = rnorm(50))

p <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y)) + agg_line(agg_aes(x=x,y=y2)) + agg_autolabel()
print(p)
