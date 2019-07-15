context("Draw panel")

## getlocation ===============
test_that("getlocation", {
  expect_that(getlocation("1", "1"), equals(c(1,1)))
  expect_that(getlocation("2", "1"), equals(c(1,1)))

  expect_that(getlocation("1", "2v"), equals(c(1,1)))
  expect_that(getlocation("2", "2v"), equals(c(1,2)))

  expect_that(getlocation("1", "2h"), equals(c(1,1)))
  expect_that(getlocation("2", "2h"), equals(c(1,1)))
  expect_that(getlocation("3", "2h"), equals(c(2,1)))
  expect_that(getlocation("4", "2h"), equals(c(2,1)))

  expect_that(getlocation("1", "2b2"), equals(c(1,1)))
  expect_that(getlocation("2", "2b2"), equals(c(1,2)))
  expect_that(getlocation("3", "2b2"), equals(c(2,1)))
  expect_that(getlocation("4", "2b2"), equals(c(2,2)))

  expect_equal(getlocation("1", "3v"), c(1,1))
  expect_equal(getlocation("2", "3v"), c(1,2))
  expect_equal(getlocation("3", "3v"), c(1,3))

  expect_equal(getlocation("1", "3h"), c(1,1))
  expect_equal(getlocation("2", "3h"), c(1,1))
  expect_equal(getlocation("3", "3h"), c(2,1))
  expect_equal(getlocation("4", "3h"), c(2,1))
  expect_equal(getlocation("5", "3h"), c(3,1))
  expect_equal(getlocation("6", "3h"), c(3,1))

  expect_equal(getlocation("1", "3b2"), c(1,1))
  expect_equal(getlocation("2", "3b2"), c(1,2))
  expect_equal(getlocation("3", "3b2"), c(2,1))
  expect_equal(getlocation("4", "3b2"), c(2,2))
  expect_equal(getlocation("5", "3b2"), c(3,1))
  expect_equal(getlocation("6", "3b2"), c(3,2))

  expect_equal(getlocation("1", "4h"), c(1,1))
  expect_equal(getlocation("2", "4h"), c(1,1))
  expect_equal(getlocation("3", "4h"), c(2,1))
  expect_equal(getlocation("4", "4h"), c(2,1))
  expect_equal(getlocation("5", "4h"), c(3,1))
  expect_equal(getlocation("6", "4h"), c(3,1))
  expect_equal(getlocation("7", "4h"), c(4,1))
  expect_equal(getlocation("8", "4h"), c(4,1))

  expect_equal(getlocation("1", "4b2"), c(1,1))
  expect_equal(getlocation("2", "4b2"), c(1,2))
  expect_equal(getlocation("3", "4b2"), c(2,1))
  expect_equal(getlocation("4", "4b2"), c(2,2))
  expect_equal(getlocation("5", "4b2"), c(3,1))
  expect_equal(getlocation("6", "4b2"), c(3,2))
  expect_equal(getlocation("7", "4b2"), c(4,1))
  expect_equal(getlocation("8", "4b2"), c(4,2))

  expect_error(getlocation("1", "foo"))
})

## getsides ================
test_that("getsides", {
  expect_that(getsides("1", "1"), equals(2))
  expect_that(getsides("1", "2h"), equals(2))
  expect_that(getsides("1", "2v"), equals(2))
  expect_that(getsides("1", "2b2"), equals(2))
  expect_equal(getsides("1", "3v"), 2)
  expect_equal(getsides("1", "3h"), 2)
  expect_equal(getsides("1", "3b2"), 2)
  expect_equal(getsides("1", "4h"), 2)
  expect_equal(getsides("1", "4b2"), 2)

  expect_that(getsides("2", "1"), equals(4))
  expect_that(getsides("2", "2h"), equals(4))
  expect_that(getsides("2", "2v"), equals(4))
  expect_that(getsides("2", "2b2"), equals(4))
  expect_equal(getsides("2", "3v"), NA)
  expect_equal(getsides("2", "3h"), 4)
  expect_equal(getsides("2", "3b2"), 4)
  expect_equal(getsides("2", "4h"), 4)
  expect_equal(getsides("2", "4b2"), 4)

  expect_error(getsides("3", "1"))
  expect_that(getsides("3", "2h"), equals(2))
  expect_error(getsides("3", "2v"))
  expect_that(getsides("3", "2b2"), equals(2))
  expect_equal(getsides("3", "3v"), 4)
  expect_equal(getsides("3", "3h"), 2)
  expect_equal(getsides("3", "4h"), 2)
  expect_equal(getsides("3", "4b2"), 2)

  expect_error(getsides("4", "1"))
  expect_that(getsides("4", "2h"), equals(4))
  expect_error(getsides("4", "2v"))
  expect_that(getsides("4", "2b2"), equals(4))
  expect_error(getsides("4", "3v"))
  expect_equal(getsides("4", "3h"), 4)
  expect_equal(getsides("4", "4h"), 4)
  expect_equal(getsides("4", "4b2"), 4)

  expect_error(getsides("5", "1"))
  expect_error(getsides("5", "2v"))
  expect_error(getsides("5", "2h"))
  expect_error(getsides("5", "2b2"))
  expect_error(getsides("5", "3v"))
  expect_equal(getsides("5", "3h"), 2)
  expect_equal(getsides("5", "4h"), 2)
  expect_equal(getsides("5", "4b2"), 2)

  expect_error(getsides("6", "1"))
  expect_error(getsides("6", "2v"))
  expect_error(getsides("6", "2h"))
  expect_error(getsides("6", "2b2"))
  expect_error(getsides("6", "3v"))
  expect_equal(getsides("6", "3h"), 4)
  expect_equal(getsides("6", "4h"), 4)
  expect_equal(getsides("6", "4b2"), 4)

  expect_error(getsides("7", "1"))
  expect_error(getsides("7", "2v"))
  expect_error(getsides("7", "2h"))
  expect_error(getsides("7", "2b2"))
  expect_error(getsides("7", "3v"))
  expect_error(getsides("7", "3h"))
  expect_equal(getsides("7", "4h"), 2)
  expect_equal(getsides("7", "4b2"), 2)

  expect_error(getsides("8", "1"))
  expect_error(getsides("8", "2v"))
  expect_error(getsides("8", "2h"))
  expect_error(getsides("8", "2b2"))
  expect_error(getsides("8", "3v"))
  expect_error(getsides("8", "3h"))
  expect_equal(getsides("8", "4h"), 4)
  expect_equal(getsides("8", "4b2"), 4)

  expect_error(getsides("9", "1"))
  expect_error(getsides("9", "2v"))
  expect_error(getsides("9", "2h"))
  expect_error(getsides("9", "2b2"))
  expect_error(getsides("9", "3v"))
  expect_error(getsides("9", "3h"))
  expect_error(getsides("9", "4h"))
  expect_error(getsides("9", "4b2"))
  expect_error(getsides("123", "2b2"))
  expect_error(getsides("1", "foo"))
})

## Need x labels ==============
test_that("needxlabels",{
  expect_that(needxlabels("1", "1"), equals(TRUE))
  expect_that(needxlabels("2", "1"), equals(FALSE))

  expect_that(needxlabels("1", "2v"), equals(TRUE))
  expect_that(needxlabels("2", "2v"), equals(TRUE))

  expect_that(needxlabels("1", "2h"), equals(FALSE))
  expect_that(needxlabels("2", "2h"), equals(FALSE))
  expect_that(needxlabels("3", "2h"), equals(TRUE))
  expect_that(needxlabels("4", "2h"), equals(FALSE))

  expect_that(needxlabels("1", "2b2"), equals(FALSE))
  expect_that(needxlabels("2", "2b2"), equals(FALSE))
  expect_that(needxlabels("3", "2b2"), equals(TRUE))
  expect_that(needxlabels("4", "2b2"), equals(TRUE))
  expect_error(needxlabels("3", "foo"))
})

## dropbottomlabel ==============
test_that("dropbottomlabel",{
  expect_that(dropbottomlabel("1", "1"), equals(FALSE))
  expect_that(dropbottomlabel("2", "1"), equals(FALSE))

  expect_that(dropbottomlabel("1", "2v"), equals(FALSE))
  expect_that(dropbottomlabel("2", "2v"), equals(FALSE))

  expect_that(dropbottomlabel("1", "2h"), equals(TRUE))
  expect_that(dropbottomlabel("2", "2h"), equals(TRUE))
  expect_that(dropbottomlabel("3", "2h"), equals(FALSE))
  expect_that(dropbottomlabel("4", "2h"), equals(FALSE))

  expect_that(dropbottomlabel("1", "2b2"), equals(TRUE))
  expect_that(dropbottomlabel("2", "2b2"), equals(TRUE))
  expect_that(dropbottomlabel("3", "2b2"), equals(FALSE))
  expect_that(dropbottomlabel("4", "2b2"), equals(FALSE))

  expect_error(dropbottomlabel("4", "foo"))
})

## needgrid ==============
test_that("needgrid", {
  expect_that(needgrid("1", "1"), equals(TRUE))
  expect_that(needgrid("2", "1"), equals(FALSE))

  expect_that(needgrid("1", "2v"), equals(TRUE))
  expect_that(needgrid("2", "2v"), equals(TRUE))

  expect_that(needgrid("1", "2h"), equals(TRUE))
  expect_that(needgrid("2", "2h"), equals(FALSE))
  expect_that(needgrid("3", "2h"), equals(TRUE))
  expect_that(needgrid("4", "2h"), equals(FALSE))

  expect_that(needgrid("1", "2b2"), equals(TRUE))
  expect_that(needgrid("2", "2b2"), equals(TRUE))
  expect_that(needgrid("3", "2b2"), equals(TRUE))
  expect_that(needgrid("4", "2b2"), equals(TRUE))

  expect_error(needgrid("4", "foo"))
})

## tick adjustment (#100) ==============
test_that("tickadjustment", {
  expect_equal(tickadjustment("1"), 1)
  expect_equal(tickadjustment("2v"), 3/2) # seems like a weird bug in R, shouldn't be needed, but shows up better this way...
  expect_equal(tickadjustment("2h"), 2)
  expect_equal(tickadjustment("2b2"), 2)
  expect_equal(tickadjustment("3v"), 2) # seems like a weird bug in R, shouldn't be needed, but shows up better this way...
  expect_equal(tickadjustment("3h"), 3)
  expect_equal(tickadjustment("3b2"), 3)
  expect_equal(tickadjustment("4h"), 4)
  expect_equal(tickadjustment("4b2"), 4)
})

## Drop first x label ================

test_that("drop x label", {
  p <- arphitgg(data.frame(x=1:10,y=1:10), agg_aes(x=x,y=y), layout = "3v", showallxlabels = TRUE, dropxlabel = TRUE) + agg_line(panel = c("1","2","3"))
  expect_true(check_graph(p, "draw-panel-drop-x"))

  # Auto drop first label in time series because would overlap
  foo <- data.frame(date = seq(as.Date("2000-03-01"),length.out=39,by="quarter"),y=1:39)
  p <- arphitgg(foo, agg_aes(x=date,y=y),layout="2v")+agg_line(panel=c("1","2"))
  expect_true(check_graph(p,  "draw-panel-drop-first-ts-auto"))

  p <- arphitgg(foo, agg_aes(x=date,y=y),layout="2v",dropxlabel = FALSE)+agg_line(panel=c("1","2"))
  expect_true(check_graph(p,  "draw-panel-drop-first-ts-override"))

  foo <- data.frame(date = seq(as.Date("2000-03-01"),length.out=18,by="quarter"),y=1:18)
  p <- arphitgg(foo, agg_aes(x=date,y=y),layout="2v")+agg_line(panel=c("1","2"))
  expect_true(check_graph(p,  "draw-panel-drop-first-ts-auto-not-required"))
})

## Formatting of y labels ==================

test_that("Formatting y labels", {
  #Many decimal points
  p <- arphitgg()+agg_ylim(0,3e-6,4)
  expect_true(check_graph(p, "draw-panel-ylabel-format-decimals"))

  # Thousands separator #359
  p <- arphitgg()+agg_ylim(0,30000,4)
  expect_true(check_graph(p, "draw-panel-ylabel-format-thousands"))
})
