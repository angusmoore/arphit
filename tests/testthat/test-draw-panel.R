# - get panel margins
# everything else can't be tested other than by looking at output

context("getlocation")
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

expect_error(getlocation("1", "foo"))

context("getsides")
expect_that(getsides("1", "1"), equals(2))
expect_that(getsides("1", "2h"), equals(2))
expect_that(getsides("1", "2v"), equals(2))
expect_that(getsides("1", "2b2"), equals(2))

expect_that(getsides("2", "1"), equals(4))
expect_that(getsides("2", "2h"), equals(4))
expect_that(getsides("2", "2v"), equals(4))
expect_that(getsides("2", "2b2"), equals(4))

expect_error(getsides("3", "1"))
expect_that(getsides("3", "2h"), equals(2))
expect_error(getsides("3", "2v"))
expect_that(getsides("3", "2b2"), equals(2))

expect_error(getsides("4", "1"))
expect_that(getsides("4", "2h"), equals(4))
expect_error(getsides("4", "2v"))
expect_that(getsides("4", "2b2"), equals(4))

expect_error(getsides("5", "1"))
expect_error(getsides("6", "2h"))
expect_error(getsides("6", "2v"))
expect_error(getsides("123", "2b2"))
expect_error(getsides("1", "foo"))

context("Need x labels")
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

context("dropbottomlabel")
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

context("needgrid")
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
