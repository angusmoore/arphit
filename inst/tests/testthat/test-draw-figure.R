# - left-right padding
# - countsrclines
# - counttitlelines
# - getfigsize
# - handle layout
# not sure how to test figuresetup and createfigure - probably just part of integration tests?


expect_error(finddevice("abc.xyz"))
expect_that(finddevice("abc.png"), equals("png"))
expect_that(finddevice("abc.pdf"), equals("pdf"))
expect_that(finddevice("abc.emf"), equals("emf"))
expect_that(finddevice(NULL), equals(NULL))

# test that each of the filenames output the right type of file
randomdata <- ts(data.frame(x1 = rnorm(12)), start = c(2000,1), frequency = 4)
for (suffix in c("png","pdf","emf")) {
  file <- paste("foo.", suffix, sep = "")
  arphit.tsgraph(randomdata, filename = file)
  # test exists
  expect_that(file.exists(file), is_true())
  # remove
  file.remove(file)
}
