context("Cross-platform test checking")

win <- list.files("../testdata/windows/")
linux <- list.files("../testdata/linux/")
used_tests <- list.files(tempdir(), ".*\\.png")

test_that("Reference files for linux and windows exist for all tests", {
  expect_false(any(!win %in% linux))
  if (any(!win %in% linux)) {
    cat(paste0("The following windows tests have no linux reference: ", win[!win %in% linux], "\n"))
  }

  expect_false(any(!linux %in% win))
  if (any(!linux %in% win)) {
    cat(paste0("The following linux tests have no windows reference: ", linux[!linux %in% win], "\n"))
  }

  skip("Blocked by #246")
  expect_false(any(!win %in% used_tests))
  if (any(!win %in% used_tests)) {
    cat(paste0("The following test reference files were never used: ", win[!win %in% used_tests], "\n"))
  }
})

