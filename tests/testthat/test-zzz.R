test_that("linting", {
  lintr::expect_lint_free()
})


win <- list.files("../testdata/windows/")
linux <- list.files("../testdata/linux/")
used_tests <- list.files(test_file_dir(), ".*\\.(png|gif)")

test_that("Reference files for linux and windows exist for all tests", {
  expect_false(any(missing_linux <- !win %in% linux))
  if (any(!win %in% linux)) {
    cat(paste0("The following windows tests have no linux reference: ",
               win[missing_linux], "\n"))
  }

  expect_false(any(!linux %in% win))
  if (any(missing_win <- !linux %in% win)) {
    cat(paste0("The following linux tests have no windows reference: ",
               linux[missing_win], "\n"))
  }

  expect_false(any(!win %in% used_tests))
  if (any(unused <- !win %in% used_tests)) {
    cat(paste0("The following test reference files were never used: ",
               win[unused], "\n"))
  }
})
