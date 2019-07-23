context("Panels")

test_that("Non existent layout and panels", {
  # Error if provide panel id not in layout
  expect_error(print(arphitgg(data.frame(x=1:10,y=1:10)) + agg_line(agg_aes(x=x,y=y),panel="3")), "Your chosen layout (1) does not have a panel 3.", fixed = TRUE)

  # Error if invalid layout
  expect_error(print(arphitgg(layout = "foo")), "Unknown layout option foo. Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2.", fixed = TRUE)
})
