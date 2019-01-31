context("Shading")

data <- data.frame(x=1:10,y=1:10,y2=2:11,y3=3:12)
test_that("Basic shading", {
  p <- arphitgg(data)  +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) +
    agg_shading(y, y2)
  expect_true(
    check_graph(p, "shading-basic")
  )
})

test_that("Shading with duplicate series names", {
  # Duplicate names in other panels
  p <- arphitgg(data, layout = "2v")  +
    agg_line(agg_aes(x=x,y=y), panel = "1") +
    agg_line(agg_aes(x=x,y=y2), panel = "1") +
    agg_line(agg_aes(x=x,y=y), panel = "2") +
    agg_line(agg_aes(x=x,y=y3), panel = "2") +
    agg_shading(y3, y)
  expect_error(print(p), "Cannot construct shading from series y because its name is in more than one panel. Supply a panel identifier for the shading.")

  # Duplicate with panel ID
  p <- arphitgg(data, layout = "2v")  +
    agg_line(agg_aes(x=x,y=y), panel = "1") +
    agg_line(agg_aes(x=x,y=y2), panel = "1") +
    agg_line(agg_aes(x=x,y=y), panel = "2") +
    agg_line(agg_aes(x=x,y=y3), panel = "2") +
    agg_shading(y3, y, panel = "2")
  expect_true(check_graph(p, "shading-duplicate-panel-id"))
})

test_that("Error handling", {
  p <- arphitgg(data)  +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) +
    agg_shading(y3, y)
  expect_error(print(p), "Series y3 from your shading options is not a recognised series in panel 1.")

  p <- arphitgg(data)  +
    agg_line(agg_aes(x=x,y=y)) +
    agg_line(agg_aes(x=x,y=y2)) +
    agg_shading(y, y3)
  expect_error(print(p), "Cannot shade with series y3 because it does not exist in any panel.")
})
