context("Autolabel collisions")

# Set up
# set parameters and draw a plot so that strheight works
graphics::par(family = "sans", xaxs = "i", yaxs = "i", ps = 20, lheight = 1)
plot(1:10,1:10)

# Line segment intersection
expect_true(linesegment.intersect(0,0,1,1,0,1,1,0))
expect_true(linesegment.intersect(10,10,10,0,0,4,20,4))
expect_false(linesegment.intersect(0,0,1,1,10,10,12,12))
expect_false(linesegment.intersect(0,0,1,1,10,10,12,NA))
expect_false(linesegment.intersect(0,0,1,1,NA,10,12,12))
expect_false(linesegment.intersect(NA,0,1,1,10,10,12,12))
expect_true(linesegment.intersect(0,0,1,1,0,1,1,1))

# Text bounding box
h <- strheight("Test", units = "user")
w <- strwidth("Test", units = "user")
bound <- text.bounding("Test", 5, 5, cex = 1)
padding_x <- LABELTEXTPADDING / graphics::par("pin")[1] * (graphics::par("usr")[2] - graphics::par("usr")[1])
padding_y <- LABELTEXTPADDING / graphics::par("pin")[2] * (graphics::par("usr")[4] - graphics::par("usr")[3])

expect_equal(bound$rx, 5 + w/2 + padding_x)
expect_equal(bound$lx, 5 - w/2 - padding_x)
expect_equal(bound$ty, 5 + h/2 + padding_y)
expect_equal(bound$by, 5 - h/2 - padding_y)

# Text collision
expect_true(text.collision("Text", 5, 5, 0, 0, 10, 10))
expect_true(text.collision("Text", 5, 5, 0, 5, 10, 5))
expect_true(text.collision("Text", 5, 5, 5, 0, 5, 10))
expect_true(text.collision("Text", 5, 5, 5, 5, 10, 10))
expect_false(text.collision("Text", 5, 5, 7, 7, 10, 10))
expect_false(text.collision("Text", 5, 5, 0, 10, 10, 10))
expect_false(text.collision("Text", 5, 5, 10, 0, 10, 10))

# Out of bounds
expect_false(outofbounds("Text", 5, 5, c(0,10), c(0,10)))
expect_false(outofbounds("Text", 9, 9, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 10, 5, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 5, 10, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 100, 5, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 5, 100, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 9.8, 5, c(0,10), c(0,10)))
expect_true(outofbounds("Text", 5, 9.8, c(0,10), c(0,10)))

# Grid collision
expect_false(grid.collision("Text", 5, 5, c(0,10), c(0, 10), 6))
expect_false(grid.collision("Text", 6, 5, c(0,10), c(0, 10), 6))
expect_true(grid.collision("Text", 6, 6, c(0,10), c(0, 10), 6))

# Series collision
foo <- data.frame(y1=1:10,y2=1)
expect_true(series.collision("Text", 5, 5, 1:10, foo, "y1"))
expect_false(series.collision("Text", 5, 5, 1:10, foo, "y2"))
expect_false(series.collision("Text", 1, 5, 1:10, foo, "y1"))

# Label collisions
foo <- list("foo" = c(5,5))
labelsmap <- list("foo" = "Foo")
expect_true(label.collision("Text", 4.9, 5.1, foo, labelsmap))
expect_true(label.collision("Text", 5, 5, foo, labelsmap))
expect_false(label.collision("Text",8, 1, foo, labelsmap))

# shutdown
graphics.off()
