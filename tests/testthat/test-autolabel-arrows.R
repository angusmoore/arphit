data <- data.frame(y=1:10,y2=2:11)
labelsmap <- list(y = "Y", y2 = "y2")
labellocations <- list(y = c(4,3),y2= c(9,4))
colors <- list(y = "red", y2 = "black")
panelnumber <-  "1"

plot(1:10, 1:10)

# Check actually adds arrows
expect_equal(length(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)), 1)
# is closest point
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$head.x, 7)
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$head.y, 8)
# right color and panel
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$color, "black")
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$panel, "1")

# Tail placements
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$panel, "1")

# Arrow beneath and furthest by y - tail above the label
labellocations <- list(y = c(4,3),y2 = c(9,4))
expect_gt(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.y, 4) # tail should start above
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.x, 9)

# Label above, furthest by y
labellocations <- list(y = c(4,7), y2 = c(7,9))
expect_lt(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.y, 7)
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.x, 4)

# label to left, furthest by x
labellocations <- list(y = c(-4,7), y2 = c(7,9))
expect_gt(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.x, -4)
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.y, 7)

# label to right, furthest by x
labellocations <- list(y = c(4,3),y2 = c(14,4))

expect_lt(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.x, 14)
expect_equal(addarrows(1:10, data, panels, labelsmap, labellocations, colors, panelnumber)[[1]]$tail.y, 4)
