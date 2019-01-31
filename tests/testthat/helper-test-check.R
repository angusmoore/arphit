check_graph <- function(p, filename) {
  comp_location <- paste0(tempdir(), "/", filename, ".png")
  agg_draw(p, comp_location)

  if (.Platform$OS.type == "windows") {
    reference_loc <- paste0("../testdata/windows/", filename, ".png")
  } else {
    reference_loc <- paste0("../testdata/linux/", filename, ".png")
  }

  if (file.exists(reference_loc)) {
    reference <- magick::image_read(reference_loc)
    comparison <- magick::image_read(comp_location)
    # Check that image similarity greater than 97%. The same graph exported on
    # different machines have _slight_ differences for some reason.
    dist <- magick::image_compare_dist(comparison, reference)$distortion
    if (dist > 0.97) {
      return(TRUE)
    } else {
      cat(paste0("\n", filename, " does not match (distortion of ", round(dist,3),")\n"))
      return(FALSE)
    }
  } else {
    cat(paste0(filename, " reference image does not exist"))
    return(FALSE)
  }
}

create_test <- function(p, filename) {
  if (.Platform$OS.type == "windows") {
    reference_loc <- paste0("tests/testdata/windows/", filename, ".png")
  } else {
    reference_loc <- paste0("tests/testdata/linux/", filename, ".png")
  }
  agg_draw(p, filename = reference_loc)
}
