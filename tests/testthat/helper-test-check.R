test_file_dir <- function() {
  if (!dir.exists("test_output/")) dir.create("test_output")
  return("test_output")
}

check_graph <- function(p, filename, max_distortion = 0.99) {
  comp_location <- paste0(test_file_dir(), "/", filename, ".png")
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
    if (dist >= max_distortion) {
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

check_gif <- function(gg_list, filename, max_distortion = 0.99) {
  comp_location <- paste0(test_file_dir(), "/", filename, ".gif")
  agg_slides(gg_list, comp_location)

  if (.Platform$OS.type == "windows") {
    reference_loc <- paste0("../testdata/windows/", filename, ".gif")
  } else {
    reference_loc <- paste0("../testdata/linux/", filename, ".gif")
  }

  if (file.exists(reference_loc) & file.exists(comp_location)) {
    reference <- magick::image_read(reference_loc)
    comparison <- magick::image_read(comp_location)
    # Check that image similarity greater than 97%. The same graph exported on
    # different machines have _slight_ differences for some reason.
    if (length(reference) != length(comparison)) {
      return(FALSE)
    }

    dist <- vector(mode = "list", length = length(reference))
    for (i in seq_along(reference)) {
      dist[i] <- magick::image_compare_dist(
        image = comparison[i],
        reference_image = reference[i]
      )$distortion
    }

    if (all(dist >= max_distortion)) {
      return(TRUE)
    } else {
      cat(paste0("\n", filename, " does not match (distortion of [", paste(round(dist,3), collapse = ", "), "])\n"))
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

delete_test <- function(filename) {
  reference_loc_w <- paste0("tests/testdata/windows/", filename, ".png")
  reference_loc_l <- paste0("tests/testdata/linux/", filename, ".png")
  file.remove(reference_loc_w)
  file.remove(reference_loc_l)
}

show_reference <- function(filename) {
  if (.Platform$OS.type == "windows") {
    reference_loc <- paste0("tests/testdata/windows/", filename, ".png")
  } else {
    reference_loc <- paste0("tests/testdata/linux/", filename, ".png")
  }
  magick::image_read(reference_loc)
}
