
label_selection <- function(label_options) {
  if (any(is.finite(label_options$distance))) {
    return(dplyr::filter(label_options,
                         rank(label_options$distance, ties.method = "first") == 1))
  } else {
    return(NULL)
  }
}
