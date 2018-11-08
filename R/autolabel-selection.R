assign_selection_groups <- function(l) {
  l$selection_group <- dplyr::case_when(
    l$distance < 0.5 & l$next_closest > 0.5 & l$los ~ 1,
    l$distance < 0.5 & l$next_closest > l$distance & l$los ~ 2,
    l$distance < 0.5 & l$next_closest <= l$distance & l$los ~ 4,
    l$distance < 0.5 & l$next_closest > 0.5 & !l$los ~ 6,
    l$distance < 0.5 & l$next_closest > l$distance & !l$los ~ 7,
    l$distance < 0.5 & l$next_closest <= l$distance & !l$los ~ 8,

    l$distance < 1 & l$next_closest > l$distance & l$los ~ 3,
    l$distance < 1 & l$next_closest > l$distance & !l$los ~ 9,
    l$distance < 1 & l$next_closest > 0.5 & l$next_closest <= l$distance & l$los ~ 5,
    l$distance < 1 & l$next_closest > 0.5 & l$next_closest <= l$distance & !l$los ~ 10,
    l$distance < 1 & l$next_closest < 0.5 ~ 12,

    l$distance < 1.5 & l$next_closest > l$distance & l$los ~ 11,
    l$distance < 1.5 & l$next_closest > 0.5 & l$los ~ 13,
    l$distance < 1.5 & l$next_closest < 0.5 & l$los ~ 14,
    l$distance < 1.5 & l$next_closest > l$distance & !l$los ~ 15,
    l$distance < 1.5 & l$next_closest > 0.5 & !l$los ~ 16,
    l$distance < 1.5 & l$next_closest < 0.5 & !l$los ~ 17,

    l$distance > 1.5 & l$next_closest > l$distance & l$los ~ 18,
    l$distance > 1.5 & l$next_closest > l$distance & !l$los ~ 19,
    l$distance > 1.5 & l$next_closest > 0.5 ~ 20,
    l$distance > 1.5 & l$next_closest < 0.5 ~ 21
  )
  return(l)
}


label_selection <- function(label_options) {
  if (any(is.finite(label_options$distance))) {
    label_options <- dplyr::filter(label_options, label_options$selection_group == min(label_options$selection_group))
    return(dplyr::filter(label_options,
                         rank(label_options$distance, ties.method = "first") == 1))
  } else {
    return(NULL)
  }
}
