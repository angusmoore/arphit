assign_selection_group <- function(dist, next_dist, los) {
  if (dist < 0.5 & next_dist > 0.5 & los) return(1)
  if (dist < 0.5 & next_dist > dist & los) return(2)
  if (dist < 0.5 & next_dist <= dist & los) return(4)
  if (dist < 0.5 & next_dist > 0.5 & !los) return(6)
  if (dist < 0.5 & next_dist > dist & !los) return(7)
  if (dist < 0.5 & next_dist <= dist & !los) return(8)

  if (dist < 1 & next_dist > dist & los) return(3)
  if (dist < 1 & next_dist > dist & !los) return(9)
  if (dist < 1 & next_dist > 0.5 & next_dist <= dist & los) return(5)
  if (dist < 1 & next_dist > 0.5 & next_dist <= dist & !los) return(10)
  if (dist < 1 & next_dist < 0.5) return(12)

  if (dist < 1.5 & next_dist > dist & los) return(11)
  if (dist < 1.5 & next_dist > 0.5 & los) return(13)
  if (dist < 1.5 & next_dist < 0.5 & los) return(14)
  if (dist < 1.5 & next_dist > dist & !los) return(15)
  if (dist < 1.5 & next_dist > 0.5 & !los) return(16)
  if (dist < 1.5 & next_dist < 0.5 & !los) return(17)

  if (dist > 1.5 & next_dist > dist & los) return(18)
  if (dist > 1.5 & next_dist > dist & !los) return(19)
  if (dist > 1.5 & next_dist > 0.5) return(20)
  if (dist > 1.5 & next_dist < 0.5) return(21)
}


label_selection <- function(label_options) {
  best_group <-
    label_options$selection_group == min(label_options$selection_group)
  label_options <- label_options[best_group, ]

  label_options[rank(label_options$distance, ties.method = "first") == 1, ]
}
