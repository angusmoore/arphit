assign_selection_group <- function(distance, next_closest, los) {
  if (distance < 0.5 & next_closest > 0.5 & los) return(1)
  if (distance < 0.5 & next_closest > distance & los) return(2)
  if (distance < 0.5 & next_closest <= distance & los) return(4)
  if (distance < 0.5 & next_closest > 0.5 & !los) return(6)
  if (distance < 0.5 & next_closest > distance & !los) return(7)
  if (distance < 0.5 & next_closest <= distance & !los) return(8)

  if (distance < 1 & next_closest > distance & los) return(3)
  if (distance < 1 & next_closest > distance & !los) return(9)
  if (distance < 1 & next_closest > 0.5 & next_closest <= distance & los) return(5)
  if (distance < 1 & next_closest > 0.5 & next_closest <= distance & !los) return(10)
  if (distance < 1 & next_closest < 0.5) return(12)

  if (distance < 1.5 & next_closest > distance & los) return(11)
  if (distance < 1.5 & next_closest > 0.5 & los) return(13)
  if (distance < 1.5 & next_closest < 0.5 & los) return(14)
  if (distance < 1.5 & next_closest > distance & !los) return(15)
  if (distance < 1.5 & next_closest > 0.5 & !los) return(16)
  if (distance < 1.5 & next_closest < 0.5 & !los) return(17)

  if (distance > 1.5 & next_closest > distance & los) return(18)
  if (distance > 1.5 & next_closest > distance & !los) return(19)
  if (distance > 1.5 & next_closest > 0.5) return(20)
  if (distance > 1.5 & next_closest < 0.5) return(21)
}


label_selection <- function(label_options) {
  label_options <- label_options[label_options$selection_group == min(label_options$selection_group), ]
  return(label_options[rank(label_options$distance, ties.method = "first") == 1, ])
}
