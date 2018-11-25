context("Autolabel selection")

distance <- seq(from=0.25,by=0.5,to=2.25)
los <- c(FALSE,TRUE)

test_candidates <- function(a_tibble, b_d, b_other, b_los) {
  fake_candidates <- dplyr::add_row(a_tibble, x="b",y="b",distance=b_d,los=b_los,next_closest=b_other,selection_group=assign_selection_group(b_d,b_other,b_los))
  expect_true(all(!is.na(fake_candidates$selection_group)))
}

loop_los <- function(a_tibble, b_d, b_other) sapply(los, function(b_los) test_candidates(a_tibble,b_d,b_other,b_los))
loop_other <- function(a_tibble, b_d) sapply(distance, function(b_other) loop_los(a_tibble, b_d, b_other))

for (a_d in distance) {
  for (a_other in distance) {
    for (a_los in los) {
      a_tibble <- tibble::tibble(x="a",y="a",distance=a_d,los=a_los,next_closest=a_other,selection_group=assign_selection_group(a_d,a_other,a_los))
      sapply(distance, function(b_d) loop_other(a_tibble, b_d))
    }
  }
}
