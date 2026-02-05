#' Sample who gets tested given the daily testing capacity
#'
#' @param prob_samples A `data.table` with the new cases from one generation,
#'   produced by [outbreak_step()].
#' @inheritParams outbreak_step
#'
#' @return A `logical` vector of length equal to the length of `prob_samples`,
#'   where `TRUE` is an individual gets tested and `FALSE` is an individual
#'   does not get tested to due limited testing capacity.
#' @keywords internal
sample_testing <- function(prob_samples, interventions) {

  # preallocate testing assuming capacity for everyone
  tested <- rep(TRUE, nrow(prob_samples))

  if (all(is.infinite(interventions$test_capacity(prob_samples)))) {
    return(list(
      tested = tested,
      test_quota = interventions$test_quota
    ))
  }

  day_seq <- 0:ceiling(max(prob_samples$onset))
  test_quota <- data.table(
    day = day_seq,
    tests_remaining = interventions$test_capacity(prob_samples)
  )

  # splice in/replace with test quota for remaining tests from other generations
  test_quota[interventions$test_quota, on = "day", tests_remaining := i.tests_remaining]

  # identify eligible cases and index by row number
  eligible <- copy(prob_samples)
  eligible <- eligible[
    , idx := .I
  ][
    asymptomatic == FALSE,
    .(idx = idx, day = as.integer(floor(onset)))
  ]

  if (nrow(eligible) > 0) {
    # join quota onto eligible cases
    eligible[test_quota, on = "day", quota := i.tests_remaining]

    # set individuals over test capacity for each day as FALSE
    eligible[, tested := seq_len(.N) <= quota, by = day]

    tested[prob_samples$asymptomatic] <- FALSE
    tested[eligible$idx] <- eligible$tested

    inf_per_day <- table(eligible$day)
    test_quota[
      day %in% as.numeric(names(inf_per_day)),
      tests_remaining := pmax(0, tests_remaining - inf_per_day)
    ]
  }

  list(
    tested = tested,
    test_quota = test_quota
  )
}
