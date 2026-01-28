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

  if (is.infinite(interventions$test_capacity)) {
    return(list(
      tested = tested,
      interventions = interventions
    ))
  }

  test_quota <- data.table(
    day = 0:max(prob_samples$onset),
    tests_remaining = interventions$test_capacity
  )

  # splice in/replace with test quota for remaining tests from other generations
  test_quota[interventions$test_quota, on = "day", tests_remaining := i.tests_remaining]

  # only symptomatic cases are tested on their symptom onset date
  onset_day <- prob_samples[asymptomatic == FALSE, floor(onset)]

  for (i in seq_along(onset_day)) {
    if (test_quota[day == onset_day[i], tests_remaining] > 0) {
     test_quota[day == onset_day[i], tests_remaining := tests_remaining - 1]
   } else {
     tested[i] <- FALSE
   }
  }

  interventions$test_quota <- test_quota

  list(
    tested = tested,
    interventions = interventions
  )
}
