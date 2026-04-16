#' Sample who gets tested given the daily testing capacity
#'
#' Allocates tests to traced contacts of ascertained (detected) infectors,
#'   subject to the daily testing capacity. Both infected contacts (appearing
#'   as offspring in `prob_samples`) and uninfected traced contacts
#'   (`uninfected_contacts`) debit the daily quota. Contacts of infectors whose
#'   `isolated_time` is `Inf` (asymptomatic or otherwise undetected) are never
#'   tested.
#'
#' @param prob_samples A `data.table` with the new cases from one generation,
#'   produced by [outbreak_step()].
#' @param uninfected_contacts a named `integer`-like vector: per-infector
#'   counts of uninfected traced contacts, as produced by [sample_offspring()].
#'   Names are the infector `caseid`.
#' @param case_data a `data.table`: the full case data so far. Used to look up
#'   the `isolated_time` of each uninfected contact's infector.
#' @inheritParams outbreak_step
#'
#' @return A `list` with two elements:
#'   * `tested`: a `logical` vector of length `nrow(prob_samples)`, `TRUE`
#'      iff the infected offspring was allocated a test this generation.
#'   * `test_quota`: a two-column `data.table` with remaining test capacity
#'      by day, carried over to the next generation.
#' @keywords internal
sample_testing <- function(prob_samples,
                           uninfected_contacts,
                           case_data,
                           interventions) {

  # preallocate testing assuming capacity for everyone
  tested <- rep(TRUE, nrow(prob_samples))

  if (all(is.infinite(interventions$test_capacity(prob_samples)))) {
    return(list(
      tested = tested,
      test_quota = interventions$test_quota
    ))
  }

  # infected offspring eligible for testing: non-asymptomatic with a finite
  # provisional `isolated_time` (set in outbreak_step() before sample_testing
  # is called). The test quota is debited on the day the case is detected
  # (= floor(isolated_time)), which is either their own self-presentation day
  # or their infector's isolation day, whichever is earlier.
  infected_idx <- which(
    prob_samples$asymptomatic == FALSE &
      is.finite(prob_samples$isolated_time)
  )
  infected_dt <- data.table(
    day = as.integer(floor(prob_samples$isolated_time[infected_idx])),
    infected = TRUE,
    orig_idx = infected_idx
  )

  # uninfected traced contacts: look up their infector's isolation time and
  # expand to one row per contact
  if (length(uninfected_contacts) > 0) {
    uc_dt <- data.table(
      infector = as.numeric(names(uninfected_contacts)),
      n = as.integer(uninfected_contacts)
    )
    uc_dt[case_data,
          infector_isolation_time := i.isolated_time,
          on = c("infector" = "caseid")]
    uc_dt <- uc_dt[is.finite(infector_isolation_time) & n > 0]
    uninfected_dt <- uc_dt[
      rep(seq_len(.N), n),
      .(day = as.integer(floor(infector_isolation_time)),
        infected = FALSE,
        orig_idx = NA_integer_)
    ]
  } else {
    uninfected_dt <- data.table(
      day = integer(0), infected = logical(0), orig_idx = integer(0)
    )
  }

  eligible <- rbindlist(list(infected_dt, uninfected_dt), use.names = TRUE)

  # no contacts to test: return unchanged quota and all tested = FALSE
  if (nrow(eligible) == 0) {
    tested[] <- FALSE
    return(list(tested = tested, test_quota = interventions$test_quota))
  }

  # build quota over a day range that covers both offspring onsets and
  # infector isolation times. Query test_capacity via a stub with extended
  # onset so it vectorises over the correct day range
  day_max <- max(
    ceiling(max(prob_samples$onset, 0)),
    max(eligible$day),
    0L
  )
  capacity_samples <- copy(prob_samples[0L])
  capacity_samples <- rbind(
    capacity_samples,
    prob_samples[1L][, onset := day_max],
    fill = TRUE
  )
  data.table::setattr(
    x = capacity_samples, name = "N", value = attr(prob_samples, "N")
  )
  day_seq <- 0:day_max
  test_quota <- data.table(
    day = day_seq,
    tests_remaining = interventions$test_capacity(capacity_samples)
  )

  # splice in carry-over quota from previous generation
  test_quota[interventions$test_quota,
             on = "day",
             tests_remaining := i.tests_remaining]

  # allocate within each day. Randomise order so infected and uninfected
  # contacts compete fairly for the daily quota
  eligible[, rand := runif(.N)]
  data.table::setorder(eligible, day, rand)
  eligible[test_quota, on = "day", quota := i.tests_remaining]
  eligible[, is_tested := seq_len(.N) <= quota, by = day]

  # flip infected offspring that received a test to TRUE (all others FALSE)
  tested[] <- FALSE
  tested[eligible[infected == TRUE & is_tested == TRUE, orig_idx]] <- TRUE

  # debit quota by the total number of tests used (infected + uninfected)
  tests_used <- eligible[is_tested == TRUE, .N, by = day]
  test_quota[tests_used,
             on = "day",
             tests_remaining := pmax(0, tests_remaining - i.N)]

  list(
    tested = tested,
    test_quota = test_quota
  )
}
