#' Allocate a fixed daily test quota across candidates competing for tests
#' on the same day
#'
#' Shared by [outbreak_setup()] (index cases) and [sample_testing()]
#'   (later generations): whenever a set of candidates each need a test on
#'   a given day, and the day's capacity may not stretch to all of them,
#'   this function decides who gets one.
#'
#' @param day an `integer` vector: the day each candidate would be tested
#'   on, one element per candidate.
#' @param test_quota a `data.table` with columns `day` and
#'   `tests_remaining`: the available daily quota. ***Note*** modified by
#'   reference to debit the tests allocated, see [data.table::set()].
#'
#' @return A `logical` vector the same length as `day`, `TRUE` iff that
#'   candidate was allocated a test.
#' @autoglobal
#' @keywords internal
allocate_tests <- function(day, test_quota) {

  # unlimited capacity across every day in test_quota: no need to build the
  # allocation machinery below, everyone is tested
  if (all(is.infinite(test_quota$tests_remaining))) {
    return(rep(TRUE, length(day)))
  }

  # randomise allocation order within each day so candidates compete fairly
  # for the day's quota
  eligible <- data.table(
    day = day, orig_idx = seq_along(day), rand = runif(length(day))
  )
  data.table::setorder(eligible, day, rand)
  eligible[test_quota, on = "day", quota := i.tests_remaining]
  eligible[, is_tested := seq_len(.N) <= quota, by = day]

  # debit the quota by the number of tests used on each day
  tests_used <- eligible[is_tested == TRUE, .N, by = day]
  test_quota[
    tests_used, on = "day", tests_remaining := pmax(0, tests_remaining - i.N)
  ]

  is_tested <- logical(length(day))
  is_tested[eligible$orig_idx] <- eligible$is_tested
  is_tested
}

#' Sample who gets tested given the daily testing capacity
#'
#' Allocates tests to symptomatic new cases (via `prob_samples`) and to
#'   uninfected traced contacts of symptomatic infectors, subject to the
#'   daily testing capacity in `interventions$test_capacity`. Both compete
#'   for the same daily quota. Contacts of asymptomatic or otherwise
#'   undetected infectors are never traced and so never consume capacity.
#'
#' @param prob_samples a `data.table`: the new cases from one generation,
#'   produced by [outbreak_step()], with a provisional `test_isolation_time`
#'   already set for cases eligible for the testing pathway (symptomatic,
#'   not self-isolating).
#' @param uninfected_contacts a named `integer`-like vector: per-infector
#'   counts of successfully-traced uninfected contacts, produced by
#'   [outbreak_step()] from [sample_offspring()]'s per-contact output after
#'   applying the tracing-ascertainment draw. Names are the infector
#'   `caseid`.
#' @param case_data a `data.table`: the full case data so far. Used to look
#'   up the `isolated_time` of each uninfected contact's infector.
#' @param N a `numeric` scalar: the current cumulative outbreak size, passed
#'   to `interventions$test_capacity()` for outbreak-size-dependent capacity.
#' @inheritParams outbreak_step
#'
#' @return A `list` with two elements:
#'   * `tested`: a `logical` vector of length `nrow(prob_samples)`, `TRUE`
#'      iff that case was allocated a test this generation.
#'   * `test_quota`: a two-column `data.table` (`day`, `tests_remaining`)
#'      with remaining test capacity by day, to carry over to the next
#'      generation.
#' @autoglobal
#' @keywords internal
sample_testing <- function(prob_samples,
                           uninfected_contacts,
                           case_data,
                           interventions,
                           N) {

  tested <- rep(FALSE, nrow(prob_samples))

  # cases eligible for the testing pathway: a finite `test_isolation_time`
  # was set in outbreak_step() for symptomatic, non-self-isolating cases.
  # The quota is debited on the day the case would be detected (its
  # provisional test_isolation_time), whether or not the test ultimately
  # comes back positive.
  infected_idx <- which(is.finite(prob_samples$test_isolation_time))
  infected_dt <- data.table(
    day = as.integer(floor(prob_samples$test_isolation_time[infected_idx])),
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
      list(day = as.integer(floor(infector_isolation_time)),
           infected = FALSE,
           orig_idx = NA_integer_)
    ]
  } else {
    uninfected_dt <- data.table(
      day = integer(0), infected = logical(0), orig_idx = integer(0)
    )
  }

  eligible <- rbindlist(list(infected_dt, uninfected_dt), use.names = TRUE)

  # build quota over a day range that covers both the testing-pathway days
  # and the uninfected-contact days, splicing in carry-over from the
  # previous generation where available. `interventions$test_quota` may be
  # absent (NULL) if this is called without going through outbreak_model()
  # (e.g. outbreak_step() used standalone before any quota exists), in
  # which case every day starts at full capacity.
  day_max <- max(ceiling(max(prob_samples$onset, 0)), eligible$day, 0L)
  day_seq <- 0:day_max
  test_quota <- data.table(
    day = day_seq,
    tests_remaining = interventions$test_capacity(day_seq, N)
  )
  if (!is.null(interventions$test_quota)) {
    test_quota[
      interventions$test_quota, on = "day", tests_remaining := i.tests_remaining
    ]
  }

  if (nrow(eligible) == 0) {
    return(list(tested = tested, test_quota = test_quota))
  }

  eligible[, is_tested := allocate_tests(day, test_quota)]
  tested[eligible[infected == TRUE & is_tested == TRUE, orig_idx]] <- TRUE

  list(tested = tested, test_quota = test_quota)
}
