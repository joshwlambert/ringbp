#' Set up initial cases for branching process
#'
#' @param initial_cases a non-negative `integer` scalar: number of initial
#'   or starting cases which are all assumed to be missed by contact tracing
#'   (i.e. tracing ascertainment = 0).
#' @inheritParams outbreak_step
#'
#' @return `data.table` of cases in outbreak so far. `data.table` columns are:
#' * `$exposure`: `numeric`
#' * `$asymptomatic`: `logical`
#' * `$caseid`: `integer`
#' * `$infector`: `numeric`
#' * `$traced`: `logical`
#' * `$onset`: `numeric`
#' * `$new_cases`: `integer`
#' * `$self_isolate`: `logical`
#' * `$isolated_time`: `numeric`
#' * `$sampled`: `logical`
#'
#' The returned `data.table` also carries a `test_quota` attribute: a
#' `data.table` (`day`, `tests_remaining`) with the test capacity remaining
#' after index-case testing, by day. Pass it on as
#' `interventions$test_quota` before the first [outbreak_step()] call so
#' capacity already used on index cases carries over (see [outbreak_model()]).
#' @autoglobal
#' @export
#' @importFrom data.table data.table
#' @importFrom stats runif
#'
#' @examples
#' delays <- delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#' )
#' event_probs <- event_prob_opts(
#'   asymptomatic = 0,
#'   presymptomatic_transmission = 0.15,
#'   symptomatic_traced = 0
#' )
#' interventions <- intervention_opts()
#'
#' # generate initial cases
#' case_data <- outbreak_setup(
#'   initial_cases = 5,
#'   delays = delays,
#'   event_probs = event_probs,
#'   interventions = interventions
#' )
#' case_data
outbreak_setup <- function(initial_cases, delays, event_probs, interventions) {

  checkmate::assert_number(initial_cases, lower = 1, finite = TRUE)
  checkmate::assert_class(delays, "ringbp_delay_opts")
  checkmate::assert_class(event_probs, "ringbp_event_prob_opts")
  checkmate::assert_class(interventions, "ringbp_intervention_opts")

  # Set up table of initial cases
  case_data <- data.table(
    exposure = 0, # Exposure time of 0 for all initial cases
    asymptomatic = runif(initial_cases) < event_probs$asymptomatic,
    caseid = seq_len(initial_cases), # set case id
    infector = 0,
    traced = FALSE,
    onset = delays$incubation_period(initial_cases),
    new_cases = NA_integer_,
    self_isolate = FALSE,
    isolated_time = Inf,
    sampled = FALSE,
    test_positive = FALSE
  )

  # provisional isolation time for symptomatic index cases, as if a test
  # were available and positive; corrected below once test capacity has
  # been allocated and results drawn (see sample_testing())
  case_data[
    asymptomatic == FALSE & self_isolate == FALSE,
    isolated_time := onset + delays$onset_to_isolation(.N)
  ]

  # index cases compete for the same daily test quota as later generations
  # (see sample_testing()); each case is debited on its own provisional
  # isolated_time (its detection day)
  eligible_idx <- which(is.finite(case_data$isolated_time))
  day_max <- ceiling(max(case_data$isolated_time[eligible_idx], 0))
  day_seq <- 0:day_max
  test_quota <- data.table(
    day = day_seq,
    tests_remaining = interventions$test_capacity(day_seq, initial_cases)
  )
  if (length(eligible_idx) > 0) {
    tested <- allocate_tests(
      day = as.integer(floor(case_data$isolated_time[eligible_idx])),
      test_quota = test_quota
    )
    case_data[
      eligible_idx[tested],
      test_positive := runif(.N) <= interventions$test_sensitivity(onset)
    ]
  }

  # isolation requires a positive, allocated test; unallocated cases keep
  # the FALSE placeholder, same as a false-negative result
  case_data[test_positive == FALSE, isolated_time := Inf]
  case_data[, test_positive := NULL]

  data.table::setattr(case_data, "test_quota", test_quota)
  case_data[]
}
