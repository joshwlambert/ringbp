#' Outbreak control and extinction functions
#'
#' @description
#' `control_prob()`: Calculate proportion of runs that have a controlled
#'   outbreak
#'
#' `detect_control()`: Calculate whether outbreaks were controlled or not
#'
#' `extinct_prob()`: Calculate proportion of runs with an extinct outbreak (a
#'   special case of `control_prob()` with `control_threshold = 0`)
#'
#' `detect_extinct()`: Calculate whether outbreaks went extinct or not (a
#'   special case of `detect_control()` with `control_threshold = 0`)
#'
#' @details
#' `extinct_prob()` and `detect_extinct()` were added to the package before
#'   `control_prob()` and `detect_control()`. The extinction functions are
#'   special cases of the more general control functions, fixing
#'   `control_threshold = 0`. They remain in the package for backwards
#'   compatibility and convenience to calculate extinction metrics.
#'
#' The data passed to `scenario` has to be produced by [scenario_sim()].
#'   It cannot be produced by [outbreak_model()] as it requires the `sim`
#'   column, which is only appended in [scenario_sim()].
#'
#' ***Warning***: the `$outbreak_ts` element of the [scenario_sim()] output
#'   carries a `cap_cases` attribute which is used by all four functions,
#'   therefore if you modify the output of [scenario_sim()] before passing it
#'   to any of these functions be careful not to drop the attribute (e.g.
#'   from subsetting the `data.table`).
#'
#' @param scenario a `list` output by [scenario_sim()], containing the
#'   `$outbreak_ts` `data.table` of weekly cases used to determine
#'   control/extinction
#' @param control_week,extinction_week an `integer` scalar, `integer` vector,
#'   or `NULL` (default): the week (zero-indexed) or set of weeks over which
#'   to test whether the outbreak has been controlled / went extinct.
#'   `control_week` is used by [control_prob()] and [detect_control()];
#'   `extinction_week` is used by [extinct_prob()] and [detect_extinct()].
#'   They behave identically and differ only in name and intent:
#'   `control_week` is paired with `control_threshold` and tests whether
#'   `weekly_cases <= control_threshold` across the specified weeks;
#'   `extinction_week` is the same test fixed to `control_threshold = 0`,
#'   i.e. `weekly_cases == 0` (extinction). Permitted forms:
#'   * `NULL` (default): use the pre-computed extinction status (`extinct`
#'     attribute) attached to the output of [scenario_sim()] — a
#'     true-extinction flag set when all infectious cases had the opportunity
#'     to transmit but no new cases were generated. Only valid when
#'     `control_threshold = 0`; when `control_threshold > 0` an explicit
#'     `control_week` is required and the default will error.
#'   * A single `integer`, e.g. `5`: tests the window from week `5` through
#'     the last simulated week. For example, `extinction_week = 5` tests
#'     whether no cases occur from week 5 onwards; `control_week = 5` with
#'     `control_threshold = 10` tests whether weekly cases stay at most 10
#'     from week 5 onwards.
#'   * An `integer` vector of length two, e.g. `c(5, 10)`: gives the
#'     lower and upper bounds (inclusive) of the week range to test. For
#'     example, `extinction_week = c(5, 10)` tests whether there were no new
#'     cases between weeks 5 and 10 (inclusive); `control_week = c(5, 10)`
#'     tests whether weekly cases stayed within `control_threshold` over the
#'     same window.
#'   * An `integer` vector of length _n_ (n >= 2), e.g. `12:16`: the exact
#'     set of weeks to test (12, 13, 14, 15, 16). For example,
#'     `extinction_week = 12:16` tests for no new cases across those weeks,
#'     and `control_week = 12:16` tests for weekly cases at or below
#'     `control_threshold` across them. Sequences are usually contiguous but
#'     non-contiguous integer vectors are allowed.
#'
#'   An outbreak that becomes controlled / extinct *before* the start of the
#'   window is still classified as controlled / extinct, because every week
#'   within the window then satisfies `weekly_cases <= control_threshold`. An
#'   outbreak that only becomes controlled / extinct *partway through* the
#'   window is not, because at least one in-window week exceeded the
#'   threshold. So a single `integer` for `extinction_week` reads naturally
#'   as "_has the outbreak gone extinct by week X_".
#'
#' @param control_threshold an `integer` scalar: the threshold number of
#'   weekly cases to classify the outbreak as controlled. By default it is
#'   `0` which is outbreak extinction. Values greater than zero correspond
#'   to control defined under outbreak suppression strategies, e.g. if
#'   weekly cases do not exceed 100 (`control_threshold = 100`), then the
#'   outbreak is said to be under control in that period.
#'
#' @importFrom data.table setDT fifelse data.table
#'
#' @return
#' `control_prob()`: a single `numeric` with the probability of control
#'
#' `detect_control()`: a `data.table`, with two columns `sim` and `control`,
#'   for a binary classification of whether the outbreak was controlled in
#'   each simulation replicate. `1` is an outbreak that was controlled, `0`
#'   if not.
#'
#' `extinct_prob()`: a single `numeric` with the probability of extinction
#'
#' `detect_extinct()`: a `data.table`, with two columns `sim` and `extinct`,
#'   for a binary classification of whether the outbreak went extinct in
#'   each simulation replicate. `1` is an outbreak that went extinct, `0` if
#'   not.
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   initial_cases = 1,
#'   offspring = offspring_opts(
#'     community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'     isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1)
#'   ),
#'   delays = delay_opts(
#'     incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'     onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#'   ),
#'   event_probs = event_prob_opts(
#'     asymptomatic = 0,
#'     presymptomatic_transmission = 0.5,
#'     symptomatic_traced = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#'
#' # calculate probability of extinction
#' extinct_prob(res)
#'
#' # determine if each outbreak simulation replicate has gone extinct
#' detect_extinct(res)
#'
#' # calculate extinction in the last 2 weeks of the simulated outbreak
#' # (i.e. the penultimate and last week of the outbreak)
#' extinct_prob(res, extinction_week = max(res$outbreak_ts$week) - 1)
#'
#' # calculate extinction as no new cases between weeks 12 and 16 of the outbreak
#' extinct_prob(res, extinction_week = 12:16)
#'
#' # calculate probability of control under a suppression strategy: weekly
#' # cases at or below 100 in the last 2 weeks of the outbreak
#' control_prob(
#'   res,
#'   control_week = max(res$outbreak_ts$week) - 1,
#'   control_threshold = 100
#' )
#' @name control
NULL

#' @rdname control
#' @export
control_prob <- function(scenario,
                         control_week = NULL,
                         control_threshold = 0) {

  control_runs <- detect_control(
    scenario = scenario,
    control_week = control_week,
    control_threshold = control_threshold
  )
  sum(control_runs$control) / max(control_runs$sim)
}

#' @rdname control
#' @autoglobal
#' @export
detect_control <- function(scenario,
                           control_week = NULL,
                           control_threshold = 0) {

  scenario <- scenario$outbreak_ts
  extinct <- attr(scenario, which = "extinct", exact = TRUE)

  if (is.null(control_week)) {
    if (control_threshold > 0) {
      stop(
        "`control_threshold` > 0 but `control_week` is `NULL`.\n Please ",
        "specify `control_week`.",
        call. = FALSE
      )
    }
    if (is.null(extinct)) {
      stop(
        "`control_week`/`extinction_week` not specified and `scenario` is ",
        "missing the `extinct` attribute.\n Use `scenario_sim()` to ",
        "simulate `scenario`, or specify `control_week`/`extinction_week`.",
        call. = FALSE
      )
    }
    message(
      "Calculating extinction using the extinction status from ",
      "the simulation."
    )
    return(
      data.table(
        sim = 1:max(scenario$sim),
        control = as.integer(extinct)
      )
    )
  }

  checkmate::assert_data_frame(scenario)
  checkmate::assert_integerish(control_week, min.len = 1)
  checkmate::assert_integerish(
    control_threshold, len = 1, lower = 0, any.missing = FALSE
  )

  if (length(control_week) == 1) {
    control_week <- control_week:max(scenario$week)
  } else if (length(control_week) == 2) {
    control_week <- min(control_week):max(control_week)
  }
  stopifnot(
    "`control_week`/`extinction_week` not in simulated outbreak data" =
      all(control_week %in% scenario$week)
  )
  if (control_threshold == 0) {
    message(
      "Calculating extinction as no new cases within weeks: ",
      min(control_week), " to ", max(control_week), " (inclusive)."
    )
  } else {
    message(
      "Calculating control as weekly cases <= ", control_threshold,
      " within weeks: ",
      min(control_week), " to ", max(control_week), " (inclusive)."
    )
  }

  cap_cases <- attr(scenario, which = "cap_cases", exact = TRUE)
  stopifnot(
    "`scenario` is missing the `cap_cases` attribute.
    Use `scenario_sim()` to simulate `scenario`" = !is.null(cap_cases)
  )

  scenario <- setDT(scenario)
  scenario <- scenario[week %in% control_week]
  scenario[, list(
    control = fifelse(
      all(weekly_cases <= control_threshold & cumulative < cap_cases), 1, 0
    )
  ), by = sim][]
}

#' @rdname control
#' @export
extinct_prob <- function(scenario, extinction_week = NULL) {
  control_prob(
    scenario = scenario,
    control_week = extinction_week,
    control_threshold = 0
  )
}

#' @rdname control
#' @export
detect_extinct <- function(scenario, extinction_week = NULL) {
  extinct <- detect_control(
    scenario = scenario,
    control_week = extinction_week,
    control_threshold = 0
  )
  colnames(extinct) <- c("sim", "extinct")
  extinct
}
