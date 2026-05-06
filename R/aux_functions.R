#' Convert symptom onset times to generation times
#'
#' Samples generation times from a skew-normal distribution based on relative
#' symptom onset times (`symptom_onset_time` - `exposure_time`), ensuring all
#' generation times are at least `latent_period`. The location parameter of the
#' skew-normal distribution is set to the relative symptom onset times.
#'
#' @param symptom_onset_time a positive `numeric` vector: symptom onset time(s)
#'   of the infector(s) in the case data. The symptom onset times are generated
#'   by sampling from the incubation period.
#' @param exposure_time a non-negative `numeric` vector: time of exposure of
#'   the infector(s) in the case data. Used to convert symptom onset in absolute
#'   time to relative time for each infectee. Default is for all exposure
#'   times to be 0.
#' @param alpha a `numeric` scalar: skew parameter of the skew-normal
#'   distribution. Used to model the relationship between incubation period and
#'   generation time.
#' @inheritParams delay_opts
#'
#' @return a `numeric` vector of generation times of equal length to the vector
#'   input to `symptom_onset_time`: the i-th element of the vector contains a
#'   sample from the generation time distribution of an individual with
#'   incubation period given by the i-th element of the `symptom_onset_time`
#'   vector. The lower bound of the output generation time vector is set by the
#'   `latent_period`, to prevent transmission before becoming infectious.
#' @export
#' @importFrom sn rsn
#'
#' @examples
#' incubation_to_generation_time(
#'   symptom_onset_time = c(1, 2, 3, 4, 1),
#'   alpha = 2
#' )
incubation_to_generation_time <- function(symptom_onset_time,
                                          exposure_time = rep(0, length(symptom_onset_time)),
                                          alpha,
                                          latent_period = 0) {

  checkmate::assert_numeric(symptom_onset_time, lower = 0, finite = TRUE)
  checkmate::assert_numeric(
    exposure_time, lower = 0, finite = TRUE, len = length(symptom_onset_time)
  )
  checkmate::assert_number(alpha, finite = TRUE)
  checkmate::assert_number(latent_period, lower = 0, finite = TRUE)

  # convert absolute to relative (individual) symptom onset time using exposure
  rel_symptom_onset_time <- symptom_onset_time - exposure_time

  # initialise generation time vector to trigger sampling loop
  gt <- rep(-Inf, times = length(rel_symptom_onset_time))
  # loop counter to stop infinite loop
  counter <- 0
  limit <- 1000
  resample_idx <- gt < latent_period
  n_resample <- sum(resample_idx)
  # ensure no negative or pre-infectious generation times
  while (n_resample && counter < limit) {
    gt[resample_idx] <- sn::rsn(
      n = n_resample,
      xi = rel_symptom_onset_time[resample_idx],
      omega = 2,
      alpha = alpha
    )
    resample_idx <- gt < latent_period
    n_resample <- sum(resample_idx)
    counter <- counter + 1
  }
  if (n_resample) {
    stop(
      "Unable to sample generation times satisfying `latent_period` >= ",
      "`incubation_period`.\nConsider reducing the `latent_period` or ",
      "checking parameter compatibility with the `incubation_period` ",
      "distribution.",
      call. = FALSE
    )
  }

  # convert generation time to absolute time and return
  gt + exposure_time
}

#' Estimate skew normal alpha parameter from proportion of presymptomatic
#' transmission
#'
#' @details Since there isn't any analytical expression for linking the two,
#' the value of alpha that corresponds to the given proportion presymptomatic
#' is obtained via numeric optimisation.
#'
#' @param presymptomatic_transmission a `numeric` scalar probability
#'   (between 0 and 1 inclusive): proportion of transmission that occurs
#'   before symptom onset.
#'
#' @return A `numeric` scalar: The `$minimum` output from [optimise()] to find
#'   the best `alpha` parameter to get the desired proportion of presymptomatic
#'   transmission.
#' @keywords internal
presymptomatic_transmission_to_alpha <- function(presymptomatic_transmission) {
  objective <- function(alpha) {
    # fix x, xi and omega for optimisation
    p_current <- sn::psn(x = 0, xi = 0, omega = 2, alpha = alpha)
    (p_current - presymptomatic_transmission)^2
  }
  # alpha domain is (-Inf, Inf), approximate with large numbers
  res <- stats::optimise(f = objective, interval = c(-1e5, 1e5))
  if (res$objective > 1e-5) {
    stop(
      "Estimating the `alpha` parameter from `presymptomatic_transmission` ",
      "did not converge."
    )
  }
  res$minimum
}

#' Outbreak control and extinction functions
#'
#' @description
#' `control_prob()`: Calculate proportion of runs that have a controlled
#'   outbreak
#'
#' `extinct_prob()`: Calculate proportion of runs with an extinct outbreak (a
#'   special case of `control_prob()` with `control_threshold = 0`)
#'
#' `detect_control()`: Calculate whether outbreaks were controlled or not
#'
#' `detect_extinct()`: Calculate whether outbreaks went extinct or not
#'
#' `extinct_prob()` and `detect_extinct()` were added to the package before
#'   `control_prob()` and `detect_control()`. The extinction functions are
#'   special cases of the more general control functions by fixing
#'   `control_threshold = 0`. They remain in the package for backwards
#'   compatibility and convenience to calculate extinction metrics.
#'
#' @details
#' The data passed to `scenario` has to be produced by [scenario_sim()].
#'   It cannot be produced by [outbreak_model()] as it requires the `sim`
#'   column, which is only appended in [scenario_sim()].
#'
#' ***Warning***: the output from [scenario_sim()] contains an `cap_cases`
#'   attribute which is used by [control_prob()], [extinct_prob()],
#'   [detect_control()], and [detect_extinct()], therefore if you modify the
#'   output of [scenario_sim()] before passing to any of the [control]
#'   functions be careful not to drop the attribute (e.g. from subsetting the
#'   `data.table`).
#'
#' @param scenario a `data.table`: weekly cases output by [scenario_sim()]
#' @param control_week,extinction_week an `integer` scalar, `integer` vector,
#'   or `NULL` (default): the week (zero-indexed) or set of weeks over which
#'   to test whether the outbreak has been controlled / went extinct.
#'
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
#' @param control_threshold an `integer` scalar: the threshold number of weekly
#'   cases to classify the outbreak as controlled. By default it is `0` which
#'   is outbreak extinction. Values greater than zero correspond
#'   to control defined under outbreak suppression strategies, e.g., if weekly
#'   cases do not exceed 100 (`control_threshold = 100`), then the outbreak is
#'   said to be under control in that period.
#'
#' @importFrom data.table setDT fifelse data.table
#'
#' @return
#' `control_prob()`: a single `numeric` with the probability of control
#'
#' `extinct_prob()`: a single `numeric` with the probability of extinction
#'
#' `detect_control()`: a `data.table`, with two columns `sim` and `control`,
#'   for a binary classification of whether the outbreak was controlled in each
#'   simulation replicate. `1` is an outbreak that was controlled, `0` if not.
#'
#' `detect_extinct()`: a `data.table`, with two columns `sim` and `extinct`,
#'   for a binary classification of whether the outbreak went extinct in each
#'   simulation replicate. `1` is an outbreak that went extinct, `0` if not.
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
#'     symptomatic_ascertained = 0.2
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
#' extinct_prob(res, extinction_week = max(res$week) - 1)
#'
#' # calculate extinction as no new cases between weeks 12 and 16 of the outbreak
#' extinct_prob(res, extinction_week = 12:16)
#'
#' # calculate probability of control as weekly cases at or below 5
#' # between weeks 12 and 16 of the outbreak
#' control_prob(res, control_week = 12:16, control_threshold = 5)
#'
#' # determine for each simulation whether weekly cases stayed at or below 5
#' # between weeks 12 and 16
#' detect_control(res, control_week = 12:16, control_threshold = 5)
#'
#' # calculate probability of control as weekly cases at or below 10
#' # from week 20 to the end of the outbreak
#' control_prob(res, control_week = 20, control_threshold = 10)
#' @name control
NULL

#' @rdname control
#' @export
control_prob <- function(scenario,
                         control_week = NULL,
                         control_threshold = 0) {

  extinct_runs <- detect_control(
    scenario = scenario,
    control_week = control_week,
    control_threshold = control_threshold
  )
  sum(extinct_runs$control) / max(scenario$sim)
}

#' @rdname control
#' @export
extinct_prob <- function(scenario,
                         extinction_week = NULL) {

  control_prob(
    scenario = scenario,
    control_week = extinction_week,
    control_threshold = 0
  )
}

#' @rdname control
#' @autoglobal
#' @export
detect_control <- function(scenario,
                           control_week = NULL,
                           control_threshold = 0) {

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
        "`control_week/extinction_week` not specified and `scenario` is ",
        "missing the `extinct` attribute.\n Use `scenario_sim()` to simulate ",
        "`scenario`, or specify `control_week/extinction_week`.",
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
    "`control_week/extinction_week` not in simulated outbreak data" =
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
    control = fifelse(all(weekly_cases <= control_threshold & cumulative < cap_cases), 1, 0)
  ), by = sim][]
}

#' @rdname control
#' @autoglobal
#' @export
detect_extinct <- function(scenario,
                           extinction_week = NULL) {

  extinct <- detect_control(
    scenario = scenario,
    control_week = extinction_week,
    control_threshold = 0
  )
  colnames(extinct) <- c("sim", "extinct")
  extinct
}

# The following function is copied from `testthat:::on_ci()` from the
# {testthat} package (version 3.2.3).
# It is licensed under the MIT license (see LICENSE.md).
# Copyright (c) [2023] [testthat auhors]
on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", unset = "FALSE")))

#' Control whether outbreak simulation continues stepping
#'
#' @description
#' Used in a while loop to determine whether the [outbreak_model()]
#'   continues to call [outbreak_step()], or to end the simulation.
#'
#' @inheritParams outbreak_step
#' @inheritParams outbreak_model
#'
#' @return a `logical` scalar: whether the outbreak is still active and should
#'   continue (`TRUE`) or if the outbreak is extinct or has reached a stopping
#'   criterion (`FALSE`).
#' @keywords internal
outbreak_continue <- function(case_data, sim) {

  latest_onset <- max(case_data$onset)
  total_cases <- nrow(case_data)
  extinct <- all(case_data$sampled)

  return(
    latest_onset < sim$cap_max_days &&
      total_cases < sim$cap_cases && !extinct
  )
}

