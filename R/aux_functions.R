#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape a positive `numeric` scalar: shape parameter of Weibull
#'   distribution
#' @param dist_scale a positive `numeric` scalar: scale parameter of Weibull
#'   distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#'
dist_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp a positive `numeric` vector: samples from the incubation
#'   period distribution
#' @inheritParams outbreak_model
#'
#' @return a `numeric` vector of equal length to the vector input to `inc_samp`
#' @export
#' @importFrom sn rsn

inf_fn <- function(inc_samp = NULL, k = NULL) {

  out <- sn::rsn(n = length(inc_samp),
                 xi = inc_samp,
                 omega = 2,
                 alpha = k)

  out <- ifelse(out < 1, 1, out)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @return a single `numeric` with the probability of extinction
#' @export
#' @inheritParams detect_extinct
extinct_prob <- function(outbreak_df_week = NULL, cap_cases  = NULL, week_range = 12:16) {

  n_sim <- max(outbreak_df_week$sim)

  extinct_runs <- detect_extinct(outbreak_df_week, cap_cases, week_range)
  out <-  sum(extinct_runs$extinct) / n_sim

  return(out)
}


#' Calculate whether outbreaks went extinct or not
#' @author Joel Hellewell
#' @param outbreak_df_week a `data.table`: weekly cases produced by the
#'   outbreak model
#' @inheritParams outbreak_model
#' @param week_range a positive `integer` vector: giving the (zero indexed)
#'   week range to test for whether an extinction occurred.
#' @importFrom data.table as.data.table fifelse
#'
#' @return A `data.table`, with two columns `sim` and `extinct`, for a binary
#' classification of whether the outbreak went extinct in each simulation
#' replicate. `1` is an outbreak that went extinct, `0` if not.
#' @autoglobal
#' @export
#'
detect_extinct <- function(outbreak_df_week  = NULL, cap_cases  = NULL, week_range = 12:16) {

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim]
}
