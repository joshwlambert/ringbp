#' Plot function for \pkg{ringbp} simulated outbreak from [scenario_sim()]
#'
#' @param x a `data.table` with class `<ringbp_scenario_sim>`: the outbreak
#'   simulated by the \pkg{ringbp} model, returned by [scenario_sim()]
#' @param ... Extra arguments, not used. Will warn if extra arguments are
#'   supplied
#'
#' @return A `ggplot` object
#' @export
#'
#' @examples
#' offspring <- offspring_opts(
#'   community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'   isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
#'   asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
#' )
#' delays <- delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
#' )
#' event_probs <- event_prob_opts(
#'   asymptomatic = 0,
#'   presymptomatic_transmission = 0.3,
#'   symptomatic_ascertained = 0
#' )
#' interventions <- intervention_opts(quarantine = TRUE)
#' sim <- sim_opts(
#'   cap_max_days = 365,
#'   cap_cases = 2000
#' )
#' res <- scenario_sim(
#'   n = 5,
#'   initial_cases = 5,
#'   offspring = offspring,
#'   delays = delays,
#'   event_probs = event_probs,
#'   interventions = interventions,
#'   sim = sim
#' )
#' plot(res)
plot.ringbp_scenario_sim <- function(x, ...) {
  chkDots(...)
  ggplot2::ggplot(data = x) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = week, y = cumulative, col = as.factor(sim)
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous("Cumulative number of cases") +
    ggplot2::theme_bw()
}
