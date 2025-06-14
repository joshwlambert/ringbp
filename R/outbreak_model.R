
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @inheritParams prop_presymptomatic_to_alpha
#' @param cap_max_days a positive `integer` scalar: stop the simulation when
#'   this many days is reached.
#' @param cap_cases a positive `integer` scalar: number of cumulative cases at
#'   which the branching process (simulation) was terminated
#'
#' @return `data.table` of cases by week, cumulative cases, and the effective
#' reproduction number of the outbreak. `data.table` columns are:
#' * `$week`: `numeric`
#' * `$weekly_cases`: `numeric`
#' * `$cumulative`: `numeric`
#' * `$effective_r0`: `numeric`
#' * `$cases_per_gen`: `list`
#' @autoglobal
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' set.seed(1)
#' out <- outbreak_model(
#'   initial_cases = 1,
#'   prop_ascertain = 0.2,
#'   cap_max_days = 350,
#'   cap_cases = 4500,
#'   r0_isolated = 0.5,
#'   r0_community = 2.5,
#'   r0_asymptomatic = 2.5,
#'   disp_isolated = 1,
#'   disp_community = 0.16,
#'   disp_asymptomatic = 0.16,
#'   prop_presymptomatic = 0.5,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_asymptomatic = 0,
#'   quarantine = FALSE
#' )
#' out
outbreak_model <- function(initial_cases,
                           r0_community, r0_isolated, r0_asymptomatic,
                           disp_community, disp_isolated, disp_asymptomatic,
                           incubation_period, prop_presymptomatic,
                           onset_to_isolation,
                           prop_ascertain, prop_asymptomatic,
                           cap_max_days, cap_cases,
                           quarantine = FALSE) {

  check_outbreak_input()


  # calculate alpha parameter from prop_presymptomatic
  alpha <- prop_presymptomatic_to_alpha(
    prop_presymptomatic = prop_presymptomatic
  )

  # Set initial values for loop indices
  total_cases <- initial_cases
  latest_onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(initial_cases = initial_cases,
                              incubation_period = incubation_period,
                              onset_to_isolation = onset_to_isolation,
                              prop_asymptomatic = prop_asymptomatic)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest_onset < cap_max_days & total_cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data = case_data,
                             disp_isolated = disp_isolated,
                             disp_community = disp_community,
                             disp_asymptomatic = disp_asymptomatic,
                             r0_isolated = r0_isolated,
                             r0_community = r0_community,
                             r0_asymptomatic = r0_asymptomatic,
                             incubation_period = incubation_period,
                             onset_to_isolation = onset_to_isolation,
                             prop_ascertain = prop_ascertain,
                             alpha = alpha,
                             quarantine = quarantine,
                             prop_asymptomatic = prop_asymptomatic)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total_cases <- nrow(case_data)
    latest_onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # Prepare output, group into weeks
  weekly_cases <- case_data[, week := floor(onset / 7)
                            ][, list(weekly_cases = .N), by = week
                              ]
  # maximum outbreak week
  max_week <- floor(cap_max_days / 7)
  # weeks with 0 cases in 0:max_week
  missing_weeks <- (0:max_week)[!(0:max_week %in% weekly_cases$week)]

  # add in missing weeks if any are missing
  if (length(missing_weeks > 0)) {
    weekly_cases <- data.table::rbindlist(list(weekly_cases,
                                               data.table(week = missing_weeks,
                                                          weekly_cases = 0)))
  }
  # order and sum up
  weekly_cases <- weekly_cases[order(week)
                               ][, cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]

  # Add effective R0
  weekly_cases <- weekly_cases[, `:=`(effective_r0 = mean(effective_r0_vect,
                                                          na.rm = TRUE),
                                        cases_per_gen = list(cases_in_gen_vect))]
  # return
  return(weekly_cases[])
}
