#' Sample the offspring distributions for cases that can be in either the
#' _community_, _isolated_ or _asymptomatic_ states and transition between
#' states
#'
#' Samples contacts from the offspring distributions (see [offspring_opts()]),
#'   splits contacts into infections and uninfected traced contacts using the
#'   per-state `*_contact_prob_infect` probabilities, and adds the next
#'   generation of transmission events by reference to `case_data`.
#'   The generation times for each infector-infectee pair and the per-infector
#'   counts of uninfected contacts are returned from the function.
#'
#' @details The offspring distribution for a case in the community cannot simply
#'   be sampled from the `community` offspring distribution as it might become
#'   isolated before contacting some or all of those individuals.
#'   To account for cases that transition between states (for now only
#'   _community_ -> _isolated_) we draw contacts from both distributions, assign
#'   all contacts a generation time, and then discard the ones that have
#'   generation time <= isolation time (for those from the isolated offspring
#'   distribution) or generation time > isolation time (for those from the
#'   community offspring distribution), respectively. Each remaining contact
#'   independently becomes an infection with probability equal to the state's
#'   `*_contact_prob_infect`.
#'
#' @inheritParams outbreak_step
#' @inheritParams incubation_to_generation_time
#' @inheritParams delay_opts
#'
#' @autoglobal
#'
#' @return A `list` with two elements:
#'   * `exposure`: a named `numeric` vector of generation times for contacts
#'     that became infections, names are the infector `caseid`.
#'   * `uninfected_contacts`: a named `integer` vector with per-infector counts
#'     of uninfected traced contacts from symptomatic infectors (community and
#'     isolated states combined). Asymptomatic infectors are excluded because
#'     their contacts are never traced. Names are the infector `caseid`.
#'
#'   ***Note*** The `case_data` supplied to the function is modified by
#'   references, see [data.table::set()] for more information.
#' @keywords internal
sample_offspring <- function(case_data, offspring, alpha, latent_period) {

  # subset to cases in current generation
  new_cases <- case_data[sampled == FALSE]

  # logical vectors with the row index of asymptomatic and symptomatic cases
  asymptomatic_idx <- new_cases$asymptomatic
  symptomatic_idx <- !asymptomatic_idx

  # cases cannot be isolated at the start of their generation so both community
  # and isolated offspring distribution are sampled for community cases and
  # are subset by whether the generation time is before or after the isolation
  # time
  community <- offspring$community(sum(symptomatic_idx))
  isolated <- offspring$isolated(sum(symptomatic_idx))

  # asymptomatic cases are known from the start of their generation so their
  # offspring can be sampled directly from the asymptomatic offspring
  # distribution
  asymptomatic <- offspring$asymptomatic(sum(asymptomatic_idx))

  # get generation times for community and isolated contacts
  community_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[symptomatic_idx], community),
    exposure_time = rep(new_cases$exposure[symptomatic_idx], community),
    alpha = alpha,
    latent_period = latent_period
  )
  names(community_exposure) <- rep(new_cases$caseid[symptomatic_idx], community)
  isolated_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[symptomatic_idx], isolated),
    exposure_time = rep(new_cases$exposure[symptomatic_idx], isolated),
    alpha = alpha,
    latent_period = latent_period
  )
  names(isolated_exposure) <- rep(new_cases$caseid[symptomatic_idx], isolated)

  # if there is any transmission from asymptomatic cases get generation time
  if (length(asymptomatic) > 0) {
    asymptomatic_exposure <- incubation_to_generation_time(
      symptom_onset_time = rep(new_cases$onset[asymptomatic_idx], asymptomatic),
      exposure_time = rep(new_cases$exposure[asymptomatic_idx], asymptomatic),
      alpha = alpha,
      latent_period = latent_period
    )
    names(asymptomatic_exposure) <- rep(new_cases$caseid[asymptomatic_idx], asymptomatic)
  } else {
    # create asymptomatic_exposure for all exposure vector below (NULL dropped)
    asymptomatic_exposure <- NULL
  }

  # subset contact events in community and isolation based on infector
  # isolation time and contact exposure time
  contact_before_isolate <- community_exposure < rep(new_cases$isolated_time[symptomatic_idx], community)
  community_exposure <- community_exposure[contact_before_isolate]
  contact_after_isolate <- isolated_exposure > rep(new_cases$isolated_time[symptomatic_idx], isolated)
  isolated_exposure <- isolated_exposure[contact_after_isolate]

  # Bernoulli split: each retained contact becomes an infection with probability
  # given by the state's *_contact_prob_infect. Skip the runif() draw when
  # prob_infect == 1 to avoid perturbing the RNG state under the default model.
  community_infected <- if (offspring$community_contact_prob_infect == 1) {
    rep(TRUE, length(community_exposure))
  } else {
    runif(length(community_exposure)) < offspring$community_contact_prob_infect
  }
  isolated_infected <- if (offspring$isolated_contact_prob_infect == 1) {
    rep(TRUE, length(isolated_exposure))
  } else {
    runif(length(isolated_exposure)) < offspring$isolated_contact_prob_infect
  }
  asymptomatic_infected <- if (offspring$asymptomatic_contact_prob_infect == 1) {
    rep(TRUE, length(asymptomatic_exposure))
  } else {
    runif(length(asymptomatic_exposure)) < offspring$asymptomatic_contact_prob_infect
  }

  # uninfected traced contacts come from symptomatic infectors only.
  # contacts of asymptomatic infectors are not traced and so never consume tests
  uninfected_names <- c(
    names(community_exposure)[!community_infected],
    names(isolated_exposure)[!isolated_infected]
  )
  uninfected_contacts <- table(uninfected_names)

  # keep only infected contacts as exposures for the next generation
  exposure <- c(
    community_exposure[community_infected],
    isolated_exposure[isolated_infected],
    asymptomatic_exposure[asymptomatic_infected]
  )

  next_gen <- table(names(exposure))

  # assign next generation of cases by reference
  case_data[sampled == FALSE, new_cases := 0L]
  case_data[as.numeric(names(next_gen)), new_cases := next_gen]

  # return infected generation times and uninfected contact counts per infector
  list(
    exposure = exposure,
    uninfected_contacts = uninfected_contacts
  )
}
