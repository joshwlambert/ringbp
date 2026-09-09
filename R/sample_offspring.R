#' Sample the offspring distributions for cases that can be in either the
#' _community_, _isolated_ or _asymptomatic_ states and transition between
#' states
#'
#' Samples contacts from the offspring distributions (see [offspring_opts()]),
#'   splits contacts into infections and uninfected contacts using the
#'   per-state `*_contact_prob_infect` probabilities, and adds the next
#'   generation of transmission events by reference to `case_data`.
#'   The generation times for each infector-infectee pair, and each
#'   uninfected contact's infector and exposure time, are returned from the
#'   function -- tracing ascertainment for uninfected contacts (only a
#'   successfully-traced uninfected contact has a route into the testing
#'   queue) is applied by the caller, not here, see [outbreak_step()].
#'
#' @details A case's offspring cannot simply be sampled from a single
#'   offspring distribution, as the case might become isolated before
#'   contacting some or all of those individuals. To account for cases that
#'   transition between states (_community_ -> _isolated_ for symptomatic
#'   cases, _asymptomatic_ -> _isolated_ for asymptomatic cases) we sample
#'   contacts from both the pre-isolation and the post-isolation (`isolated`)
#'   distributions, assign all contacts a generation time, and then discard
#'   pre-isolation contacts with a generation time after the isolation time
#'   and post-isolation contacts with a generation time before it. Each
#'   remaining contact independently becomes an infection with probability
#'   equal to the infector's state `*_contact_prob_infect` (see
#'   [offspring_opts()]); contacts made once isolated share
#'   `isolated_contact_prob_infect` regardless of whether the infector reached
#'   isolation via the symptomatic or the asymptomatic pathway.
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
#'   * `uninfected_contacts`: a `data.table` with one row per uninfected
#'     contact from symptomatic infectors (community and isolated states
#'     combined), with columns `infector` (the infector's `caseid`) and
#'     `exposure` (that contact's own generation time). Asymptomatic
#'     infectors are excluded because their contacts are never traced. Not
#'     yet filtered by tracing ascertainment -- see [outbreak_step()], which
#'     applies a per-contact tracing draw before a contact can enter the
#'     testing queue.
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

  # asymptomatic cases transmit via the asymptomatic offspring distribution
  # before isolation and the isolated distribution after isolation, so (as for
  # symptomatic cases) both are sampled and later subset by the isolation time
  asymptomatic <- offspring$asymptomatic(sum(asymptomatic_idx))
  isolated_asymptomatic <- offspring$isolated(sum(asymptomatic_idx))

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

  # get generation times for asymptomatic contacts (asymptomatic + isolated);
  # incubation_to_generation_time() returns an empty vector for generations
  # with no asymptomatic cases
  asymptomatic_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[asymptomatic_idx], asymptomatic),
    exposure_time = rep(new_cases$exposure[asymptomatic_idx], asymptomatic),
    alpha = alpha,
    latent_period = latent_period
  )
  names(asymptomatic_exposure) <- rep(new_cases$caseid[asymptomatic_idx], asymptomatic)
  isolated_asymptomatic_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[asymptomatic_idx], isolated_asymptomatic),
    exposure_time = rep(new_cases$exposure[asymptomatic_idx], isolated_asymptomatic),
    alpha = alpha,
    latent_period = latent_period
  )
  names(isolated_asymptomatic_exposure) <-
    rep(new_cases$caseid[asymptomatic_idx], isolated_asymptomatic)

  # subset contact events based on infector isolation time and contact
  # exposure time: pre-isolation contacts are kept before the infector's
  # isolation time, post-isolation (isolated) contacts after it
  contact_before_isolate <- community_exposure < rep(new_cases$isolated_time[symptomatic_idx], community)
  community_exposure <- community_exposure[contact_before_isolate]
  contact_after_isolate <- isolated_exposure > rep(new_cases$isolated_time[symptomatic_idx], isolated)
  isolated_exposure <- isolated_exposure[contact_after_isolate]

  # the same split for asymptomatic cases: asymptomatic contacts before
  # isolation, isolated contacts after. When an asymptomatic case is never
  # isolated (isolated_time is Inf) all asymptomatic contacts are retained
  # and none of the isolated contacts are
  asympt_before_isolate <- asymptomatic_exposure < rep(new_cases$isolated_time[asymptomatic_idx], asymptomatic)
  asymptomatic_exposure <- asymptomatic_exposure[asympt_before_isolate]
  asympt_after_isolate <- isolated_asymptomatic_exposure > rep(new_cases$isolated_time[asymptomatic_idx], isolated_asymptomatic)
  isolated_asymptomatic_exposure <- isolated_asymptomatic_exposure[asympt_after_isolate]

  # Bernoulli split: each retained contact becomes an infection with
  # probability given by the infector's state `*_contact_prob_infect`. Skip
  # the runif() draw when prob_infect == 1 to avoid perturbing the RNG state
  # under the default (original branching process) model. Contacts made while
  # isolated share `isolated_contact_prob_infect`, whether the infector
  # reached isolation from the symptomatic or the asymptomatic pathway.
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
  isolated_asymptomatic_infected <- if (offspring$isolated_contact_prob_infect == 1) {
    rep(TRUE, length(isolated_asymptomatic_exposure))
  } else {
    runif(length(isolated_asymptomatic_exposure)) <
      offspring$isolated_contact_prob_infect
  }

  # uninfected contacts from symptomatic infectors only (their community and
  # isolated states) -- contacts of asymptomatic infectors are never traced
  # and so never consume tests, whether or not the infector is eventually
  # isolated. Kept as individual rows (not aggregated yet) so the caller can
  # apply a tracing-ascertainment draw per contact, using each contact's own
  # exposure time, before aggregating to a per-infector count: only a traced
  # uninfected contact has a route into the testing queue (see
  # outbreak_step.R) -- an untraced one has no notification prompting them
  # to seek a test, so never competes for capacity.
  uninfected_contacts <- data.table(
    infector = as.numeric(c(
      names(community_exposure)[!community_infected],
      names(isolated_exposure)[!isolated_infected]
    )),
    exposure = c(
      community_exposure[!community_infected],
      isolated_exposure[!isolated_infected]
    )
  )

  # keep only infected contacts as exposures for the next generation
  exposure <- c(
    community_exposure[community_infected],
    isolated_exposure[isolated_infected],
    asymptomatic_exposure[asymptomatic_infected],
    isolated_asymptomatic_exposure[isolated_asymptomatic_infected]
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
