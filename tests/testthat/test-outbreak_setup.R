context("Test basic usage")

set.seed(20200410)

test_that("A basic sim setup returns the correct object", {
  incubation_period <- \(x) stats::rweibull(n = x, shape = 2.322737, scale = 6.492272)
  # delay distribution sampling function
  onset_to_isolation <- \(x) stats::rweibull(n = x, shape = 2, scale = 4)
  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = 5,
    incubation_period = incubation_period,
    onset_to_isolation = onset_to_isolation,
    prop_asymptomatic = 0
  )

  expect_equal(nrow(case_data), 5)
  expect_true(all(case_data$missed))
  expect_true(all(!case_data$asymptomatic))
})

test_that("asymptomatic arg works properly", {
  incubation_period <- \(x) stats::rweibull(n = x, shape = 2.322737, scale = 6.492272)
  # delay distribution sampling function
  onset_to_isolation <- \(x) stats::rweibull(n = x, shape = 2, scale = 4)
  # generate initial cases
  # All asymptomatics
  all_asymptomatic <- outbreak_setup(
    initial_cases = 5,
    incubation_period = incubation_period,
    onset_to_isolation = onset_to_isolation,
    prop_asymptomatic = 1
  )
  expect_true(all(all_asymptomatic$asymptomatic))

  # Mixed asympt dbinom(0, 10000, 0.5) = 0
  # With 10000 cases, probability of 0 symptomatic or 0 asympt is less than
  # machine precision
  mix <- outbreak_setup(
    initial_cases = 10000,
    incubation_period = incubation_period,
    onset_to_isolation = onset_to_isolation,
    prop_asymptomatic = 0.5
  )

  expect_length(unique(mix$asymptomatic), 2)
})
