% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenario_sim.R
\name{scenario_sim}
\alias{scenario_sim}
\title{Run a specified number of simulations with identical parameters}
\usage{
scenario_sim(
  n,
  initial_cases,
  r0_community,
  r0_isolated,
  r0_asymptomatic,
  disp_community,
  disp_isolated,
  disp_asymptomatic,
  incubation_period,
  prop_presymptomatic,
  onset_to_isolation,
  prop_ascertain,
  prop_asymptomatic,
  cap_max_days,
  cap_cases,
  quarantine = FALSE
)
}
\arguments{
\item{n}{a positive \code{integer} scalar: number of simulations to run}

\item{initial_cases}{a nonnegative \code{integer} scalar: number of initial
or starting cases which are all assumed to be missed.}

\item{r0_community}{a positive \code{numeric} scalar: reproduction number for
non-isolated cases (must be >0)}

\item{r0_isolated}{a positive \code{numeric} scalar: reproduction number for
isolated cases (must be >0)}

\item{r0_asymptomatic}{a positive \code{numeric} scalar: reproduction number for
sub-clinical non-isolated cases (must be >0)}

\item{disp_community}{a positive \code{numeric} scalar: dispersion parameter for
non-isolated cases (must be >0)}

\item{disp_isolated}{a positive \code{numeric} scalar: dispersion parameter for
isolated cases (must be >0)}

\item{disp_asymptomatic}{a positive \code{numeric} scalar: dispersion parameter
for sub-clincial non-isolated cases (must be >0)}

\item{incubation_period}{a \code{function}: a random number generating
\code{function} that samples from incubation period distribution, the
\code{function} accepts a single \code{integer} argument specifying the number of
times to sample the incubation period (i.e. length of the \code{function}
output).}

\item{prop_presymptomatic}{a \code{numeric} scalar probability (between 0 and 1
inclusive): proportion of transmission that occurs before symptom onset.}

\item{onset_to_isolation}{a \code{function}: a random number generating
\code{function} that accepts a single \code{integer} argument specifying the
length of the \code{function} output.}

\item{prop_ascertain}{a \code{numeric} scalar probability (between 0 and 1
inclusive): proportion of infectious contacts ascertained by contact
tracing}

\item{prop_asymptomatic}{a \code{numeric} scalar probability (between 0 and 1
inclusive): proportion of cases that are completely asymptomatic
(subclinical)}

\item{cap_max_days}{a positive \code{integer} scalar: stop the simulation when
this many days is reached.}

\item{cap_cases}{a positive \code{integer} scalar: number of cumulative cases at
which the branching process (simulation) was terminated}

\item{quarantine}{a \code{logical} scalar: whether quarantine is in effect, if
\code{TRUE} then traced contacts are isolated before symptom onset; defaults to
\code{FALSE}}
}
\value{
A \code{data.table} object returning the results for multiple simulations using
the same set of parameters. The table has columns
\itemize{
\item week: The week in the simulation.
\item weekly_cases: The number of new cases that week.
\item cumulative: The cumulative cases.
\item effective_r0: The effective reproduction rate for the whole simulation
\item cases_per_gen: A list column with the cases per generation. This is repeated each row.
\item sim: Index column for which simulation.
}
}
\description{
Run a specified number of simulations with identical parameters
}
\examples{
res <- scenario_sim(
  n = 5,
  initial_cases = 5,
  cap_max_days = 365,
  cap_cases = 2000,
  r0_isolated = 0,
  r0_community = 2.5,
  disp_isolated = 1,
  disp_community = 0.16,
  prop_presymptomatic = 0.3,
  onset_to_isolation = \(x) rweibull(n = x, shape = 2.5, scale = 5),
  incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
  prop_asymptomatic = 0,
  prop_ascertain = 0,
  quarantine = TRUE
)
res
}
\author{
Joel Hellewell
}
