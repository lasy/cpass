% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_cpass_data.R
\name{simulate_cycle}
\alias{simulate_cycle}
\title{Simulates CPASS data for a single cycle.}
\usage{
simulate_cycle(cycle_nb, baseline, pmdd_intensity, items, missingness_rate)
}
\arguments{
\item{cycle_nb}{(positive integer) The cycle number.}

\item{baseline}{(positive value in 1-6) Baseline level for severity of
problems for all items.}

\item{pmdd_intensity}{change from baseline during the pre-menstrual phase.}

\item{items}{(positive integers in 1-24) items for which there is
a variation in the pre-menstrual phase.}

\item{missingness_rate}{(positive value in 0-0.9) The fraction of missing
days.}
}
\value{
a \code{data.frame} providing the DRSP scores for each cycle day
(within the pre- and post- menstrual phase) and item.
}
\description{
This function simulates DRSP data for a single cycle
}
\seealso{
\code{simulate_cycle}
}
