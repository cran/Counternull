% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutation_counter_diff_means.R
\name{permutation_counter_diff_means}
\alias{permutation_counter_diff_means}
\title{Creates difference of means permutation vector for counternull distribution}
\usage{
permutation_counter_diff_means(
  sample_data,
  rand_matrix,
  counternull_value,
  variable,
  iterations,
  pairs
)
}
\arguments{
\item{sample_data}{Sample data set. Data should have column indicating
exposure (1) or non exposure (0) for each group (row) that is measured. Each
measured outcome (variable) should be represented by an additional column.}

\item{rand_matrix}{Matrix with unique randomizations for exposure
assignment}

\item{counternull_value}{Number to test out as counternull value}

\item{variable}{Variable measured
Format: sample_data$column}

\item{iterations}{Numbers of unique arrangements of exposure assignments
used to generate distribution (At most the number of rows in rand_matrix)}

\item{pairs}{Number of pairs of units there are to measure in dataset
(One pair = control unit + experimental unit)}
}
\value{
Vector with all generated test statistics in null distribution
}
\description{
Resamples data to create counternull distribution. Returns vector with test
statistics in counternull distribution.
}
\examples{
permutation_counter_diff_means(sample_district_1DS, rand_matrix_1DS, -3323,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
}
