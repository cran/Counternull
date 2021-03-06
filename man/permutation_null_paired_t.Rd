% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutation_null_paired_t.R
\name{permutation_null_paired_t}
\alias{permutation_null_paired_t}
\title{Creates Paired T statistic vector for null distribution}
\usage{
permutation_null_paired_t(rand_matrix, variable, iterations)
}
\arguments{
\item{rand_matrix}{Matrix with unique randomizations for exposure
assignment}

\item{variable}{Variable measured
Format: sample_data$column}

\item{iterations}{Numbers of unique arrangements of exposure assignments
used to generate distribution (At most the number of rows in rand_matrix)}
}
\value{
Vector with all generated test statistics in null distribution
}
\description{
Resamples data to create null distribution. Returns vector with test
statistics in null distribution.
}
\examples{
permutation_null_paired_t(rand_matrix_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
}
