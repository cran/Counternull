% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_counternull_distribution.R
\name{create_counternull_distribution}
\alias{create_counternull_distribution}
\title{Creates a counternull distribution for a given value}
\usage{
create_counternull_distribution(
  sample_data,
  extreme,
  rand_matrix,
  permutation_counter_function,
  counternull_value,
  test_stat,
  variable,
  iterations,
  pairs
)
}
\arguments{
\item{sample_data}{Sample data set. Data should have column indicating
exposure (1) or non exposure (0) for each group (row) that is measured. Each
measured outcome (variable) should be represented by an additional column.}

\item{extreme}{Direction which is defined as more extreme for test statistic
in distribution (0 for less or 1 for greater)}

\item{rand_matrix}{Matrix with unique randomizations for exposure
assignment}

\item{permutation_counter_function}{Function used to create permutation
vector for counternull distribution}

\item{counternull_value}{Number to test out as counternull value}

\item{test_stat}{Observed test statistic.}

\item{variable}{Variable measured Format: sample_data$column}

\item{iterations}{Number of unique arrangements of exposure assignments
used to generate distribution (At most the number of rows in rand_matrix)}

\item{pairs}{Number of pairs of units there are to measure in dataset
(One pair = control unit + experimental unit)}
}
\value{
Counternull test statistics (Vector)
}
\description{
Resamples data to create counternull distribution.
Calculate and prints P-value. Returns vector with counternull distribution
data points (test statistics created from resampling). Observed test
statistic is indicated in distribution using dashed black line. No effect is
indicated with gray dashed line. Counternull value is indicated with red
dashed line.
}
\examples{
create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_counter_diff_means,
-3323,find_test_stat_diff_means(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)

create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_counter_t,
-3127,find_test_stat_t(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)

create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_counter_paired_t,
-3127,find_test_stat_paired_t(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
}
