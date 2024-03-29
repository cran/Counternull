% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_null_rand.R
\name{create_null_rand}
\alias{create_null_rand}
\title{Create Null Randomization Distribution}
\usage{
create_null_rand(
  y,
  w,
  rand_matrix,
  test_stat = NULL,
  fun = NULL,
  alternative = NULL,
  bw = NULL
)
}
\arguments{
\item{y}{Vector of observed outcomes}

\item{w}{Vector indicating treatment assignments}

\item{rand_matrix}{Matrix with permutations for experiment assignments}

\item{test_stat}{Name of built in test statistic function. Provide
"diffmeans" for difference of means, "t" for t test, "paired-t" for paired
t test, and "cohens-d" for cohen's d test (optional).}

\item{fun}{Test statistic function (optional).}

\item{alternative}{Character string specifying alternative hypothesis.
Must be one of "two-sided" (default), "greater", or "less".}

\item{bw}{Bin width for histogram (optional)}
}
\value{
Class "null_rand" with 11 entries:
\describe{
  \item{null_dist}{Vector of permuted test statistics under the
  null hypothesis}
  \item{t_obs}{Observed test statistic}
  \item{counts}{Number of test statistics more extreme than observed test
  statistic}
  \item{pvalue}{Fisher-Exact P-value}
  \item{alternative}{Specified alternative}
  \item{rand_matrix}{Randomization matrix used to generate null distribution}
  \item{bin_width}{Specified bin width}
  \item{y}{Observed outcomes}
  \item{w}{Vector indicating treatment assignments}
  \item{test_stat}{Name of built in test statistic function}
  \item{fun}{Test statistic function}
  }
}
\description{
Generates null randomization distribution for a given test statistic.
}
\details{
Call summary on "null_rand" class to retrieve information on the null
randomization distribution. Call plot on "null_rand" class for visualization
of null randomization distribution.

Assignments must be indicated in arguments "w" and "rand_matrix"
using numeric 1 or 0.

Argument "rand_matrix" must have assignment permutations in each column
and must have the same number of rows as there are entries in "w".

One of either argument "test_stat" or "fun" must be specified.

Argument "fun" must take in two parameters (treated outcomes and
control outcomes) and returns a numeric test statistic value (scalar).
}
\examples{
y = sample_data$turn_angle
w = sample_data$w
n_r = create_null_rand(y, w, sample_matrix, test_stat = c("t"))
summary(n_r)
plot(n_r)
}
