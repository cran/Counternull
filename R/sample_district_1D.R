#' @title Sample data for Police District1D revealing body camera assignment
#' and behavioral outcomes
#'
#' @description This CSV dataset is taken from a study measuring impact of
#' body cameras on police behavioral outcomes in Washington D.C. police
#' districts. It includes the body camera assignments for
#' police officers (142 pairs) in District1D as well as their ID numbers and
#' rates of different behavioral outcomes pre and post body camera assignment.
#' @format A table with 225 behavioral outcomes:
#' \describe{
#' \item{z}{Body Camera Assignment}
#' \item{block_id}{ID Number}
#' \item{district}{District}
#' \item{district_block_id}{District ID}
#' \item{columns 5-229}{Behavioral Outcomes}
#' }
#' @references \doi{10.1073/pnas.1814773116}
"sample_district_1D"
