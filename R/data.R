#' Example of association file.
#'
#' Data obtained manually simulating real research data.
#' 
#' @usage data("association_file")
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{ProjectID}{The ID of the project you are working on}
#'   \item{PoolID}{ID of the pool you are working on}
#'   \item{SubjectID}{ID of the subject}
#'   \item{CompleteAmplificationID}{ID of the current amplification}
#' }
"association_file"

#' Example of integration matrix.
#'
#' Data obtained manually ismulating real research data.
#' 
#' @usage data("integration_matrix")
#'
#' @format A data frame with 140 rows and 5 variables:
#' \describe{
#'   \item{chr}{The chromosome number}
#'   \item{integration_locus}{Number of the base at
#'  which the viral insertion occurred}
#'   \item{strand}{Strand of the integration}
#'   \item{CompleteAmplificationID}{ID of the current amplification}
#'   \item{Value}{Sequence count for the IS}
#' }
"integration_matrix"