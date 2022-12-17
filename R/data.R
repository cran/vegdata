#' Taxonomic hierarchy levels
#'
#' hierarchy of taxon levels
#'
#' @format A data frame with rows and 3 variables:
#' \describe{
#'   \item{rank}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{level}{Combinations. Same integer means, they will be combined}
#'   \item{Euro.Med}{taxon level name in http://www.europlusmed.org/ database}
#'   \item{description}{Explanation of level codes}
#' }
#' @export taxlevels
"taxlevels"

#' Layer combinations
#'
#' datasets with layer codes and how they should be combined
#'  in vegetation anlyses. lc.o: do not combine any layers
#'
#' @format A data frame with rows and 2 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#' @export lc.0
#'
"lc.0"

#' Layer combinations
#'
#' datasets with layer codes and how they should be combined
#'  in vegetation anlyses.
#'
#' @format A data frame with rows and 3 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#' @export lc.1
#'
"lc.1"

#' Layer combinations
#'
#' combine all layers
#'
#' @format A data frame with rows and 2 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#' @export lc.all
#'
"lc.all"

