#' RobMixReg package built-in gaussian example data.
#'
#' A dataset generated from gaussian distribution in RobMixReg package.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{x}{x variable}
#'   \item{y}{y variable}
#'   \item{c}{cluster information}
#' }
"gaussData"


#' RobMixReg package built-in simulated example data.
#'
#' A simulation dataset from RobMixReg package. This simulation dataset is in dimension 2 and ground truth (include outliers label) of the cluster information also generated.
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{X1}{X1 variable}
#'   \item{X2}{X2 variable}
#'   \item{y}{y variable}
#'   \item{c}{cluster information}
#'   \item{outlier}{outlier indicator}
#' }
"simuData"



#' RobMixReg package built-in Colon cancer data.
#'
#' The list which contain all the information to generate variables used in the real application.
#'
#' @format A list whose length is 3:
#' \describe{
#'   \item{rnames}{A string contains the name of binding protein and epigenetic regulator.}
#'   \item{x3}{The gene expression profile of CREB3L1.}
#'   \item{y3}{The methylation profile of cg16012690 on 299 colon adenocarcinoma patients.}
#'   \item{x2}{x2}
#'   \item{y2}{y2}
#'   \item{x1}{x1}
#'   \item{y1}{y1}
#' }
"colon_data"


#' RobMixReg package built-in CCLE data.
#'
#' The list which contain all the information to generate variables used in the real application.
#'
#' @format A list whose length is 2:
#' \describe{
#'   \item{X}{Gene expression dataset.}
#'   \item{Y}{AUCC score.}
#' }
"CCLE_data"
