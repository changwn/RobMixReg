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


#' RobMixReg package built-in breast cancer multi-omics data.
#'
#' A BRCA dataset from RobMixReg package. This BRCA cancer dataset has two types of sequence data inclcuding ATAC-seq and RNA-seq. The vignette example shows that using robust regression, we can identify the outlier mutually based on ATAC-seq and RNA-seq.
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{a1}{RNA-seq vector 1}
#'   \item{a2}{RNA-seq vector 2}
#'   \item{a3}{RNA-seq vector 3}
#'   \item{b1}{ATAC-seq vector 1}
#'   \item{b2}{ATAC-seq vector 2}
#'   \item{b3}{ATAC-seq vector 3}
#' }
"BRCA_ATAC_RNA_seq"

#' RobMixReg package built-in BRCA source data.
#'
#' The list which contain all the information to generate variables used in the real application.
#'
#' @format A list whose length is 4:
#' \describe{
#'   \item{ATACseq_marker}{Three cell type marker genes}
#'   \item{RNAseq_marker}{Three cell type marker genes}
#'   \item{data_RNAseq_selected}{RNAseq sequencing data}
#'   \item{data_ATACseq_selected}{ATACseq sequencing data}
#' }
"BRCA_source"

