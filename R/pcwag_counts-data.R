#' Mutation counts attributed to each mutational signature in each tumour.
#'
#' Data from the mutational signature analysis of Alexandrov et al. on the PCAWG data.
#' The data is available as supplementary data to the paper "The repertoire of mutational signatures in human cancer"
#' on the Synapse data repository syn11738669
#' 
#' \code{pcawg_counts} contains the mutation counts as attributed by the SigProfiler algorithm on the
#' Single-Base-Substitution (SBSs) mutation catalogs of 2780 WGS tumours from the PCAWG.
#'
#' @format A data frame with 2780 rows and 68 variables
#' \describe{
#'   \item{Cancer.Types}{37 different cancer types}
#'   \item{Sample.Names}{Unique tumour identifiers}
#'   \item{Accuracy}{Cosine similarity between the tumour's mutational profile and the reconstructed mutational profile}
#'   \item{SBSXXX}{Each SBSXXX variable represents the counts attributed to the signature SBSXXX in the corresponding
#'     tumour}
#' }
#'
#' @docType data
#' @usage data(pcawg_counts)
#' @keywords datasets
#'
#' @references 
#' Alexandrov, L.B., Kim, J., Haradhvala, N.J. et al. The repertoire of mutational signatures in human cancer. 
#' Nature 578, 94–101 (2020). \url{https://doi.org/10.1038/s41586-020-1943-3}
#'
#' @source Synapse collaborative compute space, <https://www.synapse.org/#!Synapse:syn11804065>
#'
#' @examples
#' data(pcawg_counts)
"pcawg_counts"
