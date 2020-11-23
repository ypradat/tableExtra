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
#'   \item{SBS1}{Single-base-substitution signature 1. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS1.tt>}
#'   \item{SBS10a}{Single-base-substitution signature 10a. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS10a.tt>}
#'   \item{SBS10b}{Single-base-substitution signature 10b. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS10b.tt>}
#'   \item{SBS11}{Single-base-substitution signature 11. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS11.tt>}
#'   \item{SBS12}{Single-base-substitution signature 12. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS12.tt>}
#'   \item{SBS13}{Single-base-substitution signature 13. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS13.tt>}
#'   \item{SBS14}{Single-base-substitution signature 14. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS14.tt>}
#'   \item{SBS15}{Single-base-substitution signature 15. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS15.tt>}
#'   \item{SBS16}{Single-base-substitution signature 16. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS16.tt>}
#'   \item{SBS17a}{Single-base-substitution signature 17a. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS17a.tt>}
#'   \item{SBS17b}{Single-base-substitution signature 17b. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS17b.tt>}
#'   \item{SBS18}{Single-base-substitution signature 18. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS18.tt>}
#'   \item{SBS19}{Single-base-substitution signature 19. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS19.tt>}
#'   \item{SBS20}{Single-base-substitution signature 20. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS20.tt>}
#'   \item{SBS2}{Single-base-substitution signature 2. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS2.tt>}
#'   \item{SBS21}{Single-base-substitution signature 21. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS21.tt>}
#'   \item{SBS22}{Single-base-substitution signature 22. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS22.tt>}
#'   \item{SBS23}{Single-base-substitution signature 23. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS23.tt>}
#'   \item{SBS24}{Single-base-substitution signature 24. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS24.tt>}
#'   \item{SBS25}{Single-base-substitution signature 25. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS25.tt>}
#'   \item{SBS26}{Single-base-substitution signature 26. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS26.tt>}
#'   \item{SBS27}{Single-base-substitution signature 27. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS27.tt>}
#'   \item{SBS28}{Single-base-substitution signature 28. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS28.tt>}
#'   \item{SBS29}{Single-base-substitution signature 29. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS29.tt>}
#'   \item{SBS30}{Single-base-substitution signature 30. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS30.tt>}
#'   \item{SBS3}{Single-base-substitution signature 3. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS3.tt>}
#'   \item{SBS31}{Single-base-substitution signature 31. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS31.tt>}
#'   \item{SBS32}{Single-base-substitution signature 32. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS32.tt>}
#'   \item{SBS33}{Single-base-substitution signature 33. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS33.tt>}
#'   \item{SBS34}{Single-base-substitution signature 34. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS34.tt>}
#'   \item{SBS35}{Single-base-substitution signature 35. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS35.tt>}
#'   \item{SBS36}{Single-base-substitution signature 36. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS36.tt>}
#'   \item{SBS37}{Single-base-substitution signature 37. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS37.tt>}
#'   \item{SBS38}{Single-base-substitution signature 38. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS38.tt>}
#'   \item{SBS39}{Single-base-substitution signature 39. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS39.tt>}
#'   \item{SBS40}{Single-base-substitution signature 40. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS40.tt>}
#'   \item{SBS4}{Single-base-substitution signature 4. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS4.tt>}
#'   \item{SBS41}{Single-base-substitution signature 41. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS41.tt>}
#'   \item{SBS42}{Single-base-substitution signature 42. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS42.tt>}
#'   \item{SBS43}{Single-base-substitution signature 43. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS43.tt>}
#'   \item{SBS44}{Single-base-substitution signature 44. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS44.tt>}
#'   \item{SBS45}{Single-base-substitution signature 45. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS45.tt>}
#'   \item{SBS46}{Single-base-substitution signature 46. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS46.tt>}
#'   \item{SBS47}{Single-base-substitution signature 47. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS47.tt>}
#'   \item{SBS48}{Single-base-substitution signature 48. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS48.tt>}
#'   \item{SBS49}{Single-base-substitution signature 49. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS49.tt>}
#'   \item{SBS5}{Single-base-substitution signature 5. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS5.tt>}
#'   \item{SBS50}{Single-base-substitution signature 50. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS50.tt>}
#'   \item{SBS51}{Single-base-substitution signature 51. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS51.tt>}
#'   \item{SBS52}{Single-base-substitution signature 52. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS52.tt>}
#'   \item{SBS53}{Single-base-substitution signature 53. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS53.tt>}
#'   \item{SBS54}{Single-base-substitution signature 54. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS54.tt>}
#'   \item{SBS55}{Single-base-substitution signature 55. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS55.tt>}
#'   \item{SBS56}{Single-base-substitution signature 56. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS56.tt>}
#'   \item{SBS57}{Single-base-substitution signature 57. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS57.tt>}
#'   \item{SBS58}{Single-base-substitution signature 58. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS58.tt>}
#'   \item{SBS59}{Single-base-substitution signature 59. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS59.tt>}
#'   \item{SBS6}{Single-base-substitution signature 6. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS6.tt>}
#'   \item{SBS60}{Single-base-substitution signature 60. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS60.tt>}
#'   \item{SBS7a}{Single-base-substitution signature 7a. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS7a.tt>}
#'   \item{SBS7b}{Single-base-substitution signature 7b. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS7b.tt>}
#'   \item{SBS7c}{Single-base-substitution signature 7c. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS7c.tt>}
#'   \item{SBS7d}{Single-base-substitution signature 7d. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS7d.tt>}
#'   \item{SBS8}{Single-base-substitution signature 8. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS8.tt>}
#'   \item{SBS9}{Single-base-substitution signature 9. See <https://cancer.sanger.ac.uk/cosmic/signatures/SBS/SBS9.tt>}
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
