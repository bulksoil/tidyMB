#' Beta dispersion on Tidy Data
#'
#' This function is a wrapper around Vegan's betadisper function for analyzing dispersion within groups on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @param group A column in the table that identifies the independent variable
#' @return A list containing the axes, the eigenvalues, and loadings (optional)
#' @keywords PCoA
#' @export
#' @examples
#' tidy_betadisper()

tidy_betadisper <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray", group = NULL){

  metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus, return_df = F)
  wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value, return_df = TRUE)
  
  dist_ <- vegan::vegdist(wide_table, dist = dist)

  bd <- vegan::betadisper(dist_, metadata$`group`)
                          
  return(bd)
}