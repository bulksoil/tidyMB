#' Constrained Ordination on Tidy Data
#'
#' This function is a wrapper around Vegan's capscale function for performing PCoA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @param formula The formula to feed into the capscale function
#' @return A list containing the axes, the eigenvalues, and loadings (optional)
#' @keywords PCoA
#' @export
#' @examples
#' tidy_cap()

tidy_cap <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray", formula = NULL){

  metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus, return_df = T)
  wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value, return_df = T)

  md_samples <- metadata %>% ungroup %>% dplyr::select(`samples`) %>% pull()
  wide_table <- wide_table[match(md_samples, row.names(wide_table)),]
  
  pc <- vegan::capscale(as.formula(paste("wide_table ~ ", formula, sep = "")), data = metadata, dist = dist)
  axes <- dplyr::bind_cols(metadata, dplyr::as_tibble(vegan::scores(pc, choices = c(1:5))$sites))
  eigen_vals <- vegan::eigenvals(pc) / sum(vegan::eigenvals(pc))

  return(list(axes = axes, eigen_vals = eigen_vals))
}