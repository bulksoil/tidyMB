#' PCoA on Long Data
#'
#' This function is a wrapper around Vegan's capscale function for performing PCoA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @return A list containing the axes, the eigenvalues, and loadings (optional)
#' @keywords PCoA
#' @export
#' @examples
#' tidy_pcoa()

tidy_pcoa <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray", keep_loadings = F){

  metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus)
  wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value, return_df = TRUE)

  md_samples <- metadata %>% ungroup %>% dplyr::select(`samples`) %>% pull()
  wide_table <- wide_table[match(md_samples, row.names(wide_table)),]
  
  pc <- vegan::capscale(wide_table ~ 1, dist = dist)
  axes <- dplyr::bind_cols(metadata, dplyr::as_tibble(vegan::scores(pc, choices = c(1:5))$sites))
  eigen_vals <- vegan::eigenvals(pc) / sum(vegan::eigenvals(pc))

  ifelse(keep_loadings, 
    return(list(axes = axes, loadings = pc$CA$v, eigen_vals = eigen_vals)),
    return(list(axes = axes, eigen_vals = eigen_vals)))
}