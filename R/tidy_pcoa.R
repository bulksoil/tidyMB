#' PCoA on Long Data
#'
#' This function is a wrapper around Vegan's capscale function for performing PCoA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @return A list containing the axes and the eigenvalues
#' @keywords PCoA
#' @export
#' @examples
#' tidy_pcoa()

tidy_pcoa <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray"){
  to_drop <- otus
  metadata <- x %>% 
    dplyr::ungroup() %>% 
    purrr::discard(is.double) %>% 
    dplyr::select_(.dots = paste("-", to_drop)) %>% 
    dplyr::distinct()
  
  wide_table <- x %>% 
    dplyr::select_(samples, otus, value) %>% 
    tidyr::spread_(otus, value, fill = 0)
  
  pc <- vegan::capscale(log2(wide_table[,2:ncol(wide_table)] + 1) ~ 1, dist = dist)
  axes <- dplyr::bind_cols(metadata, dplyr::as_tibble(vegan::scores(pc, choices = c(1:5))$sites))
  eigen_vals <- vegan::eigenvals(pc) / sum(vegan::eigenvals(pc))
  
  return(list(axes = axes, eigen_vals = eigen_vals))
}