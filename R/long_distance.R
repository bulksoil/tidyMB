#' Convert long data to distance
#'
#' This function is a wrapper around Vegan's adonis function for performing PERMANOVA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @return S3 adonis object
#' @keywords adonis
#' @export
#' @examples
#' long_distance()

long_distance <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray") {
	
	metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus)
	wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value)

	dist_ <- as.matrix(vegan::vegdist(wide_table[,2:ncol(wide_table)], dist = dist))

	row.names(dist_) <- metadata %>% dplyr::pull(`samples`)
	colnames(dist_) <- metadata %>% dplyr::pull(`samples`)

	Var1 <- "Var1"
	Var2 <- "Var2"
	
	dist_long <- reshape2::melt(dist_) %>% 
		dplyr::inner_join(metadata, by = setNames(samples, Var1)) %>%
		dplyr::inner_join(metadata, by = setNames(samples, Var2))

	return(dist_long)
}