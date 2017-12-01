#' Convert long data to wide distance
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
#' wide_distance()

wide_distance <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray") {
	
	metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus)
	wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value)

	sample_labels <- metadata %>% pull(`samples`)

	dist_ <- vegan::vegdist(wide_table[,2:ncol(wide_table)], dist = dist)
	
	attr(dist_, "Labels") <- sample_labels

	return(dist_)
}