#' Adonis on Long Data
#'
#' This function is a wrapper around Vegan's adonis function for performing PERMANOVA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @param formula The formula to feed into adonis
#' @return S3 adonis object
#' @keywords adonis
#' @export
#' @examples
#' long_adonis()

long_adonis <- function(x, samples = "SampleID", otus = "variable", value = "RA", dist = "bray", formula) {
	
	metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus)
	wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value)

	metadata <- metadata[match(row.names(metadata), row.names(wide_table)),]
	permanova <- vegan::adonis(as.formula(paste("wide_table[,2:ncol(wide_table)] ~ ", formula, sep = "")), data = metadata, dist = dist)
	return(permanova$aov.tab)
}