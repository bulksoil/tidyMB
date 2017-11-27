#' Grab Metadata
#'
#' This function will take the long table and turn it back into a matrix with OTUs by Sample IDs.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @param formula The formula to feed into adonis
#' @return tibble of OTUs by Sample IDs
#' @export
#' @examples
#' grab_metadata()

grab_metadata <- function(x, samples = "SampleID", otus = "variable"){
	to_drop <- otus
 	metadata <- x %>% 
		dplyr::ungroup() %>% 
		purrr::discard(is.double) %>% 
		dplyr::select_(.dots = paste("-", to_drop)) %>% 
		dplyr::distinct()

	return(metadata)
}