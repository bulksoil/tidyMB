#' Grab Metadata
#'
#' This function will take a tidy dataframe and return the distinct information about each sample.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
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