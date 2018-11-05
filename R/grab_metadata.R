#' Grab Metadata
#'
#' This function will take a tidy dataframe and return the distinct information about each sample.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param return_df Will return a dataframe instead of a tibble if TRUE. Defaults to FALSE
#' @return tibble of OTUs by Sample IDs
#' @export
#' @examples
#' grab_metadata()

grab_metadata <- function(x, samples = "SampleID", otus = "variable", value = "value", return_df = FALSE){
	message("Gathering metadata")
	to_drop <- c(otus, value)
	warning(paste("Removing the column ", to_drop, " from the metadata.\n", sep = ""))
 	metadata <- x %>% 
		dplyr::ungroup() %>% 
		#purrr::discard(is.double) %>% 
		#purrr::discard(is.integer) %>%
		dplyr::select_(.dots = paste("-", to_drop)) %>% 
		dplyr::distinct()

	if(return_df) {
		df_row_names <- metadata %>% dplyr::select_(samples) %>% pull()
		metadata <- metadata %>% dplyr::select_(.dots = paste("-", samples))
		metadata = as.data.frame(metadata)
		row.names(metadata) <- df_row_names
		return(metadata)
		} else {
			return(metadata)
		}
}