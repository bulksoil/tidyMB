#' Widen Long Data
#'
#' This function will take the long table and turn it back into a matrix with OTUs by Sample IDs.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param return_df Will return a dataframe instead of a tibble if TRUE. Defaults to FALSE
#' @return tibble of OTUs by Sample IDs
#' @export
#' @examples
#' widen()

widen <- function(x, samples = "SampleID", otus = "variable", value = "RA", return_df = FALSE){
	message("Converting to wide table.")
	wide_table <- x %>% 
		dplyr::select_(samples, otus, value) %>% 
		tidyr::spread_(otus, value, fill = 0) %>% 
	  ungroup()

	if(return_df) {
	  df_row_names <- wide_table %>% dplyr::select_(samples) %>% pull()
	  wide_table <- wide_table %>% dplyr::select_(.dots = paste("-", samples))
		wide_table = as.data.frame(wide_table)
		row.names(wide_table) <- df_row_names
		return(wide_table)
	} else {
		return(wide_table)
	}
}