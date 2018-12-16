#' Create a DGEL object from tidy table
#'
#' This function will take the long table and return a DGEL object either from edgeR or DESeq2. At the moment, it can only do simple designs (limited to one factor).
#' @param method The method for doing differential gene expression. Defaults to edgeR.
#' @param samples The column header for your sample identifiers. Defaults to SampleID.
#' @param otus The column header for your OTU identifiers. Defaults to variable.
#' @param value The column header for the OTU abundances. Defaults to RA.
#' @param group The variable that the user is trying to differentiate. Needs to be a column in the dataset.
#' @return tibble of OTUs by Sample IDs
#' @export
#' @examples
#' widen()

tidyDGEL <- function(x, method = "edgeR", samples = "SampleID", otus = "variable", value = "value", group_column = NULL, formula = NULL){
	
	wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value, return_df = T)
	metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus, value = value, return_df = T)
	metadata <- metadata[match(row.names(wide_table), row.names(metadata)),]

	if(method == "edgeR"){
		y = edgeR::DGEList(counts = t(wide_table[,2:ncol(wide_table)]), 
		                   group = metadata %>% dplyr::select_(group_column) %>% dplyr::pull())
	} else if(method == "DESeq2") {
		y = DESeq2::DESeqDataSetFromMatrix(countData = t(wide_table), 
		                                   colData = metadata,
		                                   design = formula(formula))
	} else {
		stop(paste("Wrong method supplied:\nDo not recognize ", method, sep = ""))
	}

  return(y)
}


