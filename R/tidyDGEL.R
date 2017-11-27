#' Create a DGEL object from tidy table
#'
#' This function will take the long table and return a DGEL object either from edgeR or DESeq2. At the moment, it can only do simple designs (limited to one factor).

#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @param dist The distance metric options from vegan's vegdist(). Defaults to bray
#' @param formula The formula to feed into adonis
#' @return tibble of OTUs by Sample IDs
#' @export
#' @examples
#' widen()

tidyDGEL <- function(x, method = "edgeR", samples = "SampleID", otus = "variable", value = "value", group){

	wide_table <- tidyMB::widen(x, samples = samples, otus = otus, value = value)
	metadata <- tidyMB::grab_metadata(x, samples = samples, otus = otus)

	if meth
	y = DESeq2::DESeqDataSetFromMatrix(countData = t(wide_table %>% dplyr::select_(-samples)), 
                             			colData = metadata %>% dplyr::select_(group, SampleID),
                             			design = ~ group)


	y = edgeR::DGEList(counts = t(wide_table %>% dplyr::select_(-samples)), group = metadata %>% pull_(group))

  return(y)
}

```{r}

```

