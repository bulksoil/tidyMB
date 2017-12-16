#' Rarefy Long Data
#'
#' This function is a wrapper around Vegan's capscale function for performing PCoA on tidy (long) data.
#' @param samples The column header for your sample identifiers. Defaults to SampleID
#' @param otus The column header for your OTU identifiers. Defaults to variable
#' @param value The column header for the OTU abundances. Defaults to RA
#' @return A list containing the axes, the eigenvalues, and loadings (optional)
#' @keywords Rarefaction
#' @export
#' @examples
#' tidy_pcoa()

tidy_rarefy <- function(x, depth = NULL, samples = "SampleID", otus = "variable", value = "value") {

  x <- x %>% 
        group_by_(samples) %>% 
        summarise(depth = sum(`value`))

  if(!depth) {
    rare_depth <- min(x$depth)                
  }

  print(paste("Sampling at", depth, "reads per sample"))
  x <- x %>% filter(depth >= rare_depth)


  rare_values <- data.frame(table(sample(x$`otus`, rare_depth, replace = T, prob = x$`value`/x$depth)))
  names(rare_values) <- c(otus, "rare_value")
  return(suppressMessages(left_join(x, rare_values, by = "variable") %>% replace_na(list(rare_value = 0))))
}