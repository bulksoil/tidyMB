# TidyMB
### An R package for performing microbiome analysis using a tidy framework

These are a set of tools and wrappers for performing microbial ecology analyses in R using a tidy framework.

This package is a work in progress and will be updated regularly to included new functions and features.

## Download
I think there are two ways to do it.

### Method 1
One is to clone this githup repo like so

`git clone https://github.com/bulksoil/tidyMB.git`

Then install devtools in R, if you do not already have it

`install.packages("devtools")`

Set your working directory to the directory above where you downloaded the tidyMB package. Then use install using devtools.

`setwd("/Path/to/the/directory")
devtools::install("tidyMB")`

Because most of the functions here are wrappers, you will need to have the tidyverse and vegan installed

`install.packages(c("tidyverse", "vegan"))`

### Method 2: Install directly using devtools
A bit different than the above

`install_github("bulksoil/tidyMB")`