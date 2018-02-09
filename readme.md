# TidyMB
### An R package for performing microbiome analysis using a tidy framework

These are a set of tools and wrappers for performing microbial ecology analyses in R using a tidy framework. If you want to see a tidyMB analysis in action, I suggest you go over to my other GH repo [https://github.com/bulksoil/SoilDomestication](SoilDomestication).

This package is a work in progress and will be updated regularly to included new functions and features.

## Download
I think there are two ways to do it.

### Method 1: Install directly using devtools
A bit different than the above
`install.packages(devtools)`
`library(devtools)`
`install_github("bulksoil/tidyMB")`

### Method 2: Fork the repo and install it in R
If you want to contribute to this repo, or do some development by yourself (without contributing (but seriously, if you're reading this, you should consider contributing)), clone the repo like this

`git clone https://github.com/bulksoil/tidyMB.git`

Then install devtools in R, if you do not already have it

`install.packages("devtools")`
`library(devtools)`
Set your working directory to the directory above where you downloaded the tidyMB package. Then use install using devtools.

`setwd("/Path/to/the/directory")
devtools::install("tidyMB")`

Because most of the functions here are wrappers, you will need to have the tidyverse and vegan installed

`install.packages(c("tidyverse", "vegan"))`

