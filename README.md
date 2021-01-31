<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/lasy/cpass.svg?branch=master)](https://travis-ci.com/lasy/cpass)
  <!-- badges: end -->

# The cpass R package

`cpass` is an `R` package for the application of the C-PASS procedure for the diagnosis of PMDD and MRMD.

## Installation instructions

1. To use this package, you need `R` installed on your computer. To install `R`, follow these [instructions](https://cran.r-project.org). 

2. (optional) Once you have installed `R`, we recommend that you also install the programming environment `RStudio`, which makes it easier to interact with `R`. To download `RStudio`, go on their [website](https://rstudio.com/products/rstudio/download/) and download their free Desktop version.

3. Once both of these are installed, open `RStudio`. If you need a crash course or a refresher on `R`, we recommend these [tutorials](https://www.r-bloggers.com/2015/12/how-to-learn-r-2/) as well as getting familiar with the [tidyverse package suite](https://www.tidyverse.org).

4. Before installing the `cpass` package, make sure to install the `devtools` package by running,in your `R` console, the following two commands: 

`install.packages("devtools")`

`library(devtools)`

5. You can now and load the `cpass` package with: 

`devtools::install_github("lasy/cpass", dependencies = TRUE)`

`library(cpass)`

## Using the `cpass` package.

The package vignette, _i.e._ instructions for installing and using the package, can be found [here](https://lasy.github.io/cpass/articles/cpass_vignette.html). This vignette provides examples so that you can perform the same analyses on your data.

The description and validation of the C-PASS procedure can be found in [Eisenlohr-Moul, et al., 2016](https://github.com/lasy/cpass/blob/master/references/eisenlohr-moul2016.pdf).

In addition to the C-PASS procedure for the diagnosis of PMDD and MRMD, the `cpass` package also proposes the diagnosis of PME (pre-menstrual exacerbation). This function is still experimental and has not be validated clinically. Please, use with caution.
