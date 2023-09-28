---
output:
  html_document: default
  pdf_document: default
---
<!-- badges: start -->

[![R-CMD-check](https://github.com/RConsortium/submissions-pilot2/workflows/R-CMD-check/badge.svg)](https://rconsortium.github.io/submissions-pilot2/)
<!-- badges: end -->

## Overview

The objective of the R Consortium R submission Pilot 2 Project is to test the concept that a Shiny application created with the R-language can be successfully bundled into a submission package and transferred successfully to FDA reviewers. The application was built using the source data sets and analyses contained in the R submission Pilot 1 Project, with materials available on the [RConsortium/submissions-pilot1](https://github.com/RConsortium/submissions-pilot1) repository, All submission materials and communications from this pilot are publicly available, with the aim of providing a working example for future R language based FDA submissions. This is a FDA-industry collaboration through the non-profit organization R consortium.

While the intent of the project is to enable execution of the Shiny application in a reviewer's local R environment, a deployed version of the application is available in open access through the Shinyapps.io service at [rconsortium.shinyapps.io/submissions-pilot2](https://rconsortium.shinyapps.io/submissions-pilot2/).

The [working group website](https://rconsortium.github.io/submissions-wg/).

The [RConsortium/submissions-pilot2](https://github.com/RConsortium/submissions-pilot2) demonstrates an approach to organize a Shiny application as an R package.

The [RConsortium/submissions-pilot2-to-fda](https://github.com/RConsortium/submissions-pilot2-to-fda)
repo demonstrates the eCTD submission package based on the [RConsortium/submissions-pilot2](https://github.com/RConsortium/submissions-pilot2) repo.  

## Meeting Minutes

[Repo wiki](https://github.com/RConsortium/submissions-pilot2/wiki)

## FDA response

[2023/09/28] Pilot 2 (shiny) FDA response letter received! [link](https://github.com/RConsortium/submissions-wg/blob/0f1dc5c30985d413f75d196c2b6caa96231b26ee/_Documents/Summary_R_Pilot2_Submission%2027SEP2023.pdf)

## Installing `teal` :

Follow the link [here](https://github.com/insightsengineering/depository#readme)

## Running application

* Clone this repository to your local machine
* Open the project within RStudio, and run `renv::restore()` if prompted to restore the `{renv}` package library
* Open the `app.R` script and run the application within RStudio by clicking the Run App button
