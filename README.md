
# sur.impute

<!-- badges: start -->
<!-- badges: end -->

The goal of es.mv.nma is to provide a worked example of the validation of generic
NMA surrogate validation code. Rendered report available here:
https://timdisher.github.io/sur-impute/inst/reports/2023-02-12_tl-post.html 

# Getting rquired packages

Packages required for this project can be installed with `renv::install()` which
should pull from the imports in the DESCRIPTION.txt and install from a Feb 2 2023
MRAN repo/

# Navigating the package

The project is set up in package format so functions are generally in R and 
scripts/reports can be found in inst/. Before running this analysis you'll have
tup update the .Rprofile "R2WinBUGS.bugs.directory" option to point the the BUGS
location on your system points to your BUGS location.

## Generating required data

If you're trying to run this project you'll need to create the required test
data by running the scripts in data-raw.

## Generating report
The markdown for the report is in inst/reports but it depends on data and results
being in the global environment so won't run if you just try to render it. Instead
to generate the report run the make-analysis.R script. The last section of that
code renders the report after generating all the required variables.

