
# es.mv.nma

<!-- badges: start -->
<!-- badges: end -->

The goal of es.mv.nma is to provide a worked example of the validation of generic
NMA surrogate validation code.

# Navigating the package

The project is set up in package format so functions are generally in R and 
scripts/reports can be found in inst/. If you're running this package on a system
where WinBUGS is somewhere else than the programs directory (like me) then you'll
need to create a .Rprofile and create an "R2WinBUGS.bugs.directory" option that
points to your BUGS location.

## Generating required data

If you're trying to run this project you'll need to create the required test
data by running the scripts in data-raw.

## Generating report
The markdown for the report is in inst/reports but it depends on data and results
being in the global environment so won't run if you just try to render it. Instead
to generate the report run the make-analysis.R script. The last section of that
code renders the report after generating all the required variables.

