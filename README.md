
<!-- README.md is generated from README.Rmd. Please edit that file 
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. 
-->

# assertHE

<div class="logos">

<img src="https://github.com/dark-peak-analytics/darkpeak/blob/main/man/figures/logo_concise.PNG?raw=true" width="120px" align="right">

</div>

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13969179.svg)](https://doi.org/10.5281/zenodo.13969179)
[![R-CMD-check](https://github.com/dark-peak-analytics/assertHE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dark-peak-analytics/assertHE/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This work is now published in Wellcome Open Research, please cite as:

> Smith RA, Samyshkin Y, Mohammed W et al. assertHE: an R package to
> improve quality assurance of HTA models. Wellcome Open Res 2024,
> 9:701. <https://doi.org/10.12688/wellcomeopenres.23180.1>

The goal of `assertHE` is to help modellers build and review health
economic models in R. The package provides functions which can be
included within models to check that the objects created conform to
standard rules (e.g. probabilities between 0 and 1). It also provides
functions to review the structure of the model, showing the network of
functions color coded by test coverage. Users can click on the nodes to
see function and test source code, test coverage and create an AI
generated summary of the function.

Rob outlined the package at R-HTA 2024 with a
[video](https://www.youtube.com/watch?v=wr8-w-6QGno) and
[slides](https://github.com/RobertASmith/talks/blob/master/RHTA24-assertHE%20(1).pdf)
publicly available for those interested in finding out more.

We are continuing to work to improve the package and welcome
contributions. To get involved, please see the [Contribution
guide](https://github.com/dark-peak-analytics/assertHE/blob/main/CONTRIBUTING.md).
For more context about the aims of the wider project please read [the
wiki](https://github.com/dark-peak-analytics/assertHE/wiki/assertHE:-an-R-package-to-improve-quality-assurance-of-health-economic-models).

## Installation

You can install the CRAN version of assertHE from
[CRAN](https://cran.r-project.org/web/packages/assertHE/) with:

``` r
install.packages("assertHE")

library(assertHE)
```

Alternatively the development version of assertHE can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dark-peak-analytics/assertHE")

library(assertHE)
```

## Using the package

### Reviewing model structure

The below code creates a visual representation of the model structure
for a given project. The user must provide a path to the project folder,
the location of functions (typically “R”) and the location of tests
(typically “tests/testthat”).

``` r

visualise_project(
  project_path = "path_to_project_directory",
  foo_path = "R",
  test_path = "tests/testthat",
  run_coverage = TRUE)
```

The result is a visual representation of the model functions. This gives
some indication of how to review the model since each function can be
checked in isolation and in combination. It may also reveal redundant
code.

The below is an example of using the function on the cdx2cea model. The
red nodes are the ones without tests, the green nodes are the ones with
tests. When hovering over a function we can see more information
including where it is defined (file and line number) and where the test
(if any) resides. The coverage % of the function is also provided. Tests
with coverage \<20% are in red, between 20-80% in orange, and above 80%
in green. These are arbitrary cut-points, reviewers should assess
sufficiency of testing.

<figure>
<img
src="https://github.com/dark-peak-analytics/assertHE/assets/41961614/0d330730-1e0b-40d9-b18b-b2ee14511cb6"
alt="Function network for cdx2cea" />
<figcaption aria-hidden="true">Function network for cdx2cea</figcaption>
</figure>

#### Using the LLM function summary tool

To use the LLM function summary tool follow the guide
[here](https://github.com/dark-peak-analytics/assertHE/wiki/Using-the-LLM-functionality-in-assertHE).

### Internal checks for modellers

The package has a series of functions to be used **within models** to
check that the objects created conform to standard rules
(e.g. probabilities between 0 and 1).

The code below shows a basic example which shows you how to use
`check_trans_prob_array` to ensure that the time dependent transition
probability array is balanced.

``` r
library(assertHE)

# create a transition probability array
n_t <- 1000 # number of cycles
v_hs_names <- c("H", "S", "D") # health states
n_hs <- length(v_hs_names)

# create array of transition probabilities
a_P <- array(
 data = 0,
 dim = c(n_hs, n_hs, n_t),
 dimnames = list(v_hs_names, v_hs_names, 1:n_t)
)

# fill in transition probabilities for main transitions.
a_P["H", "S",] <- 0.3
a_P["H", "D",] <- 0.01
a_P["S", "D",] <- 0.1
a_P["S", "H",] <- 0.5

# Fill in the proportion remaining in health state in each slice.
# This is the remainder after all other transitions are accounted for.
for(x in 1:n_t){
 diag(a_P[,,x]) <- 1 - rowSums(a_P[,,x])
}

# Use the function from the package.
# This check should return no error, the array is square, numeric, values 
# are between 0 and 1 and all rows sum to 1.
# Note: stop_if_not = FALSE returns warnings, stop_if_not = TRUE returns errors.
check_trans_prob_array(a_P = a_P, 
                       stop_if_not = TRUE)

# We can introduce an error to see the output
# In this case, we set the first 10 cycles of transition from H to S to 0.
# This means that the rows don't sum to 1 for the H row for 1:10 cycle.
a_P["H", "S", 1:10] <- 0

check_trans_prob_array(a_P = a_P, 
                       stop_if_not = FALSE)

# The output looks like this:

# Warning message:
# In check_array_rows_balanced(a_P, stop_if_not = stop_if_not) :
#   Not valid transition probabilities
#    Transition probabilities not valid from Health States:
# 1                                           H; at cycle 1
# 2                                           H; at cycle 2
# 3                                           H; at cycle 3
# 4                                           H; at cycle 4
# 5                                           H; at cycle 5
# 6                                           H; at cycle 6
# 7                                           H; at cycle 7
# 8                                           H; at cycle 8
# 9                                           H; at cycle 9
# 10                                         H; at cycle 10
```

## Using the package to review models

Please get in contact if you would like to use the package to help
review a model in R.

The following models have been visualized using the package, as test
cases:  

- [NICE RCC Model](https://github.com/nice-digital/NICE-model-repo)  
- [sicksickerPack](https://github.com/dark-peak-analytics/sicksickerPack)
  teaching model contained in a package.  
- [cdx2cea](https://github.com/feralaes/cdx2cea) as described in
  [Alarid-Escudero et
  al. 2022](https://doi.org/10.1016/j.jval.2021.07.019)  
- [DOACs-AF-Economic-model](https://github.com/Bogdasayen/DOACs-AF-Economic-model)
  developed by Bristol University  
- The CGD AMR Cost model - in press.  
- [Embedding Economics
  Analysis](https://github.com/DanPollardSheff/Embedding-Economic-Analysis)
  Diabetes Microsimulation model described in (in press).  
- Several internal models at Dark Peak Analytics.  
- Several internal models at Maple Health.  
- The National Institute for Health and Care Excellence (NICE) have
  built the `assertHE` visualiser into their template for model reviews.

## Sharing interactive model networks

Once the model has been generated, it is possible to share the HTML for
the interactive network. In the visualisation tab click the downward
arrow on the ‘export’ button and then click ‘save as web page’.

The visualisation for the HTML file may take a while to load for large
networks. However, all the funtionality from the HTML version (not the
shiny version with the links) should be there.

## Get in contact

To get in contact about this project or other collaborations please feel
free to email me at <rsmith@darkpeakanalytics.com>.
