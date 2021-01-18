# Used as guard
ConstructorsFile <- T

# This module is able to build `CoreVariable` objects
# With one of the pdf or cdf functions.
# To extract cdf from pdf it's enough to integrate (-Inf, x)
# To extract pdf from cdf it's enough to differenciate

if (!exists('CoreVarFile'))
  source('CoreVariable.R')

# Creates a CoreVariable from a pdf
BuildFromPDF <- function(pdf) {
  cdf <- function(x) {
    return(integrate(pdf, -Inf, x)$value)
    # TODO: Fix precision errors
  }
  
  variable <- CoreVariable(cdf=cdf, pdf=pdf)
  
  return(variable)
}

# Creates a CoreVariable from a cdf
BuildFromCDF <- function(cdf) {
  eps <- 1e-6
  pdf <- function(x) {
    return((cdf(x + eps / 2) - cdf(x - eps / 2)) / eps)
  }
  
  variable <- CoreVariable(cdf=cdf, pdf = pdf)

  return(variable)
}
