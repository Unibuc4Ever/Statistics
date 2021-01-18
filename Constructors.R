# Used as guard
ConstructorsFile <- T

# This module is able to build `CoreVariable` objects
# With one of the pmf or cdf functions.
# To extract cdf from pmf it's enough to integrate (-Inf, x)
# To extract pmf from cdf it's enough to differenciate

if (!exists('CoreVarFile'))
    source('CoreVariable.R')

# Creates a CoreVariable from a pmf
BuildFromPMF <- function(pmf) {
  cdf <- function(x) {
    return(integrate(pmf, -Inf, x)$value)
    # TODO: Fix precision errors
  }
  
  variable <- CoreVariable(cdf=cdf, pmf=pmf)
  
  return(variable)
}

# Creates a CoreVariable from a cdf
BuildFromCDF <- function(cdf) {
  eps <- 1e-6
  pmf <- function(x) {
    return((cdf(x + eps / 2) - cdf(x - eps / 2)) / eps)
  }
  
  variable <- CoreVariable(cdf=cdf, pmf = pmf)

  return(variable)
}
