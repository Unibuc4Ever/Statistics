# Class storing the cdf and pmf functions.
CoreVariable <- setClass (
  # Name of the class.
  "CoreVariable",
  
  # Data stored in the class.
  # Using the type `ANY` because I don't know
  # the real type of the functions.
  slots = c (
    cdf = "ANY",
    pmf = "ANY"
  ),
  
  # If we want to set a prototype.
  #     prototype = list (
  #         cdf = ...,
  #         pmf = ...
  #     )
  
  # Checks if the object is valid.
  #validity = function(object) {
  #  if (abs(integrate(object@pmf, -Inf, Inf) - 1) > 1e-3)
  #    return("CSF doesn't have 1 as limit at +Inf!")
  #  return(T)
  #}
)

# Creates a CoreVariable from a pmf
BuildFromPMF <- function(pmf) {
  cdf <- function(x) {
    return(integrate(pmf, -Inf, x))
    # Should get rid of the "With precision of ..."
  }
  
  variable <- CoreVariable(cdf=cdf, pmf=pmf)
  
  return(variable)
}

BuildFromCDF <- function(cdf) {
  eps <- 1e-6
  pmf <- function(x) {
    return((cdf(x + eps) - cdf(x)) / eps)
  }
  
  variable <- CoreVariable(cdf=cdf, pmf = pmf)
}

std <- function(x) {
  return(exp(-x*x/2)/sqrt(2*pi))
}

unif <- function(x) {
  if (x < 0)
    return(0)
  if (x > 1)
    return(1)
  return(x)
}

# Build 2 random vars.
v1 <- BuildFromPMF(std)

v2 <- BuildFromCDF(unif)

