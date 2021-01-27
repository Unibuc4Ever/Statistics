# Used as guard
MomentUtilsFile <- T

if (!exists('ConstructorsFile'))
  source('Constructors.R')

if (!exists('FunctionOverloadFile'))
  source('FunctionOverload.R')


# Computes the n-th raw moment of a random variable. 
# Note that the first raw moment is the mean.
RawMoment <- function(x, order) {
  integrant <- function(t) {
    return(t^order * x@pdf(t))
  }
  return(Integrate(integrant, -Inf, Inf))
}


Average <- function(x) {
  return(RawMoment(x, 1))
}

Variance <- function(x) {
  avgx <- RawMoment(x, 1)
  avgx2 <- RawMoment(x, 2)
  return(avgx2 - avgx^2)
}

# Computes the n-th central moment of a random variable. 
# Note that the second central moment is the variance.
CentralMoment <- function(x, order) {
  mean <- Average(x)
  integrant <- function(t) {
    return((t - mean) ^ order * x@pdf(t))
  }
  return(Integrate(integrant, -Inf, Inf))
}
