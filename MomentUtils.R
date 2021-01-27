# Used as guard
MomentUtilsFile <- T

# This module is able to compute .

if (!exists('ConstructorsFile'))
  source('Constructors.R')

if (!exists('FunctionOverloadFile'))
  source('FunctionOverload.R')

# Task 5

# Computes the n-th central moment of a random variable. 
# Note that the second central moment is the variance.
CentralMoment <- function(x, order) {
  mean <- mean(x)
  integrant <- function(t) {
    return((t - mean) ^ order * x@pdf(t))
  }
  # To check if we want to remove "With absolute error ..."
  return(integrate(integrant, -Inf, Inf))
  # TODO: verifica daca integrala nu exista => momentul nu exista
}

# Computes the n-th raw moment of a random variable. 
# Note that the first raw moment is the mean.
RawMoment <- function(x) {
  integrant <- function(t) {
    return(t * x@pdf(t))
  }
  # To check if we want to remove "With absolute error ..."
  return(integrate(integrant, -Inf, Inf))
  # TODO: verifica daca integrala nu exista => momentul nu exista
}
