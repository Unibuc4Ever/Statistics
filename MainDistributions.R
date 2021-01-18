# Used as guard
MainDistributionsFile <- T

# This module is able to build standard distributions.

if (!exists('ConstructorsFile'))
  source('Constructors.R')

# Builds an uniform distribution from st to dr.
BuildUniformDistribution <- function(st, dr) {
  if (st >= dr)
    stop("Invalid interval for uniform distribution!")

  pdf <- function(x) {
    ifelse(x < st,
      0,
      ifelse(x > dr,
        0,
        1 / (dr - st)
      )
    )
  }

  return(BuildFromPDF(pdf))
}

# Builds a normal distribution given the mean and standard deviation.
BuildNormalDistribution <- function(mean, stddev) {
  if (stddev <= 0)
    stop("Invalid standard deviation for normal distribution!")

  pdf <- function(x) {
    exp(-1/2 * (x - mean / stddev)^2) / (stddev * sqrt(2 * pi))
  }

  return(BuildFromPDF(pdf))
}