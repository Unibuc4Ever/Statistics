# Builds an uniform distribution from st to dr.
#' @export
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
#' @export
BuildNormalDistribution <- function(mean, stddev) {
  if (stddev <= 0)
    stop("Invalid standard deviation for normal distribution!")

  pdf <- function(x) {
    exp(-1/2 * ((x - mean) / stddev)^2) / (stddev * sqrt(2 * pi))
  }

  return(BuildFromPDF(pdf))
}

# Builds an exponential distribution given the rate parameter.
#' @export
uildExponentialDistribution <- function(lambda) {
  if (lambda <= 0)
    stop("Invalid rate parameter for exponential distribution!")

  pdf <- function(x) {
    ifelse(x >= 0,
      lambda*exp(-lambda*x),
      0
    )
  }

  return(BuildFromPDF(pdf))
}

# Utility function for computing gamma. Used for computing the pdf 
# of a chi-square distribution.
#' @export
omputeGamma <- function(n) {
  if (n == round(n))
    return (factorial(n - 1))
  if (n > 1)
    return ((n - 1) * gama(n - 1))
  if (n == 0.5)
    return (sqrt(pi))
  
  return (integrate(
      function(x, a) { x ^ (a - 1) * exp(-x) }, 
      0, 
      Inf, 
      a = n)$value)
}

# Builds a chi-square distribution given the number of degrees of freedom.
#' @export
BuildChiSquareDistribution <- function(k) {
  if (k <= 0)
    stop("Invalid number of degrees of freedom for chi-square distribution!")

  pdf <- function(x) {
    ifelse(x >= 0,
      (1 / (2^(k / 2) * ComputeGamma(k / 2))) * x^(k / 2 - 1) * exp(-x / 2),
      0
    )
  }

  return(BuildFromPDF(pdf))
}
