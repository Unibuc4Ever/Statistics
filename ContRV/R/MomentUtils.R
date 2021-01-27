# Computes the n-th raw moment of a random variable. 
# Note that the first raw moment is the mean.
#' @export
RawMoment <- function(x, order) {
  integrant <- function(t) {
    return(t^order * x@pdf(t))
  }
  return(Integrate(integrant, -Inf, Inf))
}


#' @export
Average <- function(x) {
  return(RawMoment(x, 1))
}

#' @export
Variance <- function(x) {
  avgx <- RawMoment(x, 1)
  avgx2 <- RawMoment(x, 2)
  return(avgx2 - avgx^2)
}

# Computes the n-th central moment of a random variable. 
# Note that the second central moment is the variance.
#' @export
CentralMoment <- function(x, order) {
  mean <- Average(x)
  integrant <- function(t) {
    return((t - mean) ^ order * x@pdf(t))
  }
  return(Integrate(integrant, -Inf, Inf))
}

# Average for two dimensional variable
#' @export
AverageProduct2d <- function(common_pdf) {
  sum_for_x <- function(x) {
    sum_for_y <- function(y) {
      return(common_pdf(x, y) * x * y)
    }
    return(Integrate(MakeVectorized(sum_for_y), -Inf, Inf))
  }
  return(Integrate(MakeVectorized(sum_for_x), -Inf, Inf))
}

#' @export
Covariance2d <- function(common_pdf) {
  average_product <- AverageProduct2d(common_pdf)
  
  var1 <- BuildFromCommonPDF(common_pdf, 1)
  var2 <- BuildFromCommonPDF(common_pdf, 2)

  avg1 <- Average(var1)
  avg2 <- Average(var2)

  ans <- average_product - avg1 * avg2

  return(ans)
}

#' @export
Corelation2d <- function(common_pdf) {
  covariance <- Covariance2d(common_pdf)

  cnt1 <- BuildFromCommonPDF(common_pdf, 1)
  cnt2 <- BuildFromCommonPDF(common_pdf, 2)

  var1 <- Variance(cnt1)
  var2 <- Variance(cnt2)

  ans <- covariance / sqrt(var1 * var2)

  return(ans)
}
