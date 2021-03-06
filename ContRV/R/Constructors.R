# This module is able to build `CoreVariable` objects
# With one of the PDF or CDF functions.
# To extract CDF from PDF it's enough to integrate (-Inf, x).
# To extract PDF from CDF it's enough to differentiate.

# Creates a CoreVariable from a PDF.
#' @export
BuildFromPDF <- function(pdf) {
  vectorized_pdf <- MakeVectorized(pdf)

  cdf <- function(x) {
    ans <- c()
    for (elem in x)
      ans <- c(ans, Integrate(vectorized_pdf, -Inf, elem))
    return(ans)
  }
  
  variable <- CoreVariable(cdf=MakeVectorized(cdf), pdf=vectorized_pdf)
  
  return(variable)
}

# Creates a CoreVariable from a CDF.
#' @export
BuildFromCDF <- function(cdf) {
  eps <- 1e-6
  pdf <- function(x) {
    return((cdf(x + eps / 2) - cdf(x - eps / 2)) / eps)
  }
  
  variable <- CoreVariable(cdf=MakeVectorized(cdf), pdf=MakeVectorized(pdf))
  
  return(variable)
}

# Returns a CoreVariable.
# If ax=1 then returns X, else return Y.
#' @export
BuildFromCommonPDF <- function(commonpdf, ax)
{
  if (ax != 1 && ax != 2)
    stop("AX must be 1 or 2!")
  
  new_pdf <- function(point) {
    val_in_point <- function(w) {
      if (ax == 1)
        return(commonpdf(point, w))
      else
        return(commonpdf(w, point))
    }
    return(Integrate(MakeVectorized(val_in_point), -Inf, Inf))
  }
  return(BuildFromPDF(new_pdf))
}

# Returns a CoreVariable.
# Ax identifies the axis with set value "value"
#' @export
BuildConditionalPDF <- function(commonpdf, ax, val)
{
  if (ax != 1 && ax != 2)
    stop("AX must be 1 or 2!")

  new_pdf <- function(point) {
    if (ax == 1)
      return(commonpdf(val, point))
    else
      return(commonpdf(point, val)) 
  }

  integral <- Integrate(MakeVectorized(new_pdf), -Inf, Inf)

  if (integral < 0 || abs(integral) < 1e-5)
    stop("Invalid slice of the distribution at given point!")

  real_new_pdf <- function(point) {
    return(new_pdf(point) / integral)
  }

  return(BuildFromPDF(real_new_pdf))
}
