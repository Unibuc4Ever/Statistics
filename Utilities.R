# Used as guards
UtilitiesFile <- T

# Makes functions from R -> R to be from R* to R*
MakeVectorized <- function(f) {
  return(function(v) {
    ans <- c()
    for (i in v)
      ans <- c(ans, f(i))
    return(ans)
  })
}

# Returns T if the function f is a valid pdf
CheckIfFunctionIsPDF <- function(pdf) {
  vectorized_pdf <- MakeVectorized(pdf)

  # Should do smth about limits
  minimum <- optimize(vectorized_pdf, interval=c(-1e18, 1e18))$minimum
  integral <- integrate(vectorized_pdf, -Inf, Inf)$value

  # Pozitive and integral 1
  if (minimum >= 0 && abs(integral - 1) <= 1e-5)
    return(T)
  return(F)
}

# Gaseste constanta de normalizare a.i. functia sa fie un pdf.
ComputeNormalizationConstant <- function(pdf) {
  vectorized_pdf <- MakeVectorized(pdf)

  integral <- integrate(vectorized_pdf, -Inf, Inf)$value

  if (abs(integral) < 1e-5)
    stop('Function has the integral equal to 0!')

  new_pdf <- function(x) {
    return(1/integral * pdf(x))
  }

  if (!CheckIfFunctionIsPDF(new_pdf))
    stop('Unable to compute normalization constant!')
  
  return(1 / integral)
}

# Genereaza N valori dintr-o repartie data de un CoreVariable
SamplePointsFromDistribution = function(dist, size) {
  ans = c()
  val = runif(size)

  for (x in val) {
    print(x)
    # Cautam binar raspunsul
    p <- -1e9
    pas <- 1e9
    for (idk in 1:100) {
      act <- p + pas
      if (dist@cdf(act) <= x)
        p <- p + pas
      pas <- pas / 2
    }
    ans <- c(ans, p)
  }
  return(ans)
}
