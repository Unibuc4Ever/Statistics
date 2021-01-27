# Used as guards
UtilitiesFile <- T

# To be impoved
Integrate <- function(f, st, dr) {
  st <- max(st, -30)
  dr <- min(dr, 30)
  if (st >= dr)
    return(0)

  ans <- 0

  for (i in seq(st, dr, 10))
    ans <- ans + integrate(f, i, min(dr, i + 10))$value
  
  return(ans)
}

# To be left alone
Minimum <- function(f, st, dr) {
  st <- max(st, -30)
  dr <- min(dr, 30)
  if (st >= dr)
    return(0)

  ans <- 1e9

  for (i in seq(st, dr, 1e-2))
    ans <- min(ans, f(i))
  
  return(ans)
}

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
  minimum <- Minimum(vectorized_pdf, -1e9, 1e9)
  integral <- Integrate(vectorized_pdf, -Inf, Inf)

  # Pozitive and integral 1
  if (minimum >= 0 && abs(integral - 1) <= 1e-5)
    return(T)
  return(F)
}

# Gaseste constanta de normalizare a.i. functia sa fie un pdf.
ComputeNormalizationConstant <- function(pdf) {
  vectorized_pdf <- MakeVectorized(pdf)

  integral <- Integrate(vectorized_pdf, -Inf, Inf)

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
    # Cautam binar raspunsul
    p <- -100
    pas <- 100
    for (idk in 1:20) {
      act <- p + pas
      if (dist@cdf(act) <= x)
        p <- p + pas
      pas <- pas / 2
    }
    ans <- c(ans, p)
  }
  return(ans)
}

# Calculul mediei unei variabile aleatoare g(X).
ComputeMeanForFunc <- function(X, g) {
  integrant <- function(t) {
    return(g(t) * X@pdf(t))
  }
  # To check if we want to remove "With absolute error ..."
  return(integrate(integrant, -Inf, Inf)$value)
}

# Calculul dispersiei unei variabile aleatoare g(X).
# Var(X) = E(X^2) - E^2[X]
# X -> g(X)
# Var(g(X)) = E(g^2(X)) - E^2[g(X)]
ComputeVarForFunc <- function(X, g) {
  avggx  <- ComputeMeanForFunc(X, g)
  avgg2x <- ComputeMeanForFunc(X, function(x) { return(g(x)*g(x)) })
  return(avgg2x - avggx^2)
}
