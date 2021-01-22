# Used as guards
UtilitiesFile <- T

# Returns T if the function f is a valid pdf
CheckIfFunctionIsPDF <- function(pdf) {
  # Should do smth about limits
  minimum <- optimize(pdf, interval=c(-1e18, 1e18))$minimum
  integral <- integrate(pdf, -Inf, Inf)$value

  # Pozitive and integral 1
  if (minimum >= 0 && abs(integral - 1) <= 1e1-5)
    return(T)
  return(F)
}

# Gaseste constanta de normalizare a.i. functia sa fie un pdf.
ComputeNormalizationConstant <- function(pdf) {
  integral <- integrate(pdf, -Inf, Inf)$value

  if (abs(integral) < 1e-5)
    stop('Function has the integral equal to 0!')

  new_pdf <- function(x) {
    return(1/integral) * pdf(x)
  }

  if (!CheckIfFunctionIsPDF(new_pdf))
    stop('Unable to compute normalization constant!')
  
  return(1 / integral)
}

# Calculul mediei unei variabile aleatoare g(X).
ComputeMeanForFunc <- function(X, g) {
  integrant <- function(t) {
    return(g(t) * X@pdf(t))
  }
  # To check if we want to remove "With absolute error ..."
  return(integrate(integrant, -Inf, Inf))
}

# Calculul dispersiei unei variabile aleatoare g(X).
# Var(X) = E(X^2) - E^2[X]
# X -> g(X)
# Var(g(X)) = E(g^2(X)) - E^2[g(X)]
ComputeVarForFunc <- function(X, g) {
  avggx  <- ComputeMeanForFunc(X, g)
  avgg2x <- ComputeMeanForFunc(X, g^2)
  return(avgg2x - avggx^2)
}
