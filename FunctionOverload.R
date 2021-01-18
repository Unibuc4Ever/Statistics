# Used as guards
FunctionOverloadFile <- T

if (!exists('CoreVarFile'))
  source('CoreVariable.R')

setMethod(
  f = "print",
  signature = c("CoreVariable"),
  definition = function(x) {
    print("Wassaaaap?")
    print("Value of the CoreVar:")
    print("PMF: ", x@pdf)
    print("CDF: ", x@cdf)
  }
)

setMethod(
  f = "plot",
  signature = c("CoreVariable"),
  definition = function(x) {
    plot(x@pdf, -10, 10)
  }
)

setMethod(
  f = "mean",
  signature = c("CoreVariable"),
  definition = function(x) {
    integrant <- function(t) {
      return(t * x@pdf(t))
    }
    # To check if we want to remove "With absolute error ..."
    return(integrate(integrant, -Inf, Inf))
  }
)

setMethod(
  f = "var",
  signature = c("CoreVariable"),
  definition = function(x) {
    avgx <- mean(x)
    avgx2 <- mean(x * x)
    return(avgx2 - avgx^2)
  }
)