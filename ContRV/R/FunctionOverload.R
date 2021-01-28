
setMethod(
  f = "print",
  signature = c("CoreVariable"),
  definition = function(x) {
    print("Value of the CoreVariable:")
    print("PMF: ", x@pdf)
    print("CDF: ", x@cdf)
  }
)

setMethod(
  f = "plot",
  signature = c("CoreVariable"),
  definition = function(x) {
    plot(x@pdf, -30, 30)
  }
)
