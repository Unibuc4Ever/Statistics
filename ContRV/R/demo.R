# Authors: Moroianu Theodor
#          Puscasu Felix
#          Tudor Raluca
# Project: R Package with support for continuous random variables
# Date:    22 / 01 / 2021


# Importam pachetul.
library('contRV')

load_stuff <- function() {
  rm(list=ls())
  #TODO: Remove this junk
  # Raw definition of the CoreVariable class.
  source('CoreVariable.R')
      
  # CDF and PMF constructors of the CoreVariable.
  source('Constructors.R')

  # Overload of various functions function.
  source('FunctionOverload.R')

  # Overload of the print function.
  source('OperatorOverload.R')

  # Include main distributions.
  source('MainDistributions.R')

  # Useful functions.
  source('Utilities.R')
  # 
  source('MomentUtils.R')
}


###########################################################
#                                                         #
#                        Cerinta I                        #
#                                                         #
###########################################################

# Functie care poate fi normalizata
ComputeNormalizationConstant(
  function(x) {
    if (x > 0 && x < 1)
      return(2)
    return(0)
  }
)

# Functie care nu poate fi normalizata
ComputeNormalizationConstant(
  function(x) {
    if (x > 0 && x < 1)
      return(2)
    if (x > 2 && x < 3)
      return(-1)
    return(0)
  }
)



###########################################################
#                                                         #
#                        Cerinta II                       #
#                                                         #
###########################################################

# Functie care este densitate de probabilitate.
CheckIfFunctionIsPDF(
  function(x) {
    if (x > 0 && x < 1)
      return(1)
    return(0)
  }
)

# Functie care nu este densitate de probabilitate.
CheckIfFunctionIsPDF(
  function(x) {
    if (x > 0 && x < 1)
      return(2)
    return(0)
  }
)

CheckIfFunctionIsPDF(
  function(x) {
    if (x > 0 && x < 1)
      return(2)
    if (x > 2 && x < 3)
      return(-1)
    return(0)
  }
)



###########################################################
#                                                         #
#                        Cerinta III                      #
#                                                         #
###########################################################

pdf <- function(x) {
  if (x >= 0 && x <= 1)
    return(1)
  return(0)
}

var <- BuildFromPDF(pdf)
plot(var)

cdf <- function(x) {
  if (x < 0)
    return(0)
  if (x > 1)
    return(1)
  return(x)
}

var2 <- BuildFromCDF(cdf)
plot(var2@pdf, -4, 4)

wrong_pdf <- function(x) {
  if (x < 0 || x > 1)
    return(0)
  return(2)
}

v3 <- BuildFromPDF(wrong_pdf)



###########################################################
#                                                         #
#                        Cerinta IV                       #
#                                                         #
###########################################################

v1 <- BuildUniformDistribution(-10, 20)
plot(v1)

v2 <- BuildNormalDistribution(2, 6)
plot(v2)

v3 <- BuildExponentialDistribution(0.2)
plot(v3)

v4 <- BuildChiSquareDistribution(1)
plot(v4)

# TODO: ADD MORE DISTRIBUTIONS



###########################################################
#                                                         #
#                        Cerinta V                        #
#                                                         #
###########################################################

v1 <- BuildNormalDistribution(2, 1)

# Calculeaza media
print(Average(v1))

# Calculeaza dispersia
print(Variance(v1))

# Calculeaza momentul initial de ordin 7
print(RawMoment(v1, 7))

# Calculeaza momentul centrat de ordin 4
print(CentralMoment(v1, 4))



###########################################################
#                                                         #
#                        Cerinta VI                       #
#                                                         #
###########################################################

v1 <- BuildNormalDistribution(1, 2)

transformation <- function(x) {
  return(2 * x - 4)
}

print(ComputeMeanForFunc(v1, transformation))
print(ComputeVarForFunc(v1, transformation))



###########################################################
#                                                         #
#                        Cerinta VII                      #
#                                                         #
###########################################################

v1 <- BuildUniformDistribution(-10, 10)

is_positive <- function(x) {
  return(x > 0)
}

is_smaller_2 <- function(x) {
  return(x < 2)
}

prob <- Conditional(v1, is_smaller_2, is_positive)

prob2 <- Conditional(v1, is_smaller_2)



###########################################################
#                                                         #
#                        Cerinta VIII                     #
#                                                         #
###########################################################

source('Lib.R')
Information()
Information('Uniforma')
Information('Normala')
Information('Exponentiala')
Information('Chi squared')
Information('Pareto')
Information('nimic')


###########################################################
#                                                         #
#                        Cerinta IX                       #
#                                                         #
###########################################################

v1 <- BuildUniformDistribution(0, 1)
vals <- SamplePointsFromDistribution(v1, 1000)
hist(vals)

v2 <- BuildNormalDistribution(0, 1)
vals <- SamplePointsFromDistribution(v2, 1000)
hist(vals)


###########################################################
#                                                         #
#                        Cerinta X                        #
#                                                         #
###########################################################

common_pdf <- function(x, y) {
  if (min(x, y) < 0 || max(x, y) > 4)
    return(0)
  if (x + y > 6 || x + y < 2)
    return(0)
  return(1 / 12)
}

cov <- Covariance2d(common_pdf)
print(cov)



###########################################################
#                                                         #
#                        Cerinta XI                       #
#                                                         #
###########################################################

common_dist <- function(x, y) {
  if (x < 0 || y < 0 || x > 1 || y > 2)
    return(0)
  return(1/2)
}

v1 <- BuildFromCommonPDF(common_dist, ax=1)
plot(v1@pdf, -4, 4)
v2 <- BuildFromCommonPDF(common_dist, ax=2)
plot(v2@pdf, -4, 4)
v3 <- BuildConditionalPDF(common_dist, 2, 1)
plot(v3@pdf, -4, 4)



###########################################################
#                                                         #
#                        Cerinta XII                      #
#                                                         #
###########################################################

load_stuff()

v1 <- BuildNormalDistribution(1, 1)
v2 <- v1 / 2
plot(v2)

v2 <- BuildNormalDistribution(1, 1)

v3 <- v1 * v2

# v3 <- v1 + v2
plot(v3@pdf, -10, 10)

v4 <- BuildUniformDistribution(-1, 1)
v5 <- BuildUniformDistribution(-2, 2)

v6 <- v4 + v5
plot(v6@pdf, -4, 4)

v7 <- v4 * v5
plot(v7@pdf, -4, 4)

v8 <- BuildUniformDistribution(0, 1)

v9 <- v8 + 1
plot(v9@pdf, -4, 4)

v10 <- -v8
plot(v10@pdf, -4, 4)

v11 <- -v1 * 5 + v2 / 2
plot(v11)