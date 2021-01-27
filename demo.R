# Authors: Raluca Tudor
#          Moroianu Theodor
#          Puscasu Felix
# Project: R Package with support for contignous random variables
# Date:    22 / 01 / 2021


# Importam pachetul.
source('Lib.R')


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
) # WTF =============================



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

cdf <- function(x) {
  if (x < 0)
    return(0)
  if (x > 1)
    return(1)
  return(x)
}

var2 <- BuildFromCDF(cdf)


###########################################################
#                                                         #
#                        Cerinta IV                       #
#                                                         #
###########################################################



###########################################################
#                                                         #
#                        Cerinta V                        #
#                                                         #
###########################################################


###########################################################
#                                                         #
#                        Cerinta VI                       #
#                                                         #
###########################################################


###########################################################
#                                                         #
#                        Cerinta VII                      #
#                                                         #
###########################################################


###########################################################
#                                                         #
#                        Cerinta VIII                     #
#                                                         #
###########################################################


###########################################################
#                                                         #
#                        Cerinta IX                       #
#                                                         #
###########################################################


###########################################################
#                                                         #
#                        Cerinta X                        #
#                                                         #
###########################################################


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
plot(v1)
v2 <- BuildFromCommonPDF(common_dist, ax=2)
plot(v2)
v3 <- BuildConditionalPDF(common_dist, 2, 1) # WTF????
plot(v3)

###########################################################
#                                                         #
#                        Cerinta XII                      #
#                                                         #
###########################################################

v1 <- BuildUniformDistribution(1, 2)
v2 <- BuildNormalDistribution(2, 3)
v3 <- v1 + v2
v4 <- v1 - v2
v5 <- -v1
v6 <- v1 * v2
# TODO: Add numeric types
