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

# Importam pachetul.
source('Lib.R')

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

# Importam pachetul.
source('Lib.R')

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

# Importam pachetul.
source('Lib.R')

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
plot(var2@pdf, -4, 4) # TODO

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

# Importam pachetul.
source('Lib.R')

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

# Importam pachetul.
source('Lib.R')

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

# Importam pachetul.
source('Lib.R')

v1 <- BuildUniformDistribution(0, 1)
vals <- SamplePointsFromDistribution(v1, 1000)
hist(vals)

###########################################################
#                                                         #
#                        Cerinta X                        #
#                                                         #
###########################################################

# Importam pachetul.
source('Lib.R')

common_pdf <- function(x, y) {

}

###########################################################
#                                                         #
#                        Cerinta XI                       #
#                                                         #
###########################################################

# Importam pachetul.
source('Lib.R')

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

source('Lib.R')

v1 <- BuildNormalDistribution(1, 1)
v2 <- BuildNormalDistribution(1, 1)

v3 <- v1 + v2
plot(v3)

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
