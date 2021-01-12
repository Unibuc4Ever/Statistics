# Used for guards
OperatorOverload <- T

if (!exists('CoreVarFile'))
    source('CoreVariable.R')

# Habar n-am daca are sens sau e o aberatie
setMethod(f="+",
    signature=c("CoreVariable", "CoreVariable"),
    definition=function(e1, e2)
    {
        new_pmf <- function(x) {
            convolutie <- function(k) {
                return(e1@pmf(k) * e2@pmf(x - k))
            }
            return(integrate(convolutie, -Inf, Inf)$value)
        }

        new_var <- BuildFromPMF(new_pmf)
        return(new_var)
    }
)

# CDF and PMF constructors of the CoreVariable.
if (!exists('ConstructorsFile'))
    source('Constructors.R')

    
std <- function(x) {
  return(exp(-x*x/2)/sqrt(2*pi))
}

unif <- function(x) {
  if (x < 0)
    return(0)
  if (x > 1)
    return(1)
  return(x)
}

# Build 2 random vars.
v1 <- BuildFromPMF(std)
v2 <- BuildFromCDF(unif)

(v1+v1)@pmf(0)

# TODO: Find wth this doesn't work
v2@pmf(0.5)
(v2 + v2)@pmf(2) # Doesn't work (yet)
