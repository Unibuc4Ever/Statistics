# Used for guards
OperatorOverload <- T

if (!exists('CoreVarFile'))
  source('CoreVariable.R')

# Adunarea a doua variabile
setMethod(f="+",
  signature = c("CoreVariable", "CoreVariable"),
  definition = function(e1, e2) {
    new_pdf <- function(x_list) {
      ans <- c()

      for (x in x_list) {
        convolutie <- function(k) {
          return(e1@pdf(k) * e2@pdf(x - k))
        }
        
        rez_x <- Integrate(convolutie, -Inf, Inf)
        ans <- c(ans, rez_x)
      }

      return(ans)
    }

    new_var <- BuildFromPDF(new_pdf)
    return(new_var)
  }
)

# Negarea unei variabile
setMethod(f="-",
    signature = "CoreVariable",
    definition = function(e1) {
        new_pdf <- function(x) {
          return(e1@pdf(-x))
        }

        new_var <- BuildFromPDF(new_pdf)
        return(new_var)
    }
)

# Scaderea a doua variabile
setMethod(f="-",
  signature = c("CoreVariable", "CoreVariable"),
  definition = function(e1, e2) {
    return(e1 + (-e2))
  }
)

# Inmultirea a doua variabile
setMethod(f="*",
  signature = c("CoreVariable", "CoreVariable"),
  definition = function(e1, e2) {
    new_pdf <- function(x_list) {
      ans <- c()

      for (x in x_list) {
        convolutie <- function(k) {
          return(e1@pdf(k) * e2@pdf(x / k))
        }
        
        eps <- 10^-9
        rez_x <- Integrate(convolutie, -Inf, Inf)
        ans <- c(ans, rez_x)
      }

      return(ans)
    }

    new_var <- BuildFromPDF(new_pdf)
    return(new_var)
  }
)

#########################################################################################
#                                                                                       #
#                               Operatii cu constante.                                  #
#                                                                                       #
#########################################################################################

# Adunarea cu o constanta.
setMethod(f="+",
  signature = c("CoreVariable", "numeric"),
  definition = function(e1, e2) {
    new_pdf <- function(x) {
      return(e1@pdf(x - e2))
    }
    return(BuildFromPDF(new_pdf))
  }
)
setMethod(f="+",
  signature = c("numeric", "CoreVariable"),
  definition = function(e1, e2) {
    return(e2 + e1)
  }
)

# Scaderea cu o constanta.
setMethod(f="-",
  signature = c("CoreVariable", "numeric"),
  definition = function(e1, e2) {
    return(e1 + (-e2))
  }
)
setMethod(f="-",
  signature = c("numeric", "CoreVariable"),
  definition = function(e1, e2) {
    return((-e2) + e1)
  }
)

setMethod(f="*",
  signature = c("numeric", "CoreVariable"),
  definition = function(e1, e2) {
    if (abs(e1) < 1e-5)
      stop("Unable to multiply by 0!")
      
    new_pdf <- function(x) {
      return(e2@pdf(x / e1))
    }
    return(BuildFromPDF(new_pdf))
  }
)

setMethod(f="*",
  signature = c("CoreVariable", "numeric"),
  definition = function(e1, e2) {
    return(e2 * e1)
  }
)

setMethod(f="/",
  signature = c("CoreVariable", "numeric"),
  definition = function(e1, e2) {
    return(e1 * (1 / e2))
  }
)
