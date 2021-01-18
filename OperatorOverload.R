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
        
        rez_x <- integrate(convolutie, -Inf, Inf)$value
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

# Adunarea a doua variabile
setMethod(f="*",
  signature = c("CoreVariable", "CoreVariable"),
  definition = function(e1, e2) {
    new_pdf <- function(x_list) {
      ans <- c()

      for (x in x_list) {
        convolutie <- function(k) {
          return(e1@pdf(k) * e2@pdf(x / k))
        }
        
        eps <- 10^-5
        rez_x <- integrate(convolutie, -10^5, -eps)$value +
                 integrate(convolutie, eps, 10^5)$value
        ans <- c(ans, rez_x)
      }

      return(ans)
    }

    new_var <- BuildFromPDF(new_pdf)
    return(new_var)
  }
)
