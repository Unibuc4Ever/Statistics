source('Lib.R')


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


print(v1) # Works, uses custom print function
v1 # Doesnt work, i don't know which function is used
