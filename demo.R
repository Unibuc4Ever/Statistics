source('Lib.R')
    
std <- function(x) {
  return(exp(-x*x/2)/sqrt(2*pi))
}

unif <- function(x) {
  ifelse(x < 0,
    0,
    ifelse(x > 1,
      1,
      x
    )
  )
}

# Build 2 random vars.
v1 <- BuildFromPDF(std)
v2 <- BuildFromCDF(unif)
v3 <- v1 + v2
v4 <- -v2
v5 <- v1 - v2
v6 <- v3 * v5

v3@pdf(c(10, 11))
plot(v6)

for (i in c(1, 2, 4)) {
  print(i)
}