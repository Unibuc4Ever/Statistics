source('Lib.R')

# Build 2 random vars.
v1 <- BuildNormalDistribution(0, 1)
v2 <- BuildUniformDistribution(0, 1)
v3 <- v1 + v2
v4 <- -v2
v5 <- v1 - v2
v6 <- v1 * v2

plot(v2@pdf)
plot(v1)

v1@pdf(0)
v2@pdf(0.5)
v6@pdf(0.5)
v3@pdf(c(10, 11))

v6@pdf(0)
plot(v6)

for (i in c(1, 2, 4)) {
  print(i)
}