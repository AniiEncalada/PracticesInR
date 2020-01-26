n = 100000
u = 5.5
o = 1
f = numeric(n)

for (i in 1:n) {
  x <- (o * sqrt(12/n) * (sum(runif(n)) - (n/2))) + u
  f[i] = x
}
f
