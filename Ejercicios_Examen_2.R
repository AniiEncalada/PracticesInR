n = 100000
m = 5.5
d = 1
result = numeric(n)
for (i in 1:n) {
  xi <- (d * sqrt(12/n) * (sum(runif(n)) - (n/2))) + m
  result[i] = xi
}
result