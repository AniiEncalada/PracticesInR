# 1.
x0 = 17218
x = numeric(20)
x[1] = (172 * x0) %% 30307
for (i in 2:20) {
  x[i] = (x[i - 1] * 172) %% 30307
}
x

# 2.
x0 = 2019
x = numeric(20)
x[1] = (171 * x0) %% 32767
for (i in 2:20) {
  x[i] = (x[i - 1] * 171) %% 32767
}
x

# 3.
# a
set.seed(32078)
runif(10, 0, 1)

# b
set.seed(32078)
runif(10, 3, 7)

# c
set.seed(32078)
runif(10, -2, 2)

# 4.
# a
set.seed(32078)
u = runif(10, -2, 2)
mean(u)
var(u)
sd(u)

# b
median(u)

# c
sum(u < 0.6)
probability = rep(c('0.6', '0.61'), times = c(610, 390))
b = 10000
events = replicate(b, sample(probability, 1))
table_1 = table(events)
table_1
prop.table(table_1)

# d
result = 1 / (u + 1)
result

# e
hist(result)

# 5.
# a
r = runif(10000, 3.7, 5.8)
mean(r)
var(r)
sd(r)
(3.7 + 5.8) / 2
(5.8 - 3.7) ^ 2 /12
sqrt((5.8 - 3.7) ^ 2 / 12)

# b
mean(r > 4)
(5.8 - 4) / (5.8 - 3.7)

# 6
# a
u1 = runif(10000, 0, 1)
u2 = runif(10000, 0, 1)
E = u1 + u2
E

# b
u1 = runif(10000, 0, 1)
u2 = runif(10000, 0, 1)
var = u1 + u2
var
(u1) + (u2)

# c
u1 = runif(10000, 0, 1)
u2 = runif(10000, 0, 1)
p = (u1 + u2 <= 1.5)
p

# d
u1 = runif(10000, 0, 1)
u2 = runif(10000, 0, 1)
p = (sqrt(u1) + sqrt(u2) <= 1.5)
p

# 7.
# a
u1 = runif(10000)
u2 = runif(10000)
u3 = runif(10000)
u = u1 + u2 + u3
mean(u)

# b
var(u)
var(u1) + var(u2) + var(u3)

# c
mean(sqrt(u))

# d
v <- sqrt(u1) + sqrt(u2) + sqrt(u3)
mean(v >= 0.8)