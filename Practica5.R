# 1.	Generate 20 pseudorandom numbers using 
# xn =172 xn−1 (mod 30 307), with initial 
# seed x0 =17 218. 
X0 = 17218
x= numeric(20)
x[1] = (172*X0) %% 30307
for(i in 2:20){
  x[i] = (172*x[i-1])%% 30307
}
x
# 2.	Generate 20 pseudorandom numbers using 
# the multiplicative congruential generator 
# with b =	171 and m =32 767 with an initial 
# seed of 2019.  
X0 = 2019
x=numeric(20)
x[1] = (171*X0)%%32767
for(i in 2:20){
  x[i] = (171*x[i-1])%%32767
}
x
# 3.	Use the runif()function (with set.seed(32078))
#   to generate10 pseudorandom numbers from (a) 
# the uniform (0, 1) distribution (b) the uniform 
# (3, 7) distribution (c) the uniform (−2, 2) 
# distribution.
#a. 
set.seed(32078)
runif(10, 0, 1)
#b. 
set.seed(32078)
runif(10, -2, 2)
#c. 
set.seed(32078)
runif(10, 3, 7)
# 4.	Generate 1000 uniform pseudorandom variates
# using the runif() function, assigning them 
# to a vector called U. Use set.seed(19908).
# (a)  Compute the average, variance, and standard 
# deviation of the numbers in U.
set.seed(19908)
U = runif(1000)
mean(U)
var(U)
sd(U)
# (b) Compare your results with the true mean, 
# variance, and standard deviation.
median(U)
# (c)  Compute the proportion of the values of 
# U that are less than 0.6, and compare with 
# the probability that a uniform random variable 
# U is less than 0.6.
sum(U < 0.6)
p = rep(c('0.6', "0.61"), times = c(610, 390))
periodo = 10000
events = replicate(periodo, sample(p, 1))
tab = table(events)
tab
prop.table(tab)
# (d)  Estimate the expected value of 1/(U +	1). 
e = 1/(U+1)
e
# (e)  Construct a histogram of the values of U, 
# and of 1/(U+1).  
hist(U)
hist(e)
# 5.	Simulate 10 000 independent observations on 
# a uniformly distributed random variable on the 
# interval [3.7,5.8]. 

# (a) Estimate the mean, variance, and standard 
# deviation of such a uni- form random variable 
# and compare your estimates with the true values.
a = runif(10000, 3.7, 5.8)
mean(a)
var(a)
sd(a)
b = runif(10000)
mean(b)
var(b)
sd(b)
# (b)  Estimate the probability that such a random 
# variable is greater than 4.0. Compare with the 
# true value.
events = replicate(10000, sample((a>4), 1))
tab = table(events)
tab
prop.table(tab)
# Simulate 10 000 values of a uniform (0, 1) random 
# variable, U1, using runif(), and simulate another 
# set of 10 000 values of a uniform (0, 1)	random 
# variable U2. Assign these vectors to U1 and U2, 
# respectively. Since the values in U1 and U2 are 
# approximately independent, we can view U1 and U2 
# as independent uniform (0, 1) random variables.
# a. Estimate E . Compare with the true value, and 
# compare with an estimate of E[U1]+ E[U2].
u1 = runif(10000, 0, 1)
u2 = runif(10000, 0, 1)
E[u1 + u2]
E[u1] + E[u2]
# b. Estimate Var ) and Var(U1) + Var (U2). Are 
# they equal? Should the true values be equal? 
var(u1+u2)
var(u1) + var(u2)
# c. Estimate P 
events = replicate(10000, sample((u1+u2 <= 1.5), 1))
tab = table(events)
tab
prop.table(tab)
# d. Estimate P(√U1+ √U2)≤1.5.
events = replicate(10000, sample((sqrt(u1)+sqrt(u2) <= 1.5), 1))
tab = table(events)
tab
prop.table(tab)
# 7.Suppose U1, U2 and U3 are independent uniform random variables 
# on the interval (0, 1). Use simulation to estimate the following 
# quantities: 
# a.	E[U1 + U2 + U3]. 
U1 = runif(1000, 0, 1)
U2 = runif(1000, 0, 1)
U3  = runif(1000, 0, 1)
E[U1 + U2 + U3]
# b.	Var(U1 + U2 + U3) and Var(U1) + Var (U2) + Var(U3). 
var(U1 + U2 + U3) 
var(U1) + var (U2) + var(U3)
# c.	E[raiz(u1+u2+u3)]
E[sqrt(U1+U2+U3)]
# d. P[Raiz(u1) + Raiz(u2)+Raiz(u3) >=0.8]  
p[(sqrt(U1) + sqrt(U2) + sqrt(U3)) >=0.8]
events = replicate(10000, sample(((sqrt(U1) + sqrt(U2) + sqrt(U3)) >=0.8), 1))
tab = table(events)
tab
prop.table(tab)
  