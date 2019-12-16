#Creación de vectores
## c => concatenar
codes = c(380, 124, 818)
country = c('italy', 'canada', 'egypt')

# Names
codes = c(italy = 380, canada = 124, egypt = 818)
codes
class(codes) # Tipo de dato
names(codes) # Obtener Valores

# Segunda manera Names
codes = c(380, 124, 818)
country = c('italy', 'canada', 'egypt')
names(codes) = country
codes


# Secuencias
seq(1,10)

seq(1, 10, 2) # Tercer argumento => incremento

1:10
class(1:10)

seq(1, 10, 0.5)
class(seq(1, 10, 0.5))


# Subsetting
codes[2]

codes[c(1, 3)]

codes[1:3]

codes['canada']

codes[c('canada', 'egypt')]


# Coercion
x = c(1, 'canada', 3) 
class(x)  
x

x = 1:5
y = as.character(x)
y
as.numeric(y)


# Not availables (NA)
x = c('1', 'b', '3')
as.numeric(x)


# Sorting
library(dslabs)
data(murders)

sort(murders$total)


# order => Da los índices
x = c(31, 4 ,15, 92, 65)
sort(x)
index = order(x)
x[index]
index


murders$state[1:6]
murders$abb[1:6]
ind = order(murders$total)
murders$abb[ind]
murders$state[ind]


# Max and which.max
max(murders$total)

i_max = which.max(murders$total) # Accede al índice mas alto
murders$state[i_max]


# min and which.min
min(murders$total)

i_min = which.min(murders$total)
murders$state[i_min]


# Recycling
x = c(1, 2, 3)
y = c(11, 20, 30, 40, 50, 60, 70)

x + y


## Vector arithmetics
# Rescaling a vector
inches = c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)
inches*2.54

inches - 69

# Tasa de asesinatos por cada 100000 habitantes
murder_rate = murders$total / murders$population * 100000
murders$abb[order(murder_rate)]
murders$state[order(murder_rate)]
