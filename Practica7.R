# 1
operacion = rbinom(24, 25, 0.25)
operacion
any(operacion>5)
sum(operacion>5)


# 2

binsim = rbinom(1000,20,0.3)
x = binsim
x
# a
var(x)

# b
qbinom(0.95, x, 0.3)

# c
qbinom(0.99, x, 0.3)

# d
qbinom(0.99999, x, 0.3)


# 3
x <- rbinom(100, 18, 0.76)
x
mean(x)
var(x)

# 5

ranbin <- function(n, size, prob) {
  # cumpous.- vector que contiene la probabilidad de obtener
  # como mucho 'size' valores
  cumpois <- pbinom(0 : (size - 1), size, prob)
  # singlenumber.- función que genera un número aleatorio uniforme
  # en un vector x, y lo compara en con el vector cumpois para sumar
  # los valores que cumplan la condicion x > cumpois, y retorna esa suma
  singlenumber <- function() {
    x <- runif(1)
    N <- sum(x > cumpois)
    N
  }
  # replicate.- función que ejecutará singlenumber() n veces
  replicate(n, singlenumber())
}


# ranbin(1000, 10, 0.4)
system.time(ranbin(1000, 10, 0.4))
# rbinom(1000, 10, 0.4)
system.time(rbinom(1000, 10, 0.4))

system.time(ranbin(10000, 10, 0.4))
system.time(rbinom(10000, 10, 0.4))

system.time(ranbin(100000, 10, 0.4))
system.time(rbinom(100000, 10, 0.4))


# 5

ranbin2 <- function(n, size, prob) {
  # singlenumber.- funcion que a travez de los parametros size, prob
  # genera un vector x que contiene valores aleatorios que siguen 
  # una distribución uniforme de tamaño size. Este valor sirve para luego
  # ejecutar una suma de todos los valores que cumplan la condición
  # x < prob, la cual sera almacenada en N, que es el valor que retorna
  # esta función.
  singlenumber <- function(size, prob) {
    x <- runif(size)
    N <- sum(x < prob)
    N
  }
  # replicate.- ejecuta n veces la funcion singlenumber()
  replicate(n, singlenumber(size, prob))
}


system.time(ranbin2(10000, 10, 0.4))
system.time(rbinom(10000, 10, 0.4))
system.time(ranbin(10000, 10, 0.4))


system.time(ranbin2(10000, 100, 0.4))
system.time(rbinom(10000, 100, 0.4))
system.time(ranbin(10000, 100, 0.4))


system.time(ranbin2(10000, 1000, 0.4))
system.time(rbinom(10000, 1000, 0.4))
system.time(ranbin(10000, 1000, 0.4))