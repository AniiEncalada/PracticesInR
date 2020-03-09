### METODOS DE INTEGRACION ###
# Integracion de Riemann
# P[-1 < z <= 1] = phi(1) - phi(-1) = 68.2%
# j = P[a < z <= b] = phi(a) - phi(b)
# integral a, b de la funcion o(z)dz, donde:
# a < b y la o(z) = 1 / sqrt(2 * pi) * e^(-(z ^ 2) / 2)

# P[-1 < z <= 1]
pnorm(1) - pnorm(-1)


# Numero de rectangulos
m = 5000
# limites
a = -1
b = 
# Ancho de cada rectangulo
w = (b - a) / m
# m puntos medios
g = seq(a + w / 2, b - w / 2, length = m)
# Vector de m alturas
const = 1 / sqrt(2 * pi)
h = const * exp(-g ^ 2 / 2)
h
# Area total (probabilidad aproximada)
sum(w * h)


## Ejercicio 1
# Para a = 0, y b = 1, con los valores de m = 10, 20, 50 y 500.
# Cual de estos valores nos da el resultado con 5 decimales
# de exactitud

# P[0 < z <= 1]
# Usando la funcion pnorm()
pnorm(0) - pnorm(-1)

a = 0
b = 1

# Cuando m = 10
m = 10

# Cuando m = 20
m = 20

# Cuando m = 50
m = 50

# Cuando m = 500
m = 500

w = (b - a) / m
g = seq(a + w / 2, b - w / 2, length = m)
const = 1 / sqrt(2 * pi)
h = const * exp(-g ^ 2 / 2)
sum(w * h)


## Ejercicio 2
# Para m = 5000, modifique el programa para encontrar
# P[1.2 < z <= 2.5]. Compare su respuesta con el valor exacto
# usando pnorm

# P[1.2 < z <= 2.5]
# Usando la funcion pnorm()
pnorm(2.5) - pnorm(1.2)

a = 1.2
b = 2.5

# Cuando m = 5000
m = 5000

w = (b - a) / m
g = seq(a + w / 2, b - w / 2, length = m)
const = 1 / sqrt(2 * pi)
h = const * exp(-g ^ 2 / 2)
sum(w * h)


### Integracion Monte Carlo
# Seleccionar u puntos uniformemente distribuidos en el intervalo (0, 1)
set.seed(12)
# Numero de puntos aleatorios
m = 500000
# Rango
a = 0; b = 1
w = (b - a) / m
# Vector de m puntos aleatorios
u = a + (b - a) * runif(m)
# Alturas de cada uno de los puntos
h = dnorm(u)
sum(w * h)


### Metodo de Aceptacion y Rechazo
set.seed(12)
m = 500000
u = runif(m, 0, 1)
h = runif(m, 0, 0.4)
aceptacion = mean(h < dnorm(u))
0.4 * aceptacion


### Metodo aleatorio de muestreo
set.seed(2020)
m = 500000
a = -1; b = 1
z = rnorm(m)
mean(z > a & z <= b)



### Metodo del trapecio
trapezoid = function(ftn, a, b, n = 100) {
  # Integral numerica de la funcion ftn desde a hasta b
  # usando la regla del trapecio con n subdivisiones
  # ftn es una funcion de una variable x
  # asumir que a < b y n es un entero positivo
  # h el ancho de cada intervalo
  h = (b - a) / n
  # x.vec es un vector de a hasta b que tambien marca
  # comienzo y el final de cada subintervalo
  x.vec = seq(a, b, by = h)
  # Dada ftn es f(x), f.vec valores de y para cada
  # x-sub-i
  f.vec = sapply(x.vec, ftn)
  # Implementar la regla del trapecio sumando todas
  # las areas
  T = h * (f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n + 1] / 2)
  return(T)
}

## Ejercicio 3
# Estimar la integral entre 0 y 1 de 4x^3 dx = 1
ftn6 = function(x) return(4 * x ^ 3)
trapezoid(ftn6, 0, 1, n = 10000)


### CASO DE ESTUDIO: EPIDEMIAS ###
## Modelo SIR (Susceptibles, Infectados, Removerlos)
# Susceptible: Todavia no tiene la enfermedad
# Infected: Han tenido la enfermedad y se han recuperado
# y ahora son inmunes (o han muerto)
# En el tiempo t, se tiene un numero de S, I y R:
# S(t) = numero de susceptibles en el tiempo t;
# I(t) = numero de infectados en el tiempo t;
# R(t) = numero de recuperados en el tiempo t;

# En cada tiempo t, cada I (Infectado) tiene:
# 1. probabilidad a de infectar a un susceptible; y
# 2. probabilidad b de ser R

# En el tiempo  0(inicio):
# S(0) = N; tamaño de la poblacion
# I(0) = 1;
# R(0) = 0
# Total de la poblacion N + 1:
# S(t)  + I(t) + R(t) = N + 1 + 0

## Reglas
# Para cada tiempo t, la probabilidad de que un susceptible
# permanezca sin la enfermedad es: (1 - a) ^ I(t)

# Si es susceptible no se infecta, es decir:
# S(t + 1) ~ binom(S(t), (1 - a) ^ I(t))

# Ya que cada infectado tiene la probabilidad b de ser
# removido es decir: R(t + 1) ~ R(t) + binom(I(t), b)

# I(t + 1) = N + 1 - R(t + 1) - S(t + 1)

SIRsim = function(a, b, N, T){
  # T = nro de periodos de tiempo
  # a = es la tasa de infeccion
  # b = es la tasa de ser removidos
  # N = la cantidad inicial de susceptibles, 1(Persona infectada)
  # Tamaño de simulación T
  # T = 100
  S = rep(0, T + 1)
  I = rep(0, T + 1)
  R = rep(0, T + 1)
  S[1] = N
  I[1] = 1
  R[1] = 0
  for (i in 1:T) {
    # de la regla 2
    S[i + 1] = rbinom(1 , S[i], (1 - a) ^ I[i])
    # de la regla 3
    R[i + 1] = R[i] + rbinom(1, I[i], b)
    # de la regla 4
    I[i + 1] = N + 1 - R[i + 1] - S[i + 1]
  }
  return(matrix(c(S, I, R), ncol = 3))
}

N = 1000 # Poblacion
T = 100 # Periodo de tiempo
a = 0.0005 # Probabilidad de infeccion
b = 0.1 # Probabilidad de removido

Z = SIRsim(a, b, N, T)
colnames(Z) = c('S', 'I', 'R')
Z

library(tidyverse)
Z = data.frame(Z)
# ggplot(Z, aes(x = 1 : 101, y = S)) + geom_line()
# ggplot(Z, aes(x = 1 : 101)) +
#   geom_line(aes(y = S, colour = 'blue')) +
#   geom_line(aes(y = I, colour = 'red')) +
#   geom_line(aes(y = R, colour = 'black')) +
#   scale_color_manual(labels = c('S', 'I', 'R'),
#                      values = c('blue', 'red', 'black'))

require(gridExtra)
plot1 <- ggplot(Z, aes(x = 1:101, y = S)) + geom_line()
plot2 <- ggplot(Z, aes(x = 1:101, y = I)) + geom_line()
plot3 <- ggplot(Z, aes(x = 1:101, y = R)) + geom_line()
grid.arrange(plot1, plot2, plot3, nrow = 3)

# Vamos a estimar como afecta el valor de a y b a una epidemia
# es decir, estimaremos E S[t] para diferentes valores
# de a y b, a -> [0.0001, 0.001] y b -> [0.1, 0.5].
# Luego vamos a graficar los resultados en un grafico 3-D

SIR = function(a, b, N, T){
  # Simula el modelo SIR desde un tiempo 0 a T
  # devolviendo el numero de susceptibles, infectados
  # y removidos en el tiempo T
  S = N
  I = 1
  R = 0
  for (i in 1:T) {
    S = rbinom(1, S, (1 - a) ^ I)
    R = R + rbinom(1, I, b)
    I = N + 1 - S - R
  }
  return(c(S, I, R))
}
# Población
N = 1000
# Tiempo
T = 100
# Valores multiples para a y b
a = seq(0.0001, 0.001, by = 0.0001) # 10 valores
b = seq(0.1, 0.5, by = 0.05) # 10 valores
SIR(a, b, N, T)

# Fijar el tamaño de una muestra de 400
n.reps = 400
# Archivo donde se almacena los resultados
f.name = "SIR_grid.dat"
# Estimar la cantidad de susceptibles en un tiempo T, para
# cada combinación de a y  b

# Escribiendo cabecera
write(c("a", "b", "S_T"), file = f.name, ncolumns = 3)
for (i in 1:length(a)) {
  for (j in 1:length(b)) {
      S.sum = 0
      for (k in 1:n.reps) {
        S.sum = S.sum + SIR(a[i], b[j], N, T)[1]
      }
      # Escribimos los datos
      write(c(a[i], b[j], S.sum / n.reps), file = f.name,
            ncolumns = 3, append = TRUE)
  }
}
# Dibujando grafico 3-D
g = read.table(f.name, header = TRUE)
library(lattice)
# wireframe.- permite comparar una variable con otras
print(wireframe(S_T ~ a * b, data = na.omit(g),
                scales = list(arrows = FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab = "a", ylab = "b",
                zlab = "E S[T]"))
# Se puede observar lo siguiente:
# N * a = B, Na es el numero esperado de nuevos infectados
# en el tiempo 1 y B es el numero esperado de infectados
# quienes han sido removidos en el tiempo 1

# En etapas iniciales de la epidemia:
# Cuando Na > B --> GRAN EPIDEMIA. Implica que el numero
# esperado de nuevos infectados en el tiempo 1 es mayor
# que el numero esperado de infectados que han sido
# removidos en el tiempo 1.

# Cuando Na <= B --> REDUCCION DE LA EPIDEMIA.


### DISTRIBUCION EXPONENCIAL Y POISSON ###
# Dos cajeros en un banco atienden a clientes desde una
# sola cola, el tiempo que le toma a un cajero atender
# a un cliente seleccionado al azar se distribuye EXP(1 / 5)
# para que su tiempo promedio de servicio sea de 5min. Los
# otros tiempos de servicio del cajero mas experimentado se
# distribuyen como EXP(1 / 4), con un promedio de 4min.
# Los cajeros trabajan independientemente.

## Escenario 1:
# Ambos cajeros estan ocupados y eres el proximo en ser
# atendido. ¿Cual es la probabilidad de que tome mas de 5
# minutos antes de ser atendido?

# Sea X1 y X2 los tiempos de espera hasta que los cajeros
# esten libres para atender nuevos clientes. El tiempo
# hasta que uno de ellos me atienda V = min(X1, X2).
# (probabilidad P) P{V < 5}0
m = 100000
lam1 = 1 / 5 # Tasa de demora del cajero 1
lam2 = 1 / 4 # Tasa de demora del cajero 2
x1 = rexp(m, lam1)
x2 = rexp(m, lam2)
# Obtener los minimos de los vectores
V = pmin(x1, x2)
V
hist(V, prob = T)
mean(V > 5)
# P{V > 5} = P{X1 > 5, X2 < 5} = 
# e ^ (-(1 / 5) * 5) * e ^ (-(1 / 4) * 5) = e ^ (-9 / 4)

## Escenario 2
# Para finalizar su estancia en el banco, debe ser
# atendido por ambos cajeros en secuencia. No es necesario
# esperar para comenzar el servicio con cualquiera de 
# los dos cajeros.
# ¿Cual es la probabilidad de que te tome mas de 5 min
# terminar? Con X1 y X2 distribuidos como en el escenario 1,
# el tiempo que le lleva terminar T = X1 + x2
m = 100000
lam1 = 1 / 5
lam2 = 1 / 4
x1 = rexp(m, lam1)
x2 = rexp(m, lam2)
t = x1 + x2
mean(t > 5)
hist(t, prob = T)

### REDUNDANCIA PARALELA EN COMUNICACIONES SATELITALES ###
# Una computadora a bordo de un satelite, controla las
# funciones cruciales de un satelite de comunicaciones,
# fallara si si su CPU esta desactivada por la radiacion
# cosmica. La confiabilidad de la CPU es importante porque
# no es factible reparar un satelite deshabilitado. Suponga
# que el nivel de radiacion es tal que los eventos fatales
# suceden a una CPU de ese tipo. Sucede una vez cada 4 años.
# lambda = 1 / 4. La vida util aleatoria de esa CPU
# es X ~ EXP(1 / 4)
# Funcion de confiabilidad
# Para mayor confiabilida, supongamos que conectamos
# 5 CPU's en paralelo. Entonces la vida util del sistema de
# CPU resultante es:
# W = max(X1, X2, ..., X5), donde Xi se distribuyen como
# EXP(1 / 4). Queremos evaluar es valore estimado E(W).
m = 100000
n = 5
lam = 1 / 4
x_i = rexp(m * 5, lam)
DTA = matrix(x, nrow = m)
w = apply(DTA, 1, max)
mean(w)
mean(w > 5) # Aproximacion P{W > 5}
