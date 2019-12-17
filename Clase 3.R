# FUNCIONES CON VECTORES

x = c(1,2,3,4,5)

# Funcion suma
sum(x)
# Funcion de suma acumulada (resultado es un vector)
cumsum(x)
# Funcion Maximo y Minimo
max(x)
min(x)
# Funcion Promedio
mean(x)
# Funcion Mediana
median(x)
# Funcion varianza
var(x)
# Funcion Desviacion Stadar
sd(x)

# Funcion de producto
prod(x)
# Funcion de producto Acumulado (resultado es un vector)
cumprod(x)
# Funcion de quantiles
quantile(x)


x = c(2,6,3,7,9,1,4,7)
# Funcion de orden creciente
sort(x)
# Funcion que pone el vector invertido
rev(x)

x = c(1,2,3,4,5)
y = c(2,1,5,4,3)
# Funcion de covarianza entre dos conjuntos de datos
cov(x,y)
# Funcion de Coeficiente de correlacion
cor(x,y)

# VECTORES DE CARACTER

colores = c('amarillo','rojo','verde')
mas.colores = c(colores,'azul','negro')
otros.colores = c('naranja','rosa',1)

# VARIABLES Y OPERACIONES LOGICAS

x = 11:20
# Vector con booleanos
x == 15
x < 15
x > 15
x <= 15
x >= 15
x != 15
# Contar cuantos cumplen la condicion
sum(x == 15)
sum(x < 15)
sum(x > 15)
sum(x <= 15)
sum(x >= 15)
sum(x != 15)

# EJERCICIOS
# 1. Calcular la media y mediana de un vector
# y el numero de valores que estan por debajo
# de la media y de la mediana
x = c(1,5,7,9,3,5,6,2,4,7,5,6,9,8,6,2,5,1,4)
mean(x)
median(x)
sum(x < mean(x))
sum(x < median(x))

z = 1:5
# Mostrar solamente los verdaderos
z[c(TRUE,FALSE,TRUE,FALSE,TRUE)]
z = 3:7
z[c(TRUE,FALSE,TRUE,FALSE,TRUE)]

# NUMEROS ALEATORIOS
# Segun la distribucion uniforme
x = runif(10)
x[x < 0.4]
x < 0.4
x[(x < 0.2) | (x > 0.8)]
sum((x < 0.2) | (x > 0.8))
x[(x > 0.2) & (x < 0.8)]
sum((x > 0.2) & (x < 0.8))

# Ver posiciones de acuerdo a una condicion
which((x < 0.2) & (x < 0.6))

# CREACION DE FUNCIONES
# nombreFuncion = function(parametros){ expresiones }
media = function(x){
  sum(x) / length(x)
}
y = 1:100
media(y)
mean(y)

# GRAFICOS EN R
x = c(1,1,1,1,1,2,2,3,3,3,5,6,6,7,7,7)
# Tabular los datos en x
table(x)
# Diagrama de barras
barplot(table(x))
# Histograma
x = runif(100)
hist(x)
# Scaterplots o grafico de dispersion
x = runif(100)
y = runif(100)
plot(x,y)
# QQ-plots.- Verificar si dos vectores siguen la misma distribucion 
# Distribucion normal Standar
x = rnorm(1000)
y = rnorm(1000)
qqplot(x,y,main='x e y con la distribucion Normal')

# Distribucion normal
a = rnorm(1000, mean = 3, sd = 2)
qqplot(x,a,main='x N(0,1), a N(3,2)')

# Distribucion t con grados de libertad
b = rt(1000, df = 2)
qqplot(x,b,main='x N(0,1), a t(2)')

# OTRAS INSTRUCCIONES
# Sentencia for
# for(nombre_indice in vector){ comandos }
x = numeric(10)
fibonacci_12 = numeric(12)
fibonacci_12[1] = fibonacci_12[2] = 1
for (i in 3:12) {
  fibonacci_12[i] = fibonacci_12[i - 2] + fibonacci_12[i - 1]
}
fibonacci_12

# DISTRIBUCION UNIFORME
# En el intervalo comprendido entre min y max
# por defecto en min es 0 y el max 1
# dunif -> proporciona la funcion de densidad
# punif -> proporciona la funcion de distribucion
# qunif -> proporciona la funcion de cuantiles
# runif -> genera valores pseudoaleatorios

# Uso
# dunif(x, min = 0, max = 1, log = FALSE)
# punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
# qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
# runif(n, min = 0, max = 1)

# Argumentos
# x,q: vectore de cuantiles
# p: vector de probabilidades
# n: numero de observaciones
# min, max: extremos inferior y superior
# log, log.p: valores logicos, si son verdaderos,
# las probabilidades p se dan como probabilidades log(p)
# lower.tail: valor logico, si es verdadero, las probabilidades
# son P[X <= x](Funcion de distribucion acumulada), en otro caso
# P[X > x]

runif(10)
runif(10, min = -1, max = 2)
# especificar la semilla
set.seed(32789)
runif(5)

# INTRODUCCION A LA SIMULACION MONTECARLO
beads = rep(c('red', 'blue'), times = c(2, 3))
# Elegir de manera aleatoria
sample(beads, 1)
# Veces que se realiza el experimento
B = 10000000
# Simular un evento
events = replicate(B, sample(beads, 1))
tab_events = table(events)
tab_events
# Obtener la proporcion
prop.table(tab_events)
