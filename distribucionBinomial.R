####Distribución binomial####
#dbinom -> funcion de masa de probabilidad
#pbinom -> funcion de distribucion
#qbinom -> funcion de cuartiles
#rbinom -> genera valores aleatorios
####uso:####
#dbinom(x, size, prob, log=FALSE)
#pbinom(q, size, prob, lower.tail=TRUE, log.p=FALSE)
#qbinom(p, size, prob, lower.tail=TRUE, log.p=FALSE)
#rbinom(n, size, prob)
#Argumentos
#x, q: verctor de cuantiles
#p: vector de probalidades
#n: numero de observaciones. Por defecto es 1
#log, log.p: son valores lógicos; si son TRUE, las probabilidades p se dan como log(p)
#lower.tail: es un valor logico; si en TRUE, las probabilidades se refieren a la probabilidad acumulada
  #P[X<=x], en otro caso es lo contrario P[X>x]
####Ejemplos####
#ej1. Calcular la probalidad de obtener 4 caras al lanzar 6 veces una moneda
#P[X=4], con X -> B(6, 0.5)
dbinom(x=4, size = 6, prob = 0.5)
#Calcular la probabilidad de obtener como mucho 4 caras al lanzar 6 veces una monera perfecta
#P[X<= 4] con X -> (6, 0.5)
pbinom(q = 4, size = 6, prob = 0.5)
pbinom(4, 6, 0.5)
#Calcular el valor de x, tal que la probabilidad de que esta variable tome un valor determinado P[X<=x]=0.89
qbinom(0.89, 6, 0.5)
#Generar 10 valores pseudoaleatorios de una B(6, 0.5)

rbinom(10, 6, 0.5)
#Supongamos que el 10% de los tubos producidos por una máquina son defectuosos y supongamos tambien, 
#que produce 15 tubos cada hora. Cada tubo es independiente de los otros. Se juzga que el proceso esta 
#fuera de control cuando se produce más de 4 tubos defectuosos en una hora concreta. 
#Simular el número de tubos defectuosos producidos por la máquina en cada hora a lo largo de un periodo 
#de 24 horas y determinar si el proceso esta fuera de control en algun momento.
tubosDefectuosos = rbinom(24, 15, 0.1)
tubosDefectuosos
any(tubosDefectuosos>4)
sum(tubosDefectuosos>4)

##### Distribución de Poisson ####
#dpois -> funcion de probabilidad
#ppois -> funcion de distribución
#qpois -> funcion de cuantiles
#rpois -> genera valores aleatorios
####Uso:####
#dpois(x, lamda)
#ppois(q, lamda, lower.tail=TRUE)
#qpois(p, lamda, lower.tail =TRUE)
#rpois(n, lamda)
#####Ejemplo:####
#Supongamos que el número de accidentes que ocurren en una carretera al año, 
#tiene una distribución de poisson de medie 3.7.
#a. Probabilidad de que en un año hayan 6 accidentes
#b. Probabilidad de que en un año haya menos de 2 accidentes
#c. Probabilidad de que en un año hayan más de 8 accidentes.
#d. Número maximo de accidentes que se produciran con probabilidad mayor o igual a 0.9
#e. Simular el número anual de accidentes que se produciran en un periodo de 20 años.
#a. 
dpois(6, 3.7)
#b. 
ppois(2, 3.7)
#c. 
ppois(8, 3.7, lower.tail = FALSE)
#d.
qpois(0.9, 3.7)
#e.
rpois(20, 3.7)
#### Distribucion normal ####
#Entandar -> media 0 y sd 1
#dnorm(x, mean=0, sd=1)
#pnorm(q, mean=0, sd=1, lower.tail=TRUE)
#qnorm(p, mean=0, sd=1, lower.tail =TRUE)
#rnorm(n, mean=0, sd=1)
datos=rnorm(100, 3, 2)
datos
hist(datos)
hist(datos, freq = FALSE)
curve(dnorm(x, 3, 2), add=TRUE)
####Distribución exponencial####
#dexp->funcion de densidad
#pexp -> funcion de distribución
#qexp -> funcion de cuantiles
#rexp -> genera variables aleatorias
####USO:####
#dexp(x, rate=1, log=FALSE)
#pexp(q, rate=1, lower.tail=TRUE, log.p=FALSE)
#qexp(p, rate=1, lower.tail=TRUE, log.p=FALSE)
#rexp(n, rate=1)
#### Ejemplo ####
#Supongamos que el tiempo de servicio en un banco se 
#modeliza con una variable aleatoria con una distribución 
#exponencial de razon 3 clientes por minuto. Calcular la 
#probabilidad de que un cliente sea servido en menos de 
#un minuto
#P[X<=1], X ->exp(3)
pexp(1,3)
#Simular mediante en metodo de la transformada inversa 
#10 valores de la distribución exp(3) usando una semilla 111
set.seed(111)
r=runif(10)
datos= -log(1-r)/3
datos
#Simular 1000 valores de una distribución exponencial y 
#comparar histograma con la funcion de densidade teorica 
#de la distribución
set.seed(111)
r = runif(1000)
datos=-log(1-r)/3
hist(datos, freq = FALSE)
curve(dexp(x, 3), add=TRUE)

set.seed(111)
x=rexp(10,3)
x
