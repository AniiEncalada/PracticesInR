#### Caso de estudio: Epidemias ####
  #### Modelo Sir ####
#Sir (Suceptibles,Infectados, Romoverlos)
#Suceptible: Todavia no tiene la enfermedad
#Infectados: Tienen actualmente la enfermedad
#Romovidos: han tenido la enfermedad y se han recuperado y ahora son inmunes o han muerto.
#En el tiempo t se tiene un numero de S, I, R:
#S(t) = número de suceptible en el tiempo t;
#I(t) = número de infectados en el tiempo t;
#r(t) = número de personas que se han recuperadi o muerto en el tiempo t;

#En cada tiempo t cada infectado tiene:
#1. probabilidad a de infectar a un suceptible; y
#2. probabilidad b de ser R (de que se recupere o muera)

#En el tiempo 0 (inicio)
#S(0) = N; tamaño de la poblacion
#I(0) = 1;
#R(0) = 0;
#Total de la población N + 1:
#S(t) + I(t) + R(t) = N + 1

#Reglas
#Para cada tiempo t, la probabilidad de que un suceptible permanezca sin la enfermedad
# es: (1-a)^I(t)

#Si el suceptible no se infecta, es decir: 
#S(t+1) ~ binom(S(t), (1-a)^I(t))

#Ya que cada infectado tiene la probablidad b de ser removido, es decir:
#R(t+1) ~ R(t) + binom(I(t), b)

#I(t+1) * N+1 - R(t+1) - S(t+1)

SIRsim = function(a, b, N, T){
  #T = nro de periodos de tiempo
  #a es la tase de infeccion, b es la tasa de removidos
  #N cantidad de suceptiebles
  #1 es la persona que esta infectada
  #Tamaño de simulacion T
  # T = 100
  S = rep(0, T+1)
  I = rep(0, T+1)
  R = rep(0, T+1)
  S[1] = N
  I[1] = 1
  R[1] = 0
  for (i in 1:T) {
    #SDe la regla 2
    S[i+1] = rbinom(1, S[1], (1-a)^I[i])
    R[i+1] = R[1]+rbinom(1, I[i], b)
    I[i+1] = N +1 - R[i+1] - S[i+1]
  }
  return(matrix(c(S, I, R), ncol=3))
}

N= 1000
T= 100
a=0.0005
b=0.1
(N*a)
b
Z= SIRsim (a,b,N,T)
colnames(Z) = c('S', 'I', 'R')
Z
library(tidyverse)
Z = data.frame(Z)
# ggplot(Z, aes(x=1:101)) +
#   geom_line(aes(y= S, colour = 'blue'))+
#   geom_line(aes(y= I, colour = 'red'))+ 
#   geom_line(aes(y= R, colour = 'black')) +
#   scale_color_manual(labels= c("S", "I", "R"),
#                      values = c("blue", "red", "black"))
require(gridExtra)
plot1 <- ggplot(Z, aes(x=1:101, y = S))+ geom_line()
plot2 <- ggplot(Z, aes(x=1:101, y = I))+ geom_line()
plot3 <- ggplot(Z, aes(x=1:101, y = R))+ geom_line()
grid.arrange(plot1, plot2, plot3, nrow = 3)

#Vamos a estimar como afecta el valor de alfa y beta a una epidemia,
#Es decir, estimaremos la cantidad de suceptible en un tiempo t S[t] 
#para diferentes valores de a y b
#a ->[0.0001, 0.001] y b -> [0.1, 0.5]
#Luego vamos a graficar los resultados en un grafico 3D
SIR = function(a, b, N, T){
  #simula el modelo SIR desde un tiempo 0 a t 
  #Devuelve el número de suceptibles, infectados y removidos
  #en el tiempo t
  S = N
  I = 1
  R = 0
  for (i in 1:T) {
    S = rbinom(1, S, (1-a)^I)
    R = R + rbinom(1, I, b)
    I = N + 1 - S - R
  }
  return(c(S, I, R))
}
N = 1000
T = 100
#Valores multiples para a y b
a = seq(0.0001, 0.001, by=0.0001)#10 valores
b = seq(0.1, 0.5, by=0.05)#10 valores
a
b
T
N
SIR(a, b, N, T)
#Tamaño de una  muestra de 400
n.reps = 400
#Archivo donde se almacenan los resultados
f.name = "SIR_grid.dat"
#Estimar E S[T] para cada combinación de a y b
write(c("a", "b", "S_T"), file = f.name, ncolumns = 3)
for (i in 1:length(a)) {
  for (j in 1:length(b)) {
    S.sum = 0
    for (k in 1:n.reps) {
      S.sum = S.sum + SIR(a[i], b[j], N, T)[1]
    }
    write(c(a[i], b[j], S.sum/n.reps), 
          file = f.name,
          ncolumns = 3, append = TRUE)
  }
}
g = read.table(f.name, header = TRUE)
library(lattice)
print(wireframe(S_T ~ a*b, data = na.omit(g),
                scales= list(arrows= FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab="a", ylab = "b", 
                zlab = "E S[T]"))#Comparar una variable con otras variables
#Se puede observar lo siguiente:
#Na = B. Na es el número esperado de nuevos infectados
#en el tiempo 1 y el valor de B es el número esperado
#de infetados quienes han sido removidos en el tiempo 1

#En etapas iniciales de la epidemia:
#Cuando Na > B --> Gran Epidemia. Implica que el número 
#esperado de nuevos infectados en el tiempo 1 es mayor
#que el número esperado de infectados que han sido
#removidos en el tiempo 1

#Cuando Na =< B --> Reducción de Epidemia. 

#### Aplicaciones de distribuciones de Poisson y Exponencial ####
#Dos cajeros en un banco atienden a clientes desde una solo cola, el tiempo que 
#le toma a un cajero atender a un cliente selccionado al azar se
#distribuye exponencialmente con el valor de EXP(1/5) para que su
#tiempo promedio de servicio sea de 5 min. Los otros tiempos de servicio
#del cajero más esperimentado se distribuyen como EXP(1/4), con un promedio
#de 4 min
#Los cajeros trabajan independientemente.

#ESCENARIO 1: ####
#Ambis cajeros estan ocupados y eres el proximo en ser atendido
#Cual es la probabilidad de que tome más de 5 minutos antes de
#ser atentido.

#Sea x1 y x2 los tiempos de espera hasta que los cajeros esten libres 
#para atender nuevos clientes. El tiempo hasta que uno de ellos me atienda
#V = min(x1, x2). P{V > 5}
m = 100000
lam1 = 1/5
lam2 = 1/4
x1 = rexp(m, lam1)
x2 = rexp(m, lam2)
V = pmin(x1, x2) #Obtener los valores minimos de los vectores
V
hist(V, prob = T)
mean(V > 5)
#Funcion de densidad y acumulada para cada dustribución de probabilidad
#P{V>5} = P{x1 >5, x2 >5} = e^(-(1/5)*5) e^(-(1/4)*5)= e^(-9/4)

#ESCENARIO 2 ####
#Para finalizar su estancia en el banco debe ser atendido por ambos cajeros 
#en secuencia. No es necesario esperar para comenzar el servicio con 
#cualquiera de los dos cajeros. 
#Cual es la probabilidad de que te tome mas de 5 minutos terminar.
#Con x1 y x2 distribuidos como en el escenario 1, el tiempo que 
#le lleva terminar T = x1 + x2
m = 100000
lam1 = 1/5
lam2 = 1/4
x1 = rexp(m,lam1)
x2 = rexp(m, lam2)
t = x1 +  x2
t
mean(t > 5)
hist(t, prob = T)

# Redundancia paralela en comunicaciones satelitales ####
#Una computadora abordo de un satelite controla las funciones 
#cruciales de un satelite de comunicaciones, fallará
#si su cpu esta desactivado por la radiación cósmica
#La confiabilidad de la cpu es importante porque
#no es factible reparar un satelite deshabilitado.
#Suponga que el nivel de radiacion es tal que los e
#ventos fatales suceden a una cpu de este tipo.
#Esto sucede una vez cada cuatro años -- > lambda = 1/4
#La vida util aleatoria de esa cpu se distribuye 
#exponencialmente con X ~ Exp(1/4)
#Función de confiabilidad (Pendiente)

#Para mayor confiabilidad supongamos que conectamos
#5 CPUs en paralelo. Entonces la vida util del sistema
#de cpu resultante es:
#W = max(x1, x2, ..., x5), donde Xi se distribuye como
#EXP(1/4). Queremos evaluar el valor estimado de E(W)

m = 100000
n=5
lam=1/4
x=rexp(m*n, lam)
DTA = matrix(x, nrow = m)
w = apply(DTA, 1, max)
mean(w) #Aproximación o alor estimado  de w E(W)
mean(w>5)
