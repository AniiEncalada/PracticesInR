#### Metodo de integración ######
#### Integración de Riemann ####
#P{-1 < Z <= 1} = phi(1)- phi(-1) = 68.3%
#J= P{a < Z <= b} = phi(b)-phi(a) = integral a,b de la funcion phi(z)dz
#donde a < b y la funcion phi(z) = 1/sqrt(2*pi)*e^(-z^2/2)
#P{-1 < Z <= 1}
pnorm(1) - pnorm(-1)
#riemann
m = 5000; a=0; b=1 #constantes
w = (b - a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
h = (1/2) * exp(-g /2) #vector de m alturas
sum(w*h) #area total


#Para a=0 y b=1 con los valos de m = 10,20,50 y 500 
#cual de estos valores nos da el resultado con 5 decimales de exactitud
m = 10; a=0; b=1 #constantes
w = (b-a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
const = 1/sqrt(2*pi)
h = const * exp(-g^2 /2) #vector de m alturas
sum(w*h) #area total
#---------------------------------------
m = 20; a=0; b=1 #constantes
w = (b-a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
const = 1/sqrt(2*pi)
h = const * exp(-g^2 /2) #vector de m alturas
sum(w*h) #area total
#---------------------------------------
m = 50; a=0; b=1 #constantes
w = (b-a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
const = 1/sqrt(2*pi)
h = const * exp(-g^2 /2) #vector de m alturas
sum(w*h) #area total
#---------------------------------------
m = 500; a=0; b=1 #constantes
w = (b-a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
const = 1/sqrt(2*pi)
h = const * exp(-g^2 /2) #vector de m alturas
sum(w*h) #area total
#Para m = 5000, modifique el programa para encontrar P{1.2<z <=2.5}.
#Compare el valor exacto usando pnorm
m = 5000; a=1.2; b=2.5 #constantes
w = (b-a)/m #ancho de cada rectangulo
g = seq(a + w/2, b - w/2, length = m) #m puntos
const = 1/sqrt(2*pi)
h = const * exp(-g^2 /2) #vector de m alturas
sum(w*h) #area total
pnorm(2.5)-pnorm(1.2)

#### Integración MonteCarlo ####
#Seleccionar u puntos(0,1)
#set.seed(12)
m = 100000#Numero de punto aleatorios
a = 0; b = 1 #intervalo
w = (b - a) / m
u = a + (b - a) * runif(m) #vector de m punto aleatorios
h = dnorm(u) #altura de cada uno de los puntos
sum(w * h)

#### Aceptacion y rechazo ####
set.seed(12)
m=500000
u=runif(m, 0,1)
h=runif(m,0,0.4)
frac.acc = mean(h<dnorm(u))
0.4*frac.acc

#### Aleatorio de muestreo ####
#Evaluar el area bajo la curva entre un intervalo dado
set.seed(2020)
m=500000
a=0; b=1
z=rnorm(m)
mean(z>a & z<b)

### Metodo del trapecio ####
trapezoid = function(ftn, a, b, n=100){
  #Integracion numerica de ftn desde a hasta b
  #usando la regla del trapecio con n subdivisiones
  #ftn es una funcion de una variable x
  #asumir que a < b y n es un entero positivo
  #h es el ancho de cada intervalo
  h = (b-a)/n
  #x.vec es un vector de a hasta b que tambien marca el comienzo y el final
  #de cada subintervalo
  x.vec = seq(a,b,by = h)
  #Dada ftn es f(x), f.vec valores de y para cada x-sub-i
  f.vec = sapply(x.vec, ftn)
  #Implementar la regla del trapecio sumando todas las areas
  T = h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
  return(T)
}
#Estimar la integral entre 0 y 1 de la siguiente funcion:
#4x^3 dx = 1
ftn6 = function(x) return(4*x^3)
trapezoid(ftn6, 0, 1, n=20)
trapezoid(ftn6, 0, 1, n=40)
trapezoid(ftn6, 0, 1, n=60)
trapezoid(ftn6, 0, 1, n=80)
trapezoid(ftn6, 0, 1, n=100)
trapezoid(ftn6, 0, 1, n=20000)

#### Metodo de Simpson ####
#Divide el intervalo [a,b] en n subintervalos donde n debe ser par
#luego para cada pares de intervalos consecutivos se calcula (aproxima) 
#el area de f(x) atravez de una parábola (polinomio de grado 2) 
#Sea u < v < w van a estar separados por 3 puntos h, Para x que pertenece
#al intervalo [u, w] queremos aproxima f(x) atravez de una parabola que pasa
#atravez de los puntos (u, f(u)), (v, f(v)), y (w, f(w)). Para estos puntos 
#hay exactamente una parabola p(x)
simpson = function(ftn, a,b,n=100){
  #Integral numerica de ftn, desde a hasta b
  #usando la regla se simpson con n subdivisiones
  #ftn es una función de una sola variables
  #asumimos que a < b y n es un entero positivo par
  #Asegurar que n es par(4 es el numero minimo de intervalos para la regla de
  #simpson)
  n = max(c(2*(n%/%2), 4))
  #h es el ancho de cada intervalo
  h = (b-a)/n
  #x.sib-i impar
  x.vec = seq(a+h, b-h, by=2*h)
  #para valores pares
  x.vec1 = seq(a+2*h, b-2*h, by = 2*h)
  #calcular los valores para y
  f.vec = sapply(x.vec , ftn)
  f.vec1 =  sapply(x.vec1 , ftn)
  S = h/3 * (ftn(a) + 4*sum(f.vec) + 2*sum(f.vec1) + ftn(b))
  return(S)
}
ftn6 = function(x) return(4*x^3)
simpson(ftn6, 0, 1, 2)




