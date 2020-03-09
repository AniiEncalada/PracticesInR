# Escriba un programa que explore la relación entre n y p. Recorra habitaciones de
# tamaño n = 1 a n = 60, encontrando p para cada tamaño de habitación. Luego realice
# un grafico p contra n (y = p, x = n)

n=1:60
n
p = 1 - (n / 365)
p
plot(n, p,type="l")

# Escriba un programa que resuelva el mismo problema utilizando simulación. Use la
# semilla = 1237 y 10000 iteraciones. Es decir, debe encontrar la probabilidad de que
# haya por lo menos unas coincidencia.

# Simulacion Montecarlo
set.seed(1237)
m = 10000
a = 0
b = 23                                                                     
w = (b - a) / m
x = a + (b - a) * runif(m)
x
h = (x / 365)
h
sum(w * h)

# Metodo del trapecio
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

ftn6 = function(x) return(x / 365)
trapezoid(ftn6, 0, 23, n = 10000)


# Wikipedia
total=1; 
for(i in 1:60) { 
  total=total*((366-i)/365)
  cat("La probabilidad de que en un grupo de ", i, " personas, dos cumplan años el mismo día es de: ", 1-total,"\n")
}