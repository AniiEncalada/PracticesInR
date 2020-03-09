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

# a.	En el escenario 1, simule la probabilidad que tu puedas 
# ser atendido por una cajera usando mean(x2==v). Explique el código 
# con comentarios [El valor exacto es 5/9]

valorExacto = 5 / 9 # Valor Exacto
m = 100000 # Numero de simulaciones
lamda1 = 1 / 5 # Tasa promedio 1
lamda2 = 1 / 4 # Tasa promedio 2
x1 = rexp(m, lamda1) # m Variables aleatorias con tasa promedio 1
x2 = rexp(m, lamda2) # m Variables aleatorias con tasa promedio 1
V = pmin(x1, x2) # Obteniendo valores mínimos entre x1 y x2
mean(V == x2) # Promedio entre los tiempos mínimos(V) y x2
valorExacto

# b.	En el escenario 1, simule el valor esperado utilizando mean(v) 
# y compárelo con el valor exacto (20/9), cual es el valor de la variación?

valorExacto = 20 / 9 # Valor Exacto
m = 100000 # Numero de simulaciones
lamda1 = 1 / 5 # Tasa promedio 1
lamda2 = 1 / 4 # Tasa promedio 2
x1 = rexp(m, lamda1) # m Variables aleatorias con tasa promedio 1
x2 = rexp(m, lamda2) # m Variables aleatorias con tasa promedio 1
V = pmin(x1, x2) # Obteniendo valores mínimos entre x1 y x2
mean(V) # Promedio entre los tiempos mínimos(V) y x2
valorExacto

# c.	Ahora suponga que solo hay un cajero con una tasa de 
# servicio de 1/5. Usted es el próximo en la fila para ser atendido. 
# Aproxime utilizando simulación, la probabilidad de que les tome 
# más de 5 minutos terminar de ser atendido.

m = 100000 # Numero de simulaciones
lamda = 1 / 5 # Tasa promedio
x1 = rexp(m, lamda) # m Variables aleatorias con tasa promedio 1 / 5
mean(V > 5) # Promedio de ser atendido en mas de 5 min

