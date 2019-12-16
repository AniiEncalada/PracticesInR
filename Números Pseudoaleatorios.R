##Congruencial Mixto
d = 53 #Modulo
a = 23 #Multiplicador
b = 0 #Constante aditiva
s = 21 #Semilla
m = 60 #Longitud de corrida

r = numeric(m)
r[1] = s
for (i in 1:(m-1)) {
  r[i+1] = (a * r[i] + b) %% d  
}
r
