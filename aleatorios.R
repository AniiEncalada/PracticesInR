## Congruencial mixto #####
## Periodo completo ####
d=53 #modulo
a=20 #multiplicador
b=0 #constante aditiva
s = 21 #semilla
m= 60 #longitud de corrida
r = numeric(m) #recibe tamaño
r[1]=s
for (i in 1:(m-1)) {
  r[i+1]=(a*r[i]+b)%%d
}
r

############ Peiodo incompleto #####
d=53 #modulo
a=23 #multiplicador
b=0 #constante aditiva
s = 21 #semilla
m= 60 #longitud de corrida
r = numeric(m) #recibe tamaño
r[1]=s
for (i in 1:(m-1)) {
  r[i+1]=(a*r[i]+b)%%d
}
r
