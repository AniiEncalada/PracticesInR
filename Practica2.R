######## Crear vectores #######
#C concatenar
codes = c(380, 124, 818)
country = c('italy', 'canada', 'egypt')

#Nombres
codes = c(italy=380, canada=124, egypt=818)
class(codes)
#Obtener el nombre se ha asigado a los valores de la cadena
names(codes)

codes = c(380, 124, 818)
country = c('italy', 'canada', 'egypt')
names(codes)=country #Iguala valores
codes

####### Secuencias #####
seq(1, 10) #inicio, final
seq(1, 10, 2) #inicio, final, condicion
1:10 #indexar
class(1:10) #Solo enteros
seq(1, 10, 0.5)
class(seq(1, 10, 0.5))

###### Accesos #####
#Para acceder a un sub elemento
codes[2]
#Para acceder a un subconjunto
codes[c(1:2)]
codes[c(1:3)]
codes[c(1,3)]
codes['canada']
codes[c('egypt', 'italy')]

##### Coercion ####
#Cuando se usa una lista donde no todos los elementos 
#correcponden al mismo tipo de dato, R trata de definir
#un tipo que se puede aplicar a todos.
x = c(1, 'canada', 3)
class(x)
x=1:5
y = as.character(x) #Combertir en cadena de texto
y
as.numeric(y) #Converit en numero

##### Not availables ####
#Cuando no puede interpretar el tipo de dato lo pasa
#a no available // equivalente de null
x = c('1', 'b', '3')
class(x)
as.numeric(x)

##### Ordanar ####
#Sort
library(dslabs)
data("murders")
sort(murders$total)

#order - da indices
x = c(31, 4, 15, 92, 65)
sort(x)
index= order(x)
x[index]
index

murders$state[1:6]
murders$abb[1:6]
index = order(murders$total)
murders$abb[index]
murders$state[index]

##### Maximo y minimo ####
#max and which.max
max(murders$total) #Obtener máximo
iMax = which.max(murders$total) #Indice del maximo
murders$state[iMax]

#min and which.min
min(murders$total) #Obtener minimo
iMin = which.min(murders$total) #indice del minimo
murders$state[iMin]

##### Recycling // reciclar #####
x = c(1, 2, 3)
y = c(10, 20, 30, 40, 50, 60, 70)
x+y

##### Opereciones Atirmeticas ####
#Vector arithmetics
#Rescaling a vector
inches = c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)
inches*2.54
inches-69
murderRate = murders$total/murders$population*100000
murders$abb[order(murderRate)]
murders$state[order(murderRate)]
