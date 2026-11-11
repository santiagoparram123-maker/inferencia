setwd("~/Santiago/Universidad/inferencia")
library(readr)
icfes_2016 <- read_delim("icfes_2016.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
View(icfes_2016)

#omitir datos faltantes 
icfes = na.omit(icfes_2016)
# muestreo aleatoria simple 
N = nrow(icfes)# nrow = cantidad de filas; Total de mi Poblacion 
n = 1000 # tamaño de muestra 
individuos <- sample(N,n) # muestreo aleatorio simple sample(poblacion,tamaño muestra) sample nos da la lista de individuos ; <- sirve para asignar 
muestra = icfes[individuos,] # aca ya a esa lista que creamos la unimos con las demas variables y creamos el dataset 

# cuantas muestras se pueden seleccionar ?
choose(N,n)

#muestreo sistematico 
N = 120 # tamaño de Poblacion
n = 30  # tamaño de muestra 
ancho = ceiling(N/n); ancho 
k=ancho
arranque= sample(1:k,1);arranque # inicio 
indivi = seq(arranque, arranque+k*(n-1),k);indivi
# Tarea hacerlo con la base icfes 
icfes = na.omit(icfes_2016)
N = nrow(icfes)
n = 1000
ancho = ceiling(N/n); ancho 
k=ancho
arranque= sample(1:k,1);arranque # inicio 
indivi = seq(arranque, arranque+k*(n-1),k);indivi
muestra = icfes[indivi,]

# Muestreo estratificado 
N1 = 352
n1 = 50 
N1.i = c(8,35,189,115,5); N1.i
n1.i = round((n1/N1)*N1.i,0); n1.i 
# tarea seleccionar de forma aleatoria las empresas. 
sum(n1.i)
