#PH para la media, varianza desconocida

calificaciones=c(71, 93, 91, 86, 75, 73, 86, 82, 76, 57,84, 
                 89, 67, 62, 72,77, 68, 65, 75, 84)
mu=80#valor hipotético

prueba=t.test(calificaciones,#calificaciones
       mu=mu,
       alternative = "less" #less es a una cola a izquierda
       );prueba
n=length(calificaciones);n
gl=n-1;gl
alpha=0.05
t=qt(alpha,gl);t

tc=prueba$statistic;tc
pvalor=pt(tc,gl);pvalor


#PH para la diferencia de medias, varianzas desconocidas pero iguales

El=c(32, 37, 35, 28, 41, 44, 35, 31, 34)
Sc=c(35, 31, 29, 25, 34, 40, 27, 32, 31)

Prueba=t.test(El,#muestra1
              Sc,#Muestra2
              alternative = "greater",#preuba a una cola a derecha
              mu=0,
              var.equal = TRUE#varianzas iguales
              );Prueba
n1=length(El);n1
n2=length(Sc);n2
gl=n1+n2-2;gl
alpha=0.05
t=qt()
