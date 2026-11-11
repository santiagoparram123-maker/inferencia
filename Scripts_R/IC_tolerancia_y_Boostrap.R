# Instalar y cargar el paquete
install.packages("tolerance")
library(tolerance)


datos=rnorm(30,mean=96.2, sd = 0.8)

intervalo <- normtol.int(x = datos, 
                         alpha = 0.05, 
                         P = 0.90,# nivel de tolerancia 
                         side = 2, # bilateral
                         method="EXACT" #factor exacto
                         )
print(intervalo)
intervalo

#ejemplo 2

diam=c(1.01, 0.97, 1.03, 1.04, 0.99, 0.98,
       0.99, 1.01, 1.03) #datos

#Intervalo de Confianza

t.test(diam,
       conf.level = 0.99)

#intervalo de predicción

promedio=mean(diam);promedio#promedio muestral
s=sd(diam);s#desviación estándar
nc=0.99;nc#Nivel de confianza
alpha=1-nc
n=length(diam)
gl=n-1
t=abs(qt(alpha/2,gl));t
se=s*sqrt(1+1/n);se
me=t*se;me
LI=promedio-me;LI
LS=promedio+me;LS

#límites de tolerancia

datos <- rnorm(9, mean = promedio, sd = s)

intervalo <- normtol.int(x = datos, 
                         alpha = 0.01, 
                         P = 0.95,# nivel de tolerancia 
                         side = 2, # bilateral
                         method="EXACT" #factor exacto
)
print(intervalo)

#Intervalos para la media con bootstrapping

library(boot)

set.seed(16092026)

#calcular la media
boot_promedio=function(datos,indices){
  return(mean(datos[indices]))
}
#Bootstrap
library(boot)

boots=boot(diam,
           statistic = boot_promedio,
           R=2000#réplicas
           )
boot.ci(boots,
        type="perc",
        conf=0.99)
