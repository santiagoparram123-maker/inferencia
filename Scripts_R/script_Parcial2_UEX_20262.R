#*---------
#*# Script parcial 2
#*-------------



#---------
#-IC proporción
#----------

n=#tamaño de muestra
  x=#casos favorables
  nc=#nivel de confianza
  a=1-nc#error
prop.test(x,
          n,
          conf.level = nc,
          correct = FALSE)

pe=x/n#estimador puntual
se=sqrt(pe*(1-pe)/n);se#error estándar
z=abs(qnorm(a/2));z
me=z*se
LI=pe-me;LI
LS=pe+me;LS




#-----------------
# IC para la diferencia de proporciones
#-----------------

n1=#tamaño de muestra 1
n2=#tamaño de muestra 2
x1=#casos a favor 1
x2=#casos a favor 2
pe1=x1/n1;pe1#proporción estimada 1 o puntual
pe2=x2/n2;pe2#proporción estimada 2 o puntual 2
ep_dif=pe1-pe2;ep_dif#estimación puntual de la diferencia
se=sqrt(pe1*(1-pe1)/n1+pe2*(1-pe2)/n2);se#error estándar
nc=#niveles de confianza
a=1-nc#alpha
z=abs(qnorm(a/2));z
me=z*se;me#margen de error
LI=ep_dif-me;LI
LS=ep_dif+me;LS


prop.test(c(x1,x2),
          c(n1,n2),
          conf.level = nc,
          correct = FALSE)





#---------------------------
#-IC para el cociente de varianzas
#----------------------

datos1=c()
datos2=c()

nc=#nivel de confianza
a=1-nc;a#alpha
var1=var(datos1);var1#varianza muestral 1
var2=var(datos2);var2#varianza muestral 2
n1=length(datos1);n1#tamaño de muestra 1
n2=length(datos2);n2#tamaño de muestra 2
gl1=n1-1#grados de libertad 1
gl2=n2-1#grados de libertad 2
cociente=var1/var2;cociente
f1=qf(1-a/2,gl1,gl2);f1
f2=qf(1-a/2,gl2,gl1);f2

LI=cociente*(1/f1);LI
LS=cociente*f2;LS

#con la función
var.test(datos1,
         datos2,
         conf.level = nc)

#---------------
#-Muestras pequeñas
#----------------

#IC para la media

datos=c()
nc=#nivel de confianza
t.test(datos,
         conf.level = nc)

#-----
#programado
#---
a=1-nc#alpha
n=length(tiempo)#tamaño de muestra
gl=n-1#grados de libertad
t=abs(qt(a/2,gl));t
s=sd(tiempo);s#desviación estándar
se=s/sqrt(n);se#error estándar
me=t*se;me #margen de error
prom=mean(tiempo);prom#promedio 
LI=prom-me;LI
LS=prom+me;LS

#---------------------
#-- Ic para la diferencia de medias
#- varianzas desconocidas pero iguales
#---------------------

datos1=c()
datos2=c()
nc=#nivel de confianza

#probamos si las varianzas son iguales
var.test(datos1,
           datos2,
           conf.level = nc)

#Ic para la diferencia de medias
#usando t.test
t.test(datos1,
       datos2,
       conf.level = nc,
       var.equal = TRUE#varianzas iguales
)

#ic programado

n1=length(datos1);n1#tamaño de muestra 1
n2=length(datos2);n2#tamaño de muestra 2
gl=n1+n2-2#grados de libertad
var1=var(datos1);var1#marianza muestral 1
var2=var(datos2);var2#marianza muestral 1
s2=((n1-1)*var1+(n2-1)*var2)/gl;s2#varianza combinada
a=1-nc;a#alpha
t=abs(qt(a/2,gl));t
se=sqrt(s2*(1/n1+1/n2));se#error estándar
me=t*se;me#margen de error
prom1=mean(datos1);prom1#media muestral 1
prom2=mean(datos2);prom2#media muestral 2
dif_prom=prom1-prom2;dif_prom#estimador puntual
LI=dif_prom-me;LI
LS=dif_prom+me;LS


#--------------
#- IC para la diferencia de medias,
# varianzas desconocidas y diferentes
#------------------

datos1=c()
datos2=c()
nc=#nivel de confianza
  #verificando varianzas iguales o no
  var.test(datos1,
           datos2,
           conf.level = nc)
#ic para la diferencia de las medias
#usando t.test
t.test(datos1,
       datos2,
       conf.level = nc,
       var.equal = FALSE #varianzas diferentes
)

#sin usar t.test

prom1=mean(datos1);prom1#media muestral 1
prom2=mean(datos2);prom2#media muestral 2
dif_prom=prom1-prom2;dif_prom#estimador puntual
var1=var(datos1);var1#varianza muestral 1
var2=var(datos2);var2#varianza muestral 2
n1=length(datos1)
n2=length(datos2)
se=sqrt(var1/n1+var2/n2);se#error estándar
a=1-nc;a#alpha
num=(var1/n1+var2/n2)^2;num
den=((var1/n1)^2)/(n1-1)+((var2/n2)^2)/(n2-1);den
gl=num/den;gl#grados de libertad
t=abs(qt(a/2,gl));t
me=t*se;me
LI=dif_prom-me;LI  
LS=dif_prom+me;LS 



# Instalar y cargar el paquete
install.packages("tolerance")
library(tolerance)


datos <- rnorm(n, mean = media, sd = desvi)

intervalo <- normtol.int(x = datos, 
                         alpha = 0.05, 
                         P = 0.90,# nivel de tolerancia 
                         side = 2, # bilateral
                         method="EXACT" #factor exacto
)
print(intervalo)




X=c()

#Intervalo de Confianza

t.test(X,
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

datos <- rnorm(n, mean = promedio, sd = s)

intervalo <- normtol.int(x = datos, 
                         alpha = 0.01, 
                         P = 0.95,# nivel de tolerancia 
                         side = 2, # bilateral
                         method="EXACT" #factor exacto
)
print(intervalo)

#Intervalos para la media con bootstrapping

library(boot)

set.seed(10042026)

#calcular la media
boot_promedio=function(datos,indices){
  return(mean(datos[indices]))
}
#Bootstrap

boots=boot(X,
           statistic = boot_promedio,
           R=2000#réplicas
)
boot.ci(boots,
        type="perc",
        conf=0.99)


# --- Tamaño del efecto (d de Cohen) ---

library(effsize)
d_cohen=cohen.d(tiempo,#datos
                NA,
                mu =mu
)
d_cohen

# --- Potencia de la prueba ---
potencia=power.t.test(n = length(tiempo), 
                      delta = abs(mean(tiempo) - mu), 
                      sd = sd(tiempo), 
                      sig.level = alpha, 
                      type = "one.sample", 
                      alternative = "one.sided")
print(potencia)



#Prueba de hipótesis muestras pareadas

X=c()
Y=c()

t.test(x,#muestra 1
       y,#muestra 2
       alternative = "two.sided",#prueba a dos colas
       mu=0,
       paired=TRUE
)

alpha=
t=pt(alpha/2,4)


#Simulación de intervalos de confianza

#IC para la media  usando z
ic_m_C1=function(n,mu,sigma,n.sim,n.c){
  li=c()#Límite inferior del intervalo
  ls=c()#Límite superior de intervalo
  for(i in 1:n.sim){
    x=rnorm(n,mu,sigma)#simular datos de uina normal
    li[i]=mean(x)-qnorm((1-n.c)/2,lower.tail=FALSE)*(sigma/sqrt(n))
    ls[i]=mean(x)+qnorm((1-n.c)/2,lower.tail=FALSE)*(sigma/sqrt(n))
  }
  mean(ifelse(li<mu & mu<ls,1,0))
}
ic_m_C1(n=25,mu=20,sigma=3,n.sim=100000,n.c=0.95)
















#--------------
#Filtros y selecciones
#------------

Grupo1=base%>% 
  select(var_cuanti,variable_cuali) %>% 
  filter(variable_cuali=="caracteristica",variable_cuanti > numero))

