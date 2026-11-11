
#PH diferencia de medias varianzas
#desconocidas pero iguales (continuación)
alpha=0.05
gl=16
t=qt(0.05,gl, lower.tail = FALSE);t

#PH diferencia de medias varianzas
#desconocidas pero diferentes

alpha=0.01
n1=28
n2=22
prom1=1025
prom2=910
desv1=150
desv2=125
num=(desv1^2/n1+desv2^2/n2)^2
den=((desv1^2/n1)^2/(n1-1))+((desv2^2/n2)^2/(n2-1))
gl=num/den;gl

t=qt(alpha/2,gl);t
tc=(prom1-prom2)/sqrt(desv1^2/n1+desv2^2/n2);tc

pvalor=pt(-abs(tc),gl)+pt(abs(tc),gl,lower.tail = FALSE);pvalor


#PH muestras pareadas

TA=c(10.6, 9.8, 12.3, 9.7, 8.8)
TB=c(10.2, 9.4, 11.8, 9.1, 8.3)
diferencia=TA-TB;diferencia

t.test(TA,
       TB,
       paired = TRUE)
alpha=0.05
n=length(TA)
gl=n-1
t=qt(alpha/2,gl);t
