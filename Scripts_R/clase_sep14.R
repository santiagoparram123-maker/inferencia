

#
tiempos=c(52, 59, 54, 42, 44, 50, 42, 48,
          55, 54, 60, 55, 44, 62, 62, 57,
          45, 46, 43, 56)
promedio=mean(tiempos);promedio
desv=sd(tiempos);desv
n=length(tiempos);n
se=desv/sqrt(n);se
nc=0.99
alpha=1-nc
t=abs(qt(alpha/2,n-1));t

qt(0.005,29)
