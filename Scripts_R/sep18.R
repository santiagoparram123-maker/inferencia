# Función Monte Carlo para evaluar las propiedades de dos estimadores

evaluar_estimadores = function(n, mu, sigma, n.sim){
  #n tamaño de muestra
  #mu media poblacional
  #sigma desviación estándar poblacinal
  #n.sim cantidad de simulaciones
medias=numeric(n.sim) #promedios
medianas=numeric(n.sim)#mdianas
  
for(i in 1:n.sim) {
    # Generando muestra de tamaño n
    x=rnorm(n, mean=mu, sd = sigma)
    
    # Calculando los dos estimadores para la misma muestra
    medias[i]   = mean(x)
    medianas[i] = median(x)
  }
  
  # Métricas para evaluar comportamiento
  
  # Sesgo (Esperado: cercano a 0)
  sesgo_media   = mean(medias) - mu
  sesgo_mediana = mean(medianas) - mu
  
  # Varianza de los estimadores
  var_media   = var(medias)
  var_mediana = var(medianas)
  
  # Error Cuadrático Medio (ECM)
  ecm_media   = mean((medias - mu)^2)
  ecm_mediana = mean((medianas - mu)^2)
  
  # Consolidar resultados en una tabla
  resultados = data.frame(
    Estimador = c("Media Muestral", "Mediana Muestral"),
    Promedio_simulado = c(mean(medias), mean(medianas)),
    Sesgo = c(sesgo_media, sesgo_mediana),
    Varianza = c(var_media, var_mediana),
    ECM = c(ecm_media, ecm_mediana),
    check.names = FALSE
  )

  return(resultados)
}

# Ejecución: Muestra pequeña (n = 15), parámetro real mu = 10, sigma = 2
set.seed(123)
evaluar_estimadores(n = 15, mu = 10, sigma = 2, n.sim = 100000)



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

#IC para la media con varianza desconocida

ic_m_C2=function(n,mu,sigma,n.sim,n.c){
  li=c()#Límite inferior del intervalo
  ls=c()#Límite superior de intervalo
  for(i in 1:n.sim){
    x=rnorm(n,mu,sigma)#simular datos de uina normal
    s=sd(x) #Desviación estándar muestral
    li[i]=mean(x)-qt((1-n.c)/2,n-1,lower.tail=FALSE)*(s/sqrt(n))
    ls[i]=mean(x)+qt((1-n.c)/2,n-1,lower.tail=FALSE)*(s/sqrt(n))
  }
  mean(ifelse(li<mu & mu<ls,1,0))
}
ic_m_C2(n=25,mu=20,sigma=3,n.sim=10000,n.c=0.99)


#IC para la diferencia de medias con varianza desconocida
#varianzas iguales

