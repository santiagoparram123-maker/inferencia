##########################################
#Distribución muestral para la media
##########################################


n=#Tamaño de muestra
mu=#media poblacional
sigma=#Desviación estándar poblacional
se=sigma/sqrt(n);se#error estándar

pnorm(6,mu,se)#P(x_barra<=6)
pnorm(6,mu,se,lower.tail=FALSE)#P(x_barra>=6)

alpha=#Valor del área o probabildad
x=qnorm(alpha,mu,sigma);x #P(X<=x)=alpha

##########################################
#Distribución muestral para la proporción
##########################################


n=#Tamaño de muestra
p=#proporción poblacional
se=sqrt(p*(1-p)/n);se#error estándar

pnorm(0.6,p,se,lower.tail = FALSE)#P(p^>=0.6)
pnorm(0.6,p,se)#P(p^<=0.6)

z=(0.6-p)/se;z#Estandarización con p^=0.6
pnorm(z,lower.tail = FALSE)#P(Z>=z)


##########################################
#Aprox binomial-normal
##########################################

n=#Tamaño de muestra
p=#probabilidad de éxito

pbinom(9,n,p)#P(X<=x)


mu=n*p;mu#media
sigma=sqrt(n*p*(1-p));sigma#desviación estándar

pnorm(9.5,mu,sigma)





##########################################
#Distribución chi cuadrado
##########################################


gl=# grados de libertad n-1
qchisq(0.05,gl,lower.tail = FALSE)#quantile x, P(X>=x)=0.05

datos=c()#ingresar como un vector separado por comas

n=length(datos);n#Tamaño de muestra
s2=var(datos);s2#VArianza muestral
varianza_pobla= #Varianza poblacional

chi.2=s2*(n-1)/varianza_pobla;chi.2#distribución de la varianza

pchisq(chi.2,n-1,lower.tail=FALSE)


##########################################
#LGN
##########################################


# Cargar librerías
library(ggplot2)
library(gridExtra)

# Parámetros
set.seed(31072026)  # Semilla para que la gráfica sea exactamente reproducible
N=3000     # Número total de observaciones secuenciales

# ==========================================
# Convergencia de la Media Acumulada
# ==========================================
mu=100
sigma=15

# Generar la secuencia de datos
datos_normales <- rnorm(N, mean = mu, sd = sigma)

# Calcular la media acumulada paso a paso
# cumsum(x) suma los valores hasta la posición i, y (1:N) divide por n
media_acumulada <- cumsum(datos_normales) / (1:N)

# Crear data frame para ggplot
df_media <- data.frame(n = 1:N, Media_Acum = media_acumulada)

# Gráfico para la Media
g_media_lln <- ggplot(df_media, aes(x = n, y = Media_Acum)) +
  geom_line(color = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = mu, color = "red", linetype = "dashed", linewidth = 1) +
  coord_cartesian(ylim = c(95, 105)) + # Ajustar el eje Y para ver mejor el detalle
  labs(title = "Ley de los Grandes Números: Media",
       subtitle = expression("Convergencia hacia el valor esperado (" * mu == 100 * ")"),
       x = "Número de observaciones (n)",
       y = "Media Acumulada") +
  theme_minimal()

grid.arrange(g_media_lln,ncol = 1)

# ==============================================================================
# EXPERIMENTO BOOTSTRAP: Cuando se asume una distribución normal
# ==============================================================================

# 1. Parámetros iniciales
set.seed(123) #Semilla
n=length(datos);n #tamaño de muestra
mu_teorica=#media teórica mu
sd_teorica=# sigma

# 2. Generación de muestra y remuestreos
muestra_original=rnorm(n, mean = mu_teorica, sd = sd_teorica)#el ejercicio dice que es normal
media_orig=mean(muestra_original);media_orig#Media original
se_teorico=sd_teorica/sqrt(n);se_teorico #sigma/raiz(n)

medias_boot_100=replicate(100, #100 réplicas
                             mean(sample(muestra_original, 
                                         size = n, 
                                         replace = TRUE))#se haga reemplazo
)

# ==============================================================================
# EXPERIMENTO BOOTSTRAP: Cuando NO se asume una distribución normal
# ==============================================================================


# 1. Parámetros iniciales
set.seed(1504) # Semilla:
datos = c()#ingreso de datos separados por comas
n = length(datos); n # Tamaño de la muestra

# 2. Cálculo Teórico 

# Se calcula la desviación estándar directamente de los datos,
sd_muestral = sd(datos);sd_muestral#Desviación estándar muestral

# Error estándar teórico
se_teorico = sd_muestral / sqrt(n); se_teorico 

# 3. Simulación Bootstrap 
# Se hace el remuestreo directamente sobre el vector 'datos', sin usar rnorm()
medias_boot_10000 = replicate(10000, # 10000 iteraciones solicitadas
                             mean(sample(datos, 
                                         size = n, 
                                         replace = TRUE))) # Con reemplazo

# Error estándar Bootstrap (desviación estándar de las medias resultantes)
se_boot = sd(medias_boot_10000); se_boot
