
#LGN

# Cargar librerías
library(ggplot2)
library(gridExtra)

# Parámetros
set.seed(31072026)  # Semilla para que la gráfica sea exactamente reproducible
N <- 3000     # Número total de observaciones secuenciales

# ==========================================
# Convergencia de la Media Acumulada
# ==========================================
mu <- 100
sigma <- 15

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
# ==========================================
#  Convergencia de la Proporción Acumulada
# ==========================================
p <- 0.40

# Generar la secuencia de datos (1s y 0s)
datos_bernoulli <- rbinom(N, size = 1, prob = p)

# proporción acumulada 
prop_acumulada <- cumsum(datos_bernoulli) / (1:N)


df_prop <- data.frame(n = 1:N, Prop_Acum = prop_acumulada)

# Gráfico para la Proporción
g_prop_lln <- ggplot(df_prop, aes(x = n, y = Prop_Acum)) +
  geom_line(color = "seagreen", alpha = 0.8) +
  geom_hline(yintercept = p, color = "red", linetype = "dashed", linewidth = 1) +
  coord_cartesian(ylim = c(0.2, 0.6)) +
  labs(title = "Ley de los Grandes Números: Proporción",
       subtitle = expression("Convergencia hacia el valor esperado (" * p == 0.40 * ")"),
       x = "Número de observaciones (n)",
       y = "Proporción Acumulada") +
  theme_minimal()

# ==========================================
# Visualizar en conjunto
# ==========================================
grid.arrange( g_prop_lln, ncol = 1)






# ==========================================
#Animada
# ==========================================



#Con plotly


library(plotly)


grafico_interactivo <- ggplotly(g_media_lln)

# Mostrar el gráfico interactivo
grafico_interactivo
