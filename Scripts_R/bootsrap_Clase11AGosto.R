# ==============================================================================
# EXPERIMENTO BOOTSTRAP: B =100 y B=1000
# ==============================================================================

# 1. Parámetros iniciales
set.seed(123)
n <- 15 #tamaño de muestra
mu_teorica <- 50#media te´rica mu
sd_teorica <- 10# sigma

# 2. Generación de muestra y remuestreos
muestra_original <- rnorm(n, mean = mu_teorica, sd = sd_teorica)#el ejercicio dice que es normal
media_orig <- mean(muestra_original)
se_teorico <- sd_teorica / sqrt(n) #sigma/raiz(n)

medias_boot_100 <- replicate(100, #100 réplicas
                             mean(sample(muestra_original, 
                                         size = n, 
                                         replace = TRUE))#se haga reemplazo
                             )
medias_boot_1000 <- replicate(1000, #1000 réplicas
                              mean(sample(muestra_original, 
                                          size = n,
                                          replace = TRUE)))

# 3. Configurar panel (85% para gráficas, 15% para leyenda)
layout(matrix(c(1, 2, 3, 3), 
              nrow = 2, #dos filas
              ncol = 2, #dos columnas
              byrow = TRUE), #organice por filas 
       heights = c(1, 0.18))

# Márgenes para las gráficas superiores
par(mar = c(4.5, 4.5, 3.5, 1.5))

# ------------------------------------------------------------------------------
# GRÁFICA 1: B = 100
# ------------------------------------------------------------------------------
ylim_100 <- max(c(density(medias_boot_100)$y,
                  dnorm(media_orig,
                        mu_teorica, se_teorico))) * 1.1

hist(medias_boot_100, 
     probability = TRUE, 
     main = "Bootstrap con B = 100 Réplicas",
     xlab = "Media Muestral", 
     ylab = "Densidad",
     col = "lightblue", 
     border = "white",
     ylim = c(0, ylim_100),
     cex.main = 1.05,
     las = 1)

lines(density(medias_boot_100), col = "blue", lwd = 2)
curve(dnorm(x, mean = mu_teorica, sd = se_teorico), add = TRUE, col = "red", lwd = 2, lty = 2)
abline(v = media_orig, col = "darkgreen", lwd = 2, lty = 3)

# ------------------------------------------------------------------------------
# GRÁFICA 2: B = 1000
# ------------------------------------------------------------------------------
ylim_1000 <- max(c(density(medias_boot_1000)$y, dnorm(media_orig, mu_teorica, se_teorico))) * 1.1

hist(medias_boot_1000, 
     probability = TRUE, 
     main = "Bootstrap con B = 1000 Réplicas",
     xlab = "Media Muestral", 
     ylab = "Densidad",
     col = "lightblue", 
     border = "white",
     ylim = c(0, ylim_1000),
     cex.main = 1.05,
     las = 1)

lines(density(medias_boot_1000), col = "blue", lwd = 2)
curve(dnorm(x, mean = mu_teorica, sd = se_teorico), add = TRUE, col = "red", lwd = 2, lty = 2)
abline(v = media_orig, col = "darkgreen", lwd = 2, lty = 3)

# ------------------------------------------------------------------------------
# LEYENDA ÚNICA (CORREGIDA PARA QUE NO SE CORTE EN LOS BORDES)
# ------------------------------------------------------------------------------
par(mar = c(0, 1, 0, 1)) # Margen lateral para evitar que se corte a la izquierda
plot.new()              # Lienzo invisible para la leyenda

legend("center", 
       legend = c("Empírica Bootstrap (Kernel)", 
                  "Normal Teórica N(mu, SE)", 
                  "Media Muestral Observada"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(1, 2, 3), 
       lwd = 2,
       seg.len = 1.5,    # Hace la línea más corta para ganar espacio
       horiz = TRUE,    # Disposición horizontal
       bty = "n",
       cex = 0.75)      # Tamaño de letra ajustado

# Restaurar configuración gráfica predeterminada
par(mfrow = c(1, 1))
