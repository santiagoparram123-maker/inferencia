
tiempo=c( 17, 18, 23, 19, 19, 24, 20, 15, 16, 17, 22, 20, 20, 19, 17, 
          24, 20, 13, 21, 17, 15, 18, 15, 16, 17, 13, 21, 19, 15, 22)
mu=20
alpha=0.05

t.test(tiempo, 
       mu = mu,
       alternative = "less")


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





#Bootstrap

# Muestras
set.seed(123)
grupo_A <- rnorm(15, mean = 10, sd = 2)
grupo_B <- rnorm(15, mean = 12, sd = 2)

# Prueba paramétrica (t-test)
p_parametrico <- t.test(grupo_A, grupo_B)$p.value
p_parametrico

# --- Procedimiento Bootstrap ---
media_global <- mean(c(grupo_A, grupo_B))
A_transf <- grupo_A - mean(grupo_A) + media_global
B_transf <- grupo_B - mean(grupo_B) + media_global

diferencia_obs <- mean(grupo_A) - mean(grupo_B)

B <- 1000
diferencias_boot <- replicate(B, {
  resample_A <- sample(A_transf, replace = TRUE)
  resample_B <- sample(B_transf, replace = TRUE)
  mean(resample_A) - mean(resample_B)
})

# p-valor bootstrap (proporción de casos más extremos que lo observado)
p_bootstrap <- mean(abs(diferencias_boot) >= abs(diferencia_obs))
p_bootstrap

cat("p-valor Paramétrico:", p_parametrico, "\n")
cat("p-valor Bootstrap:", p_bootstrap, "\n")
