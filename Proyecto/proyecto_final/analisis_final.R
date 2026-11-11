# =============================================================================
#  PROYECTO INTEGRADOR - INFERENCIA ESTADISTICA
#  Entrega final: informe inferencial completo
#
#  Universidad Externado de Colombia
#  Departamento de Matematicas - Pregrado en Ciencia de Datos
#  Segundo semestre de 2026
#
#  Integrantes: Santiago Bernal, Santiago Parra, Daniel Nunez,
#               Andres Felipe Baquero
#
#  Base: "Evaluaciones Agropecuarias Municipales - EVA. 2019-2025.
#         Base Agricola" - UPRA, via Datos Abiertos Colombia (id uejq-wxrr).
#        Subconjunto de trabajo: registros del cultivo de maiz.
#
#  Software: R (>= 4.3). Funciones de R base y el paquete boot, que trae R
#            de serie. Solo se usan las pruebas y funciones vistas en clase:
#            var.test, t.test (Welch y pareada), prop.test, lm con summary,
#            confint y anova, shapiro.test, y boot con boot.ci (bootstrap
#            percentil). Cada estadistico se verifica ademas a mano con pt,
#            qt, pnorm, qnorm y pf.
#
#  Como reproducirlo
#  -----------------
#  1) Descargar la porcion de cereales de la base con la consulta que aparece
#     en DESCARGA_URL y guardarla como datos/eva_cereales.csv
#  2) Ubicar este script en la carpeta que contiene 'datos/'
#  3) Ejecutar:  Rscript analisis_final.R
#
#  Salidas: figuras/  (PDF vectorial para LaTeX y PNG a 300 ppp)
#           salidas/  (cifras para LaTeX y registro completo de resultados)
#
#  Las secciones 1 a 3 repiten sin cambios la depuracion de la Entrega 1, de
#  modo que la base analizada es exactamente la misma.
# =============================================================================

## ---------------------------------------------------------------------------
## 0. Configuracion general
## ---------------------------------------------------------------------------

rm(list = ls())
options(stringsAsFactors = FALSE, OutDec = ".", scipen = 999)
invisible(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

library(boot)         # bootstrap (boot y boot.ci), como en clase

ALFA    <- 0.05       # nivel de significancia de todos los contrastes
SEMILLA <- 20261117   # semilla de Shapiro-Wilk y del bootstrap (7.4 y 7.6)
B_BOOT  <- 2000       # remuestreos bootstrap (los mismos que en clase)

DESCARGA_URL <- paste0(
  "https://www.datos.gov.co/resource/uejq-wxrr.csv?",
  "$where=grupo_cultivo='Cereales'&$limit=100000")

RUTA_DATOS   <- file.path("datos", "eva_cereales.csv")
RUTA_FIGURAS <- "figuras"
RUTA_SALIDAS <- "salidas"
dir.create(RUTA_FIGURAS, showWarnings = FALSE)
dir.create(RUTA_SALIDAS, showWarnings = FALSE)

# Paleta en escala de grises (el informe se imprime en blanco y negro).
COL_RELLENO <- "#C8C8C8"
COL_BORDE   <- "#2B2B2B"
COL_G1      <- "#4D4D4D"   # tecnificado (oscuro)
COL_G2      <- "#B0B0B0"   # tradicional (claro)
COL_REJILLA <- "#DCDCDC"
COL_ACENTO  <- "#1A1A1A"
COL_PUNTO   <- grDevices::adjustcolor("#3D3D3D", alpha.f = 0.18)

PAR_BASE <- list(mar = c(4.2, 4.8, 1.6, 1.2), mgp = c(3.0, 0.7, 0), las = 1,
                 cex.lab = 1.02, cex.axis = 0.92, col.axis = COL_BORDE,
                 col.lab = COL_BORDE, fg = COL_BORDE, bty = "n", tcl = -0.3)

# Dibuja la misma figura en PDF (vectorial, para LaTeX) y en PNG a 300 ppp.
render <- function(archivo, dibujar, ancho = 6.6, alto = 4.3) {
  pdf(file.path(RUTA_FIGURAS, paste0(archivo, ".pdf")), width = ancho,
      height = alto, family = "Times", pointsize = 11,
      encoding = "ISOLatin1")
  do.call(par, PAR_BASE); dibujar(); dev.off()
  png(file.path(RUTA_FIGURAS, paste0(archivo, ".png")), width = ancho * 300,
      height = alto * 300, res = 300, family = "serif", type = "cairo")
  do.call(par, PAR_BASE); dibujar(); dev.off()
  invisible(NULL)
}

rejilla_h <- function() abline(h = axTicks(2), col = COL_REJILLA, lwd = 0.6)

sin_tildes <- function(x) {
  x <- toupper(trimws(as.character(x)))
  chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", x)
}

# Registro de resultados: toda cifra del informe queda escrita aqui.
LOG <- file.path(RUTA_SALIDAS, "resultados_inferenciales.txt")
cat("", file = LOG)
reg <- function(...) {
  linea <- paste0(...)
  cat(linea, "\n", sep = "", file = LOG, append = TRUE)
  cat(linea, "\n", sep = "")
}
regt <- function(objeto) {
  capture.output(print(objeto), file = LOG, append = TRUE)
  print(objeto)
}
regv <- function(etiqueta, valor) reg(sprintf("  %-54s %s", etiqueta, valor))

# Formatos con coma decimal (convencion del informe).
fmt  <- function(x, dec = 2) formatC(x, format = "f", digits = dec,
                                     decimal.mark = ",")
fmtm <- function(x, dec = 2) formatC(x, format = "f", digits = dec,
                                     decimal.mark = ",", big.mark = ".")
mil  <- function(x) formatC(round(x), format = "d", big.mark = ".",
                             decimal.mark = ",")
# Valor p para el registro de texto plano.
pv   <- function(p) {
  if (p < 2.2e-16) "< 2.2e-16"
  else if (p < 0.0001) format(p, digits = 3, scientific = TRUE)
  else formatC(p, format = "f", digits = 4)
}

## ---------------------------------------------------------------------------
## 1. Lectura de la base (identica a la Entrega 1)
## ---------------------------------------------------------------------------

reg("=========================================================================")
reg(" ENTREGA FINAL - SALIDA DEL ANALISIS INFERENCIAL")
reg(" Base: EVA Base Agricola (UPRA) - subconjunto de maiz")
reg(" Generado el ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
reg(" Nivel de significancia de todos los contrastes: alfa = ", ALFA)
reg("=========================================================================")
reg("")

if (!file.exists(RUTA_DATOS)) {
  stop("No se encuentra ", RUTA_DATOS, ".\nDescarguelo con la consulta:\n",
       DESCARGA_URL)
}

crudo <- read.csv(RUTA_DATOS, header = TRUE, sep = ",", dec = ".",
                  na.strings = c("", "NA", "NULL", "ND"), check.names = TRUE,
                  encoding = "UTF-8")

normalizar_nombre <- function(x) {
  x <- tolower(x)
  x <- chartr("áéíóúüñ", "aeiouun", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}
names(crudo) <- normalizar_nombre(names(crudo))

renombrar <- function(d, patron, nuevo) {
  i <- grep(patron, names(d))
  if (length(i) == 1) names(d)[i] <- nuevo
  d
}
crudo <- renombrar(crudo, "sembrada",                "area_sembrada")
crudo <- renombrar(crudo, "cosechada",               "area_cosechada")
crudo <- renombrar(crudo, "^produc",                 "produccion")
crudo <- renombrar(crudo, "^rendimiento",            "rendimiento")
crudo <- renombrar(crudo, "^(a_o|ano|anio|year)$",   "anio")
crudo <- renombrar(crudo, "^desagregaci",            "desagregacion")
crudo <- renombrar(crudo, "^ciclo",                  "ciclo")
crudo <- renombrar(crudo, "^estado",                 "estado_fisico")

faltan <- setdiff(c("cultivo", "desagregacion", "departamento", "municipio",
                    "area_sembrada", "area_cosechada", "produccion",
                    "rendimiento", "anio", "periodo"), names(crudo))
if (length(faltan) > 0) {
  stop("Faltan columnas en el CSV: ", paste(faltan, collapse = ", "),
       "\nColumnas encontradas: ", paste(names(crudo), collapse = ", "))
}

## ---------------------------------------------------------------------------
## 2. Depuracion (identica a la Entrega 1)
## ---------------------------------------------------------------------------

VARS_NUM <- c("area_sembrada", "area_cosechada", "produccion", "rendimiento")
VARS_CAT <- c("departamento", "municipio", "desagregacion", "ciclo", "periodo")

d  <- crudo
n0 <- nrow(d)

es_maiz <- sin_tildes(d$cultivo) == "MAIZ"
d  <- d[!is.na(es_maiz) & es_maiz, ]
n1 <- nrow(d)

for (v in VARS_NUM) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
for (v in VARS_CAT) d[[v]] <- trimws(as.character(d[[v]]))

desg <- sin_tildes(d$desagregacion)
d$sistema <- NA_character_
d$sistema[grepl("TRADICIONAL", desg)] <- "Tradicional"
d$sistema[grepl("TECNIFICADO", desg)] <- "Tecnificado"
d  <- d[!is.na(d$sistema), ]
n2 <- nrow(d)

d  <- d[!is.na(d$area_sembrada) & d$area_sembrada > 0 &
        !is.na(d$produccion)    & d$produccion    > 0 &
        !is.na(d$rendimiento)   & d$rendimiento   > 0, ]
n3 <- nrow(d)

d  <- d[complete.cases(d[, c(VARS_NUM, VARS_CAT)]), ]
n4 <- nrow(d)

reg("1. BASE ANALIZADA (misma depuracion de la Entrega 1)")
reg("---------------------------------------------------------------")
regv("Registros descargados (grupo Cereales)", n0)
regv("Registros de maiz", n1)
regv("Con sistema de produccion identificable", n2)
regv("Con area sembrada, produccion y rendimiento > 0", n3)
regv("Completos en las variables analizadas (base final)", n4)
if (n0 %in% c(30000, 50000, 100000)) {
  reg("  AVISO: el numero de registros descargados coincide con el limite ",
      "de la consulta; la descarga pudo quedar truncada.")
}
reg("")

if (n4 < 1000) stop("La base depurada no alcanza los 1000 registros exigidos.")

## ---------------------------------------------------------------------------
## 3. Recodificaciones de apoyo
## ---------------------------------------------------------------------------

d$sistema <- factor(d$sistema, levels = c("Tradicional", "Tecnificado"))

desg2   <- sin_tildes(d$desagregacion)
d$color <- ifelse(grepl("AMARILLO", desg2), "Amarillo",
           ifelse(grepl("BLANCO",   desg2), "Blanco", "Sin especificar"))
d$color <- factor(d$color, levels = c("Amarillo", "Blanco", "Sin especificar"))

# Perdida de area: el area cosechada es menor que la sembrada.
d$perdida <- d$area_cosechada < d$area_sembrada

# Identificador de municipio: el codigo DANE si la descarga lo trae (es
# estable aunque el nombre cambie de escritura entre anios); si no, el par
# departamento-municipio normalizado.
i_cod <- grep("dane_municipio|^cod.*mun|^c_d_mun", names(d))
if (length(i_cod) == 1) {
  d$mun_id <- formatC(as.integer(d[[i_cod]]), width = 5, flag = "0")
} else {
  d$mun_id <- paste(sin_tildes(d$departamento), sin_tildes(d$municipio),
                    sep = " / ")
}

N_MUN <- length(unique(d$mun_id))
N_DEP <- length(unique(sin_tildes(d$departamento)))

asimetria <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); m <- mean(x)
  (sum((x - m)^3) / n) / (sqrt(sum((x - m)^2) / n))^3
}

## ---------------------------------------------------------------------------
## 4. Resumen descriptivo de apoyo (retoma la Entrega 1)
## ---------------------------------------------------------------------------

trad <- d$rendimiento[d$sistema == "Tradicional"]
tec  <- d$rendimiento[d$sistema == "Tecnificado"]
n_trad <- length(trad); n_tec <- length(tec)

reg("2. RESUMEN DESCRIPTIVO DE APOYO")
reg("---------------------------------------------------------------")
desc_sis <- data.frame(
  sistema   = c("Tradicional", "Tecnificado"),
  n         = c(n_trad, n_tec),
  media     = c(mean(trad), mean(tec)),
  mediana   = c(median(trad), median(tec)),
  de        = c(sd(trad), sd(tec)),
  asimetria = c(asimetria(trad), asimetria(tec)))
regt(desc_sis)
regv("Municipios representados", N_MUN)
regv("Departamentos representados", N_DEP)
regv("Anios", paste(range(d$anio), collapse = " a "))
reg("")

## ---------------------------------------------------------------------------
## 5. PREGUNTA 1. Rendimiento medio: tecnificado frente a tradicional
##    Parametro: mu_tec - mu_trad (t/ha).
##    H0: mu_tec - mu_trad = 0    H1: mu_tec - mu_trad != 0   (bilateral)
## ---------------------------------------------------------------------------

reg("3. PREGUNTA 1 - RENDIMIENTO MEDIO POR SISTEMA")
reg("---------------------------------------------------------------")

## 5.1 Supuesto de igualdad de varianzas: prueba F (var.test)
##     H0: sigma2_tec / sigma2_trad = 1   H1: != 1
vt <- var.test(tec, trad, conf.level = 1 - ALFA)
F_vt  <- unname(vt$statistic)
gl1_vt <- unname(vt$parameter[1]); gl2_vt <- unname(vt$parameter[2])
# Verificacion manual
F_man <- var(tec) / var(trad)
p_man <- 2 * min(pf(F_man, n_tec - 1, n_trad - 1),
                 pf(F_man, n_tec - 1, n_trad - 1, lower.tail = FALSE))
stopifnot(isTRUE(all.equal(F_vt, F_man)), isTRUE(all.equal(vt$p.value, p_man)))
F_crit_inf <- qf(ALFA / 2, gl1_vt, gl2_vt)
F_crit_sup <- qf(1 - ALFA / 2, gl1_vt, gl2_vt)

reg("5.1 Prueba F de igualdad de varianzas (var.test)")
regv("Razon de varianzas s2_tec / s2_trad", fmt(F_vt, 3))
regv("Grados de libertad", paste(gl1_vt, "y", gl2_vt))
regv("Region de no rechazo", paste0("[", fmt(F_crit_inf, 3), "; ",
                                    fmt(F_crit_sup, 3), "]"))
regv("Valor p", pv(vt$p.value))
regv("IC 95% para la razon de varianzas",
     paste0("[", fmt(vt$conf.int[1], 3), "; ", fmt(vt$conf.int[2], 3), "]"))
regv("Decision", ifelse(vt$p.value < ALFA,
     "se rechaza H0: varianzas distintas -> t de Welch",
     "no se rechaza H0: varianzas comparables"))

## 5.2 Prueba t de Welch para muestras independientes
w <- t.test(tec, trad, var.equal = FALSE, conf.level = 1 - ALFA)
dif_w  <- mean(tec) - mean(trad)
ee_w   <- sqrt(var(tec) / n_tec + var(trad) / n_trad)
t_man  <- dif_w / ee_w
gl_man <- (var(tec) / n_tec + var(trad) / n_trad)^2 /
          ((var(tec) / n_tec)^2 / (n_tec - 1) +
           (var(trad) / n_trad)^2 / (n_trad - 1))
p_man  <- 2 * pt(-abs(t_man), gl_man)
stopifnot(isTRUE(all.equal(unname(w$statistic), t_man)),
          isTRUE(all.equal(unname(w$parameter), gl_man)),
          isTRUE(all.equal(w$p.value, p_man)))
t_crit_w <- qt(1 - ALFA / 2, gl_man)
ic_w <- dif_w + c(-1, 1) * t_crit_w * ee_w
stopifnot(isTRUE(all.equal(as.numeric(w$conf.int), ic_w)))

reg("")
reg("5.2 Prueba t de Welch (t.test, var.equal = FALSE)")
regv("Media tecnificado (t/ha)", fmt(mean(tec), 3))
regv("Media tradicional (t/ha)", fmt(mean(trad), 3))
regv("Diferencia estimada (t/ha)", fmt(dif_w, 3))
regv("Error estandar", fmt(ee_w, 4))
regv("Estadistico t", fmt(t_man, 3))
regv("Grados de libertad (Welch-Satterthwaite)", fmt(gl_man, 1))
regv("Valor critico t(1 - alfa/2)", fmt(t_crit_w, 3))
regv("Valor p", pv(w$p.value))
regv("IC 95% para la diferencia", paste0("[", fmt(ic_w[1], 3), "; ",
                                         fmt(ic_w[2], 3), "]"))
regv("Razon de medias tec / trad", fmt(mean(tec) / mean(trad), 3))

## 5.3 Complemento: comparacion pareada dentro del mismo municipio, anio,
##     periodo y color de grano. Cada par comparte lugar, temporada y tipo de
##     grano; lo unico que cambia es el sistema de produccion.
##     Parametro: mu_D, con D = rend_tec - rend_trad.
##     H0: mu_D = 0    H1: mu_D != 0
clave_par <- paste(d$mun_id, d$anio, d$periodo, d$color, sep = " | ")
conteo_clave <- table(clave_par, d$sistema)
n_claves_dup <- sum(conteo_clave > 1)
rend_clave <- tapply(d$rendimiento, list(clave_par, d$sistema), mean)
pares <- rend_clave[complete.cases(rend_clave), , drop = FALSE]
n_pares <- nrow(pares)
dif_par <- pares[, "Tecnificado"] - pares[, "Tradicional"]
mun_pares <- unique(sub(" \\| .*$", "", rownames(pares)))

tp <- t.test(pares[, "Tecnificado"], pares[, "Tradicional"], paired = TRUE,
             conf.level = 1 - ALFA)
ee_p  <- sd(dif_par) / sqrt(n_pares)
t_p   <- mean(dif_par) / ee_p
p_p   <- 2 * pt(-abs(t_p), n_pares - 1)
stopifnot(isTRUE(all.equal(unname(tp$statistic), t_p)),
          isTRUE(all.equal(tp$p.value, p_p)))
t_crit_p <- qt(1 - ALFA / 2, n_pares - 1)
prop_tec_mayor <- mean(dif_par > 0)

reg("")
reg("5.3 Comparacion pareada (t.test, paired = TRUE)")
regv("Pares municipio-anio-periodo-color con ambos sistemas", n_pares)
regv("Municipios distintos que aportan pares", length(mun_pares))
regv("Celdas clave-sistema con mas de un registro (promediadas)",
     n_claves_dup)
regv("Diferencia media D (t/ha)", fmt(mean(dif_par), 3))
regv("Desviacion estandar de D", fmt(sd(dif_par), 3))
regv("Estadistico t", fmt(t_p, 3))
regv("Grados de libertad", n_pares - 1)
regv("Valor critico t(1 - alfa/2)", fmt(t_crit_p, 3))
regv("Valor p", pv(tp$p.value))
regv("IC 95% para mu_D", paste0("[", fmt(tp$conf.int[1], 3), "; ",
                                fmt(tp$conf.int[2], 3), "]"))
regv("Pares en que el tecnificado rinde mas (%)",
     fmt(100 * prop_tec_mayor, 1))
regv("Asimetria de D", fmt(asimetria(dif_par), 3))

## 5.4 Sensibilidad a la dependencia entre registros: un promedio por
##     municipio y sistema (2019-2025) y prueba de Welch sobre esos promedios.
mun_sis <- aggregate(d$rendimiento,
                     by = list(mun_id = d$mun_id, sistema = d$sistema),
                     FUN = mean)
names(mun_sis)[3] <- "rend"
m_tec  <- mun_sis$rend[mun_sis$sistema == "Tecnificado"]
m_trad <- mun_sis$rend[mun_sis$sistema == "Tradicional"]
wm <- t.test(m_tec, m_trad, var.equal = FALSE, conf.level = 1 - ALFA)

reg("")
reg("5.4 Sensibilidad: un promedio por municipio y sistema (Welch)")
regv("Municipios con tecnificado / con tradicional",
     paste(length(m_tec), "/", length(m_trad)))
regv("Diferencia estimada (t/ha)", fmt(mean(m_tec) - mean(m_trad), 3))
regv("Estadistico t", fmt(unname(wm$statistic), 3))
regv("Grados de libertad", fmt(unname(wm$parameter), 1))
regv("Valor p", pv(wm$p.value))
regv("IC 95%", paste0("[", fmt(wm$conf.int[1], 3), "; ",
                      fmt(wm$conf.int[2], 3), "]"))
reg("")

## 5.5 Sensibilidad a los rendimientos extremos: se repite la prueba de Welch
##     sin el 0,1 % de rendimientos mas altos de la base.
corte_rend <- quantile(d$rendimiento, 0.999)
tec_r  <- tec[tec <= corte_rend]; trad_r <- trad[trad <= corte_rend]
wr <- t.test(tec_r, trad_r, var.equal = FALSE, conf.level = 1 - ALFA)
reg("5.5 Sensibilidad: sin el 0,1 % de rendimientos mas altos (> ",
    fmt(corte_rend, 2), " t/ha)")
regv("Registros excluidos", sum(d$rendimiento > corte_rend))
regv("Diferencia estimada (t/ha)", fmt(mean(tec_r) - mean(trad_r), 3))
regv("Estadistico t y valor p", paste0(fmt(unname(wr$statistic), 3), "; ",
                                       pv(wr$p.value)))
regv("IC 95%", paste0("[", fmt(wr$conf.int[1], 3), "; ",
                      fmt(wr$conf.int[2], 3), "]"))
reg("")

## ---------------------------------------------------------------------------
## 6. PREGUNTA 2. Proporcion de registros con perdida de area
##    Parametro: p_trad - p_tec.
##    H0: p_trad - p_tec = 0    H1: p_trad - p_tec != 0   (bilateral)
## ---------------------------------------------------------------------------

reg("4. PREGUNTA 2 - PROPORCION DE REGISTROS CON PERDIDA DE AREA")
reg("---------------------------------------------------------------")

x_trad <- sum(d$perdida[d$sistema == "Tradicional"])
x_tec  <- sum(d$perdida[d$sistema == "Tecnificado"])
p_trad <- x_trad / n_trad
p_tec  <- x_tec  / n_tec

# Condiciones para la aproximacion normal: al menos 5 exitos y 5 fracasos.
cond_np <- min(x_trad, n_trad - x_trad, x_tec, n_tec - x_tec)

pr <- prop.test(c(x_trad, x_tec), c(n_trad, n_tec), correct = FALSE,
                conf.level = 1 - ALFA)
p_comb <- (x_trad + x_tec) / (n_trad + n_tec)
z_man  <- (p_trad - p_tec) /
          sqrt(p_comb * (1 - p_comb) * (1 / n_trad + 1 / n_tec))
p_zman <- 2 * pnorm(-abs(z_man))
stopifnot(isTRUE(all.equal(unname(pr$statistic), z_man^2)),
          isTRUE(all.equal(pr$p.value, p_zman)))
z_crit <- qnorm(1 - ALFA / 2)
ee_np  <- sqrt(p_trad * (1 - p_trad) / n_trad + p_tec * (1 - p_tec) / n_tec)
ic_pr  <- (p_trad - p_tec) + c(-1, 1) * z_crit * ee_np
stopifnot(isTRUE(all.equal(as.numeric(pr$conf.int), ic_pr)))

# Intervalos individuales (para la figura)
ic_ind <- function(x, n) {
  p <- x / n; p + c(-1, 1) * z_crit * sqrt(p * (1 - p) / n)
}
ic_trad <- ic_ind(x_trad, n_trad)
ic_tec  <- ic_ind(x_tec,  n_tec)
razon_pp <- p_trad / p_tec

regv("Con perdida / total, tradicional", paste(x_trad, "/", n_trad))
regv("Con perdida / total, tecnificado", paste(x_tec, "/", n_tec))
regv("Proporcion tradicional", fmt(p_trad, 4))
regv("Proporcion tecnificado", fmt(p_tec, 4))
regv("Minimo de exitos o fracasos por grupo (>= 5)", cond_np)
regv("Proporcion combinada (bajo H0)", fmt(p_comb, 4))
regv("Estadistico Z (manual)", fmt(z_man, 3))
regv("X-squared de prop.test (= Z^2)", fmt(unname(pr$statistic), 3))
regv("Valor critico z(1 - alfa/2)", fmt(z_crit, 3))
regv("Valor p", pv(pr$p.value))
regv("Diferencia p_trad - p_tec (p.p.)", fmt(100 * (p_trad - p_tec), 2))
regv("IC 95% para la diferencia (p.p.)",
     paste0("[", fmt(100 * ic_pr[1], 2), "; ", fmt(100 * ic_pr[2], 2), "]"))
regv("Razon de proporciones trad / tec", fmt(razon_pp, 3))

## 6.2 Sensibilidad al desfase entre semestres. Una parte del area sembrada en
##     un semestre se cosecha en el siguiente: en algunos registros el area
##     cosechada SUPERA a la sembrada. Para que ese desfase pese menos, el
##     indicador se recalcula sumando los dos semestres de cada anio por
##     municipio y desagregacion, y se repite la prueba.
pct_cos_mayor <- mean(d$area_cosechada > d$area_sembrada)
clave_anual <- paste(d$mun_id, d$anio, d$desagregacion, sep = " | ")
sem_a <- tapply(d$area_sembrada,  clave_anual, sum)
cos_a <- tapply(d$area_cosechada, clave_anual, sum)
sis_a <- tapply(as.character(d$sistema), clave_anual, function(z) z[1])
perd_a <- cos_a < sem_a
xa_trad <- sum(perd_a[sis_a == "Tradicional"]); na_trad <- sum(sis_a == "Tradicional")
xa_tec  <- sum(perd_a[sis_a == "Tecnificado"]); na_tec  <- sum(sis_a == "Tecnificado")
pra <- prop.test(c(xa_trad, xa_tec), c(na_trad, na_tec), correct = FALSE,
                 conf.level = 1 - ALFA)
za  <- sign(xa_trad / na_trad - xa_tec / na_tec) * sqrt(unname(pra$statistic))
reg("")
reg("6.2 Sensibilidad: perdida calculada con el total anual")
regv("Registros con area cosechada mayor que la sembrada (%)",
     fmt(100 * pct_cos_mayor, 1))
regv("Totales anuales con perdida, tradicional",
     paste0(xa_trad, " / ", na_trad, " (", fmt(100 * xa_trad / na_trad, 1), " %)"))
regv("Totales anuales con perdida, tecnificado",
     paste0(xa_tec, " / ", na_tec, " (", fmt(100 * xa_tec / na_tec, 1), " %)"))
regv("Estadistico Z y valor p", paste0(fmt(za, 3), "; ", pv(pra$p.value)))
regv("IC 95% para la diferencia (p.p.)",
     paste0("[", fmt(100 * pra$conf.int[1], 2), "; ",
            fmt(100 * pra$conf.int[2], 2), "]"))
reg("")

## ---------------------------------------------------------------------------
## 7. PREGUNTA 3. Relacion entre area sembrada y produccion
##    Modelo planteado en la Entrega 1:  produccion = b0 + b1 area + e
##    Se ajusta con todos los registros y, por separado, dentro de cada
##    sistema de produccion: tres regresiones lineales simples.
##    En cada una: H0: b1 = 0 frente a H1: b1 != 0 (prueba t) y prueba F
##    global; tambien H0: b0 = 0 (prueba t sobre el intercepto).
##    Como los supuestos de los errores no se cumplen, el IC de la pendiente
##    se recalcula con bootstrap percentil (boot y boot.ci, como en clase).
## ---------------------------------------------------------------------------

reg("5. PREGUNTA 3 - REGRESION LINEAL SIMPLE")
reg("---------------------------------------------------------------")

resumen_modelo <- function(m, etiqueta) {
  s   <- summary(m)
  cf  <- s$coefficients
  ic  <- confint(m, level = 1 - ALFA)
  Fv  <- s$fstatistic
  pF  <- pf(Fv[1], Fv[2], Fv[3], lower.tail = FALSE)
  gl  <- m$df.residual
  # Verificacion manual de la prueba t sobre la pendiente, de su valor p y
  # de la F global (en la regresion simple, F = t^2)
  t_b1 <- cf[2, 1] / cf[2, 2]
  p_b1 <- 2 * pt(-abs(t_b1), gl)
  stopifnot(isTRUE(all.equal(t_b1, cf[2, 3])),
            isTRUE(all.equal(p_b1, cf[2, 4])),
            isTRUE(all.equal(unname(Fv[1]), t_b1^2)))
  ic_man <- cf[2, 1] + c(-1, 1) * qt(1 - ALFA / 2, gl) * cf[2, 2]
  stopifnot(isTRUE(all.equal(unname(ic[2, ]), ic_man)))
  av <- anova(m)
  stopifnot(isTRUE(all.equal(av$`F value`[1], unname(Fv[1]))))
  reg(etiqueta)
  regt(round(cf, 6))
  regv("IC 95% intercepto", paste0("[", fmt(ic[1, 1], 4), "; ",
                                   fmt(ic[1, 2], 4), "]"))
  regv("IC 95% pendiente",  paste0("[", fmt(ic[2, 1], 4), "; ",
                                   fmt(ic[2, 2], 4), "]"))
  regv("Valor critico t(1 - alfa/2)", fmt(qt(1 - ALFA / 2, gl), 3))
  regv("R2", fmt(s$r.squared, 4))
  regv("R2 ajustado", fmt(s$adj.r.squared, 4))
  regv("Error estandar residual", fmt(s$sigma, 4))
  regv("F global", paste0(fmt(Fv[1], 2), " con ", Fv[2], " y ", Fv[3],
                          " gl; valor p ", pv(pF)))
  regv("Valor critico F(1 - alfa)", fmt(qf(1 - ALFA, Fv[2], Fv[3]), 3))
  list(s = s, cf = cf, ic = ic, F = unname(Fv[1]), gl1 = unname(Fv[2]),
       gl2 = unname(Fv[3]), pF = unname(pF), gl = gl,
       r2 = s$r.squared, r2a = s$adj.r.squared, sigma = s$sigma)
}

d_trad <- d[d$sistema == "Tradicional", ]
d_tec  <- d[d$sistema == "Tecnificado", ]

mA <- lm(produccion ~ area_sembrada, data = d)
rA <- resumen_modelo(mA, "7.1 Modelo global: produccion ~ area_sembrada (todos los registros)")
regv("Covarianza entre area y produccion",
     fmt(cov(d$area_sembrada, d$produccion), 1))
regv("Correlacion de Pearson entre area y produccion",
     fmt(cor(d$area_sembrada, d$produccion), 3))
reg("")
mT <- lm(produccion ~ area_sembrada, data = d_trad)
rT <- resumen_modelo(mT, "7.2 Modelo dentro del sistema tradicional")
reg("")
mK <- lm(produccion ~ area_sembrada, data = d_tec)
rK <- resumen_modelo(mK, "7.3 Modelo dentro del sistema tecnificado")
reg("")

## 7.4 Diagnostico de residuos, con las herramientas vistas en clase
##   Linealidad y varianza constante: grafico de residuos estandarizados
##   frente a valores ajustados (Figura 5a). Como apoyo descriptivo, sin
##   prueba formal, se compara la desviacion estandar de los residuos en el
##   tercio de valores ajustados mas bajos y en el tercio mas alto.
##   Normalidad: grafico cuantil-cuantil (Figura 5b) y prueba de Shapiro-Wilk.
##   shapiro.test admite como maximo 5000 datos, de modo que se aplica a una
##   muestra aleatoria de 5000 residuos (o a todos, si el modelo tiene menos).
##   Independencia: se discute a partir del diseno de los datos (un mismo
##   municipio aporta registros de varios semestres); no se contrasta.
diag_modelo <- function(m) {
  e <- resid(m); f <- fitted(m)
  q <- quantile(f, c(1 / 3, 2 / 3))
  de_bajo <- sd(e[f <= q[1]]); de_alto <- sd(e[f > q[2]])
  n_sw <- min(5000, length(e))
  set.seed(SEMILLA)
  error <- if (length(e) > 5000) sample(e, n_sw) else e
  sw <- shapiro.test(error)
  list(de_bajo = de_bajo, de_alto = de_alto, razon = de_alto / de_bajo,
       W = unname(sw$statistic), p = sw$p.value, n_sw = n_sw)
}
dA <- diag_modelo(mA); dT <- diag_modelo(mT); dK <- diag_modelo(mK)

reg("7.4 Diagnostico de residuos")
for (x in list(list("Modelo global", dA), list("Modelo tradicional", dT),
               list("Modelo tecnificado", dK))) {
  reg("  ", x[[1]])
  regv("  DE de residuos, tercio de ajustados bajos / altos",
       paste0(fmt(x[[2]]$de_bajo, 1), " / ", fmt(x[[2]]$de_alto, 1),
              "  (razon ", fmt(x[[2]]$razon, 1), ")"))
  regv("  Shapiro-Wilk (H0: los errores son normales)",
       paste0("W = ", fmt(x[[2]]$W, 4), "; valor p ", pv(x[[2]]$p),
              "; n = ", x[[2]]$n_sw))
}
reg("")

## 7.5 Observaciones poco comunes: el modelo global sin el 1 % de areas mas
##     grandes, y el peso del sistema tecnificado dentro de ese 1 %.
corte99 <- quantile(d$area_sembrada, 0.99)
grandes <- d$area_sembrada > corte99
mA99 <- lm(produccion ~ area_sembrada, data = d[!grandes, ])
pct_tec_grandes <- mean(d$sistema[grandes] == "Tecnificado")
reg("7.5 Modelo global sin el 1 % de areas mas grandes (> ",
    fmt(corte99, 1), " ha)")
regv("Registros excluidos", sum(grandes))
regv("Pendiente", fmt(coef(mA99)[2], 4))
regv("R2", fmt(summary(mA99)$r.squared, 4))
regv("Tecnificados dentro del 1 % de areas mas grandes (%)",
     fmt(100 * pct_tec_grandes, 1))
regv("Tecnificados en toda la base (%)", fmt(100 * n_tec / n4, 1))
reg("")

## 7.6 Intervalo bootstrap percentil para la pendiente (como en clase: boot y
##     boot.ci con type = "perc"). Se remuestrean registros completos (area y
##     produccion juntos) y en cada remuestreo se vuelve a ajustar la recta.
##     El intervalo no exige normalidad ni varianza constante de los errores.
boot_pendiente <- function(datos, indices) {
  coef(lm(produccion ~ area_sembrada, data = datos[indices, ]))[2]
}
boot_modelo <- function(datos, r) {
  set.seed(SEMILLA)
  boots <- boot(datos[, c("produccion", "area_sembrada")],
                statistic = boot_pendiente,
                R = B_BOOT)
  ic <- boot.ci(boots, type = "perc", conf = 1 - ALFA)$percent[4:5]
  list(pend = boots$t[, 1], li = ic[1], ls = ic[2], se = sd(boots$t[, 1]),
       sesgo = mean(boots$t[, 1]) - boots$t0,
       razon_ancho = (ic[2] - ic[1]) / (r$ic[2, 2] - r$ic[2, 1]))
}
bA <- boot_modelo(d, rA)
bT <- boot_modelo(d_trad, rT)
bK <- boot_modelo(d_tec, rK)

reg("7.6 IC bootstrap percentil para la pendiente (", B_BOOT, " remuestreos)")
for (x in list(list("Modelo global", rA, bA), list("Modelo tradicional", rT, bT),
               list("Modelo tecnificado", rK, bK))) {
  reg("  ", x[[1]])
  regv("  Pendiente estimada", fmt(x[[2]]$cf[2, 1], 4))
  regv("  IC 95% clasico", paste0("[", fmt(x[[2]]$ic[2, 1], 4), "; ",
                                  fmt(x[[2]]$ic[2, 2], 4), "]"))
  regv("  IC 95% bootstrap percentil", paste0("[", fmt(x[[3]]$li, 4), "; ",
                                              fmt(x[[3]]$ls, 4), "]"))
  regv("  Error estandar clasico / bootstrap",
       paste0(fmt(x[[2]]$cf[2, 2], 4), " / ", fmt(x[[3]]$se, 4)))
  regv("  Sesgo bootstrap", fmt(x[[3]]$sesgo, 4))
  regv("  Ancho del IC bootstrap / ancho del IC clasico",
       fmt(x[[3]]$razon_ancho, 1))
}
reg("")

## ---------------------------------------------------------------------------
## 8. Figuras
## ---------------------------------------------------------------------------

## FIGURA 1 - Rendimiento por sistema (distribucion y media de cada grupo)
render("fig1_box_sistema", function() {
  bp <- boxplot(rendimiento ~ sistema, data = d, plot = FALSE)
  tope <- as.numeric(quantile(d$rendimiento, 0.99))
  plot(NA, xlim = c(0.5, 2.7), ylim = c(0, tope * 1.05), xaxt = "n",
       yaxt = "n", xlab = "Sistema de producción",
       ylab = "Rendimiento (toneladas por hectárea)")
  ticks <- pretty(c(0, tope), 6)
  axis(2, at = ticks, col = COL_BORDE)
  abline(h = ticks, col = COL_REJILLA, lwd = 0.6)
  boxplot(rendimiento ~ sistema, data = d, add = TRUE, xaxt = "n", yaxt = "n",
          col = c(COL_G2, COL_G1), border = COL_BORDE, outline = FALSE,
          boxwex = 0.45, medlwd = 2.4, staplewex = 0.35, whisklty = 1)
  medias <- c(mean(trad), mean(tec))
  points(1:2, medias, pch = 23, bg = "white", col = COL_ACENTO, cex = 1.3,
         lwd = 1.4)
  axis(1, at = 1:2, col = COL_BORDE, tick = FALSE,
       labels = paste0(levels(d$sistema), "\n(n = ", mil(bp$n), ")"),
       padj = 0.4)
  # Rotulos de media y mediana; si quedan muy cerca se separan.
  sep <- 0.05 * tope
  for (i in 1:2) {
    ym <- medias[i]; yd <- bp$stats[3, i]
    if (abs(ym - yd) < sep) {
      centro <- (ym + yd) / 2
      ym <- centro + ifelse(medias[i] >= bp$stats[3, i], 1, -1) * sep / 2
      yd <- centro - ifelse(medias[i] >= bp$stats[3, i], 1, -1) * sep / 2
    }
    text(i + 0.27, yd, paste0("Mna. ", fmt(bp$stats[3, i], 2)),
         cex = 0.85, adj = 0)
    text(i + 0.27, ym, paste0("Media ", fmt(medias[i], 2)),
         cex = 0.85, adj = 0, font = 2)
  }
  box(bty = "l", col = COL_BORDE)
}, 6.0, 4.3)

## FIGURA 2 - Estimaciones de la brecha de rendimiento con su IC del 95 %
est_br <- c(dif_w, mean(dif_par), mean(m_tec) - mean(m_trad))
li_br  <- c(ic_w[1], tp$conf.int[1], wm$conf.int[1])
ls_br  <- c(ic_w[2], tp$conf.int[2], wm$conf.int[2])
etq_br <- c("Todos los registros\n(t de Welch)",
            "Mismo municipio, periodo\ny color (t pareada)",
            "Un promedio por\nmunicipio (t de Welch)")

render("fig2_ic_brecha", function() {
  par(mar = c(4.4, 11.2, 1.2, 1.2))
  xr <- range(0, li_br, ls_br)
  xr <- c(min(0, xr[1]) - 0.04 * diff(xr), xr[2] + 0.42 * diff(xr))
  y <- 3:1
  plot(NA, xlim = xr, ylim = c(0.5, 3.5), yaxt = "n",
       xlab = "Tecnificado menos tradicional (toneladas por hectárea)",
       ylab = "")
  abline(v = axTicks(1), col = COL_REJILLA, lwd = 0.6)
  abline(v = 0, col = COL_ACENTO, lty = 2, lwd = 1.2)
  segments(li_br, y, ls_br, y, lwd = 2.4, col = COL_BORDE)
  segments(li_br, y - 0.09, li_br, y + 0.09, lwd = 1.6, col = COL_BORDE)
  segments(ls_br, y - 0.09, ls_br, y + 0.09, lwd = 1.6, col = COL_BORDE)
  points(est_br, y, pch = 21, bg = COL_G1, col = COL_ACENTO, cex = 1.5)
  axis(2, at = y, labels = etq_br, tick = FALSE, las = 1, cex.axis = 0.88)
  text(ls_br, y, paste0("   ", fmt(est_br, 2), "  [", fmt(li_br, 2), "; ",
                        fmt(ls_br, 2), "]"), adj = 0, cex = 0.85)
  box(bty = "l", col = COL_BORDE)
}, 6.6, 3.4)

## FIGURA 3 - Proporcion de registros con perdida de area, con IC del 95 %
render("fig3_ic_perdida", function() {
  pr_g <- 100 * c(p_trad, p_tec)
  li_g <- 100 * c(ic_trad[1], ic_tec[1])
  ls_g <- 100 * c(ic_trad[2], ic_tec[2])
  ymax <- max(ls_g) * 1.28
  plot(NA, xlim = c(0.4, 2.6), ylim = c(0, ymax), xaxt = "n",
       xlab = "Sistema de producción",
       ylab = "Registros con pérdida de área (%)")
  rejilla_h()
  rect(1:2 - 0.27, 0, 1:2 + 0.27, pr_g, col = c(COL_G2, COL_G1),
       border = NA)
  arrows(1:2, li_g, 1:2, ls_g, angle = 90, code = 3, length = 0.07,
         lwd = 1.8, col = COL_ACENTO)
  text(1:2, ls_g + ymax * 0.06,
       paste0(fmt(pr_g, 1), " %  [", fmt(li_g, 1), "; ", fmt(ls_g, 1), "]"),
       cex = 0.86, font = 2)
  axis(1, at = 1:2, tick = FALSE, padj = 0.4,
       labels = paste0(c("Tradicional", "Tecnificado"), "\n(n = ",
                       mil(c(n_trad, n_tec)), ")"))
  box(bty = "l", col = COL_BORDE)
}, 5.8, 4.2)

## FIGURA 4 - Dispersion del area sembrada y la produccion, con las rectas
##            ajustadas con todos los registros y dentro de cada sistema
eje_miles <- function(lado) {
  tx <- axTicks(lado)
  axis(lado, at = tx, col = COL_BORDE,
       labels = formatC(tx, format = "d", big.mark = "."))
}
COL_PT_TRAD <- grDevices::adjustcolor("#9A9A9A", alpha.f = 0.45)
COL_PT_TEC  <- grDevices::adjustcolor("#1F1F1F", alpha.f = 0.55)

render("fig4_dispersion", function() {
  par(mfrow = c(1, 2), mar = c(4.2, 5.2, 2.2, 0.8), cex.main = 0.98,
      font.main = 2)
  paneles <- list(list(d, "(a) Todos los registros"),
                  list(d[!grandes, ], "(b) Sin el 1 % de áreas mayores"))
  for (k in seq_along(paneles)) {
    s <- paneles[[k]][[1]]; es_tec <- s$sistema == "Tecnificado"
    plot(s$area_sembrada, s$produccion, type = "n", xaxt = "n", yaxt = "n",
         xlab = "Área sembrada (hectáreas)", ylab = "",
         main = paneles[[k]][[2]])
    title(ylab = "Producción (toneladas)", line = 4.0)
    eje_miles(1); eje_miles(2)
    points(s$area_sembrada[!es_tec], s$produccion[!es_tec], pch = 16,
           cex = 0.35, col = COL_PT_TRAD)
    points(s$area_sembrada[es_tec], s$produccion[es_tec], pch = 17,
           cex = 0.40, col = COL_PT_TEC)
    abline(coef(mA), col = COL_ACENTO, lwd = 2.2)
    abline(coef(mK), col = COL_ACENTO, lwd = 1.4, lty = 2)
    abline(coef(mT), col = COL_ACENTO, lwd = 1.4, lty = 3)
    if (k == 1) {
      legend("topleft", bty = "n", cex = 0.72,
             legend = c("Registro tradicional", "Registro tecnificado",
                        "Recta con todos los registros",
                        "Recta del tecnificado", "Recta del tradicional"),
             pch = c(16, 17, NA, NA, NA), lty = c(NA, NA, 1, 2, 3),
             lwd = c(NA, NA, 2.2, 1.4, 1.4),
             col = c("#8A8A8A", "#1F1F1F", COL_ACENTO, COL_ACENTO, COL_ACENTO))
    }
    box(bty = "l", col = COL_BORDE)
  }
}, 7.0, 3.9)

## FIGURA 5 - Diagnostico de residuos del modelo con todos los registros
qq_ralo <- function(e, ...) {
  n <- length(e); es <- sort(e); pp <- ppoints(n)
  idx <- unique(c(1:min(60, n), round(seq(1, n, length.out = 1500)),
                  max(1, n - 59):n))
  plot(qnorm(pp[idx]), es[idx], pch = 16, cex = 0.35, col = COL_G1,
       xlab = "Cuantiles teóricos normales",
       ylab = "Residuos estandarizados", ...)
  abline(0, 1, col = COL_ACENTO, lwd = 1.3, lty = 2)
}

render("fig5_residuos", function() {
  par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.2, 0.8), cex.main = 0.98,
      font.main = 2)
  f <- fitted(mA); e <- rstandard(mA)
  # Para que el eje sea legible se muestra el 99 % de valores ajustados mas
  # bajos y los residuos entre los percentiles 0,05 y 99,95 (nota de la figura).
  lim_x <- c(min(f), quantile(f, 0.99))
  lim_y <- quantile(e, c(0.0005, 0.9995))
  plot(f, e, pch = 16, cex = 0.3, col = COL_PUNTO, xaxt = "n",
       xlim = lim_x, ylim = range(lim_y, -4, 4),
       xlab = "Valores ajustados (toneladas)",
       ylab = "Residuos estandarizados",
       main = "(a) Residuos frente a ajustados")
  eje_miles(1)
  abline(h = 0, col = COL_ACENTO, lwd = 1.2, lty = 2)
  qq_ralo(e, main = "(b) Gráfico cuantil-cuantil normal")
}, 7.0, 3.6)

## FIGURA 6 - Pendiente de cada modelo con su IC bootstrap y su IC clasico,
##            junto al rendimiento medio de cada grupo (pregunta 1)
render("fig6_pendientes", function() {
  par(mar = c(4.4, 7.4, 1.2, 1.0))
  est  <- c(rA$cf[2, 1], rT$cf[2, 1], rK$cf[2, 1])
  li_c <- c(rA$ic[2, 1], rT$ic[2, 1], rK$ic[2, 1])
  ls_c <- c(rA$ic[2, 2], rT$ic[2, 2], rK$ic[2, 2])
  li_b <- c(bA$li, bT$li, bK$li)
  ls_b <- c(bA$ls, bT$ls, bK$ls)
  rend <- c(mean(d$rendimiento), mean(trad), mean(tec))
  y <- 3:1
  xr <- c(0, max(ls_b, rend) * 1.45)
  plot(NA, xlim = xr, ylim = c(0.5, 4.25), yaxt = "n",
       xlab = "Toneladas de producción por hectárea sembrada", ylab = "")
  abline(v = axTicks(1), col = COL_REJILLA, lwd = 0.6)
  # IC bootstrap: linea delgada con topes
  segments(li_b, y, ls_b, y, lwd = 1.6, col = COL_BORDE)
  segments(li_b, y - 0.09, li_b, y + 0.09, lwd = 1.6, col = COL_BORDE)
  segments(ls_b, y - 0.09, ls_b, y + 0.09, lwd = 1.6, col = COL_BORDE)
  # IC clasico: barra gruesa, debajo de la linea
  segments(li_c, y - 0.22, ls_c, y - 0.22, lwd = 5, lend = 1,
           col = COL_ACENTO)
  points(est, y, pch = 21, bg = COL_G1, col = COL_ACENTO, cex = 1.4)
  points(rend, y + 0.24, pch = 23, bg = "white", col = COL_ACENTO, cex = 1.1,
         lwd = 1.2)
  axis(2, at = y, tick = FALSE, las = 1, cex.axis = 0.9,
       labels = c("Todos los\nregistros", "Tradicional", "Tecnificado"))
  text(ls_b, y, paste0("  ", fmt(est, 2), "  [", fmt(li_b, 2), "; ",
                       fmt(ls_b, 2), "]"), adj = 0, cex = 0.82)
  legend("top", bty = "n", cex = 0.74, ncol = 2, inset = 0.01,
         legend = c("Pendiente estimada", "IC 95 % bootstrap",
                    "IC 95 % clásico", "Rendimiento medio (pregunta 1)"),
         pch = c(21, NA, NA, 23), pt.bg = c(COL_G1, NA, NA, "white"),
         lty = c(NA, 1, 1, NA), lwd = c(NA, 1.6, 5, NA),
         col = COL_ACENTO)
  box(bty = "l", col = COL_BORDE)
}, 6.6, 3.7)

## ---------------------------------------------------------------------------
## 9. Cifras exportadas a LaTeX
##    El informe no escribe ningun numero a mano: cada cifra del texto y de
##    las tablas se inserta con la macro correspondiente.
## ---------------------------------------------------------------------------

# En LaTeX: el menos y los signos de relacion van con \ensuremath para que
# funcionen dentro y fuera de modo matematico, y la coma decimal va entre
# llaves, {,}, para que en modo matematico no se agregue un espacio tras ella.
tex_signo <- function(x) gsub("(^|\\s)-", "\\1\\\\ensuremath{-}", as.character(x))
macros <- character(0)
mac <- function(nombre, valor) {
  stopifnot(grepl("^[A-Za-z]+$", nombre), length(valor) == 1)
  valor <- gsub("([0-9]),([0-9])", "\\1{,}\\2", tex_signo(valor))
  macros <<- c(macros, paste0("\\newcommand{\\", nombre, "}{", valor, "}"))
}
fp     <- function(p) if (p < 0.001) "\\ensuremath{<}\\,0,001" else fmt(p, 3)
fp_rel <- function(p) if (p < 0.001) "\\ensuremath{<}~0,001" else
  paste0("\\ensuremath{=}~", fmt(p, 3))
num    <- function(x, dec = 2) fmtm(x, dec)
decide <- function(p) if (p < ALFA) "Se rechaza" else "No se rechaza"

# Base
mac("Ndescargados", mil(n0)); mac("Nmaiz", mil(n1)); mac("Nfinal", mil(n4))
mac("Nmunicipios", mil(N_MUN)); mac("Ndepartamentos", mil(N_DEP))
mac("AnioMin", min(d$anio)); mac("AnioMax", max(d$anio))
mac("NTrad", mil(n_trad)); mac("NTec", mil(n_tec))
mac("PctTec", fmt(100 * n_tec / n4, 1))
mac("Alfa", fmt(ALFA, 2)); mac("NivelConf", fmt(100 * (1 - ALFA), 0))

# Pregunta 1: descriptivos
mac("MediaTrad", fmt(mean(trad), 2)); mac("MediaTec", fmt(mean(tec), 2))
mac("MedianaTrad", fmt(median(trad), 2)); mac("MedianaTec", fmt(median(tec), 2))
mac("DETrad", fmt(sd(trad), 2)); mac("DETec", fmt(sd(tec), 2))
mac("AsimTrad", fmt(asimetria(trad), 2)); mac("AsimTec", fmt(asimetria(tec), 2))
mac("QITrad", fmt(quantile(trad, 0.25), 2)); mac("QIIITrad", fmt(quantile(trad, 0.75), 2))
mac("QITec", fmt(quantile(tec, 0.25), 2)); mac("QIIITec", fmt(quantile(tec, 0.75), 2))
mac("RazonMedias", fmt(mean(tec) / mean(trad), 2))
# Prueba F de varianzas
mac("VTF", fmt(F_vt, 2)); mac("VTglUno", mil(gl1_vt)); mac("VTglDos", mil(gl2_vt))
mac("VTp", fp(vt$p.value)); mac("VTpRel", fp_rel(vt$p.value))
mac("VTcritInf", fmt(F_crit_inf, 3)); mac("VTcritSup", fmt(F_crit_sup, 3))
mac("VTicInf", fmt(vt$conf.int[1], 2)); mac("VTicSup", fmt(vt$conf.int[2], 2))
# Welch
mac("WDif", fmt(dif_w, 2)); mac("WEE", fmt(ee_w, 3))
mac("Wt", fmt(t_man, 2)); mac("Wgl", fmtm(gl_man, 1))
mac("Wcrit", fmt(t_crit_w, 3)); mac("Wp", fp(w$p.value)); mac("WpRel", fp_rel(w$p.value))
mac("WicInf", fmt(ic_w[1], 2)); mac("WicSup", fmt(ic_w[2], 2))
# Pareada
mac("NPares", mil(n_pares)); mac("NMunPares", mil(length(mun_pares)))
mac("PDif", fmt(mean(dif_par), 2)); mac("PDE", fmt(sd(dif_par), 2))
mac("Pt", fmt(t_p, 2)); mac("Pgl", mil(n_pares - 1))
mac("Pcrit", fmt(t_crit_p, 3)); mac("Pp", fp(tp$p.value)); mac("PpRel", fp_rel(tp$p.value))
mac("PicInf", fmt(tp$conf.int[1], 2)); mac("PicSup", fmt(tp$conf.int[2], 2))
mac("PPctTecMayor", fmt(100 * prop_tec_mayor, 1))
# Sensibilidad municipal
mac("MNTec", mil(length(m_tec))); mac("MNTrad", mil(length(m_trad)))
mac("MDif", fmt(mean(m_tec) - mean(m_trad), 2))
mac("Mt", fmt(unname(wm$statistic), 2)); mac("Mgl", fmtm(unname(wm$parameter), 1))
mac("Mcrit", fmt(qt(1 - ALFA / 2, unname(wm$parameter)), 3))
mac("Mp", fp(wm$p.value)); mac("MpRel", fp_rel(wm$p.value))
mac("MicInf", fmt(wm$conf.int[1], 2)); mac("MicSup", fmt(wm$conf.int[2], 2))
mac("CorteRend", fmt(corte_rend, 1))
mac("NExcluRend", mil(sum(d$rendimiento > corte_rend)))
mac("RDif", fmt(mean(tec_r) - mean(trad_r), 2))
mac("RicInf", fmt(wr$conf.int[1], 2)); mac("RicSup", fmt(wr$conf.int[2], 2))

# Pregunta 2
mac("XTrad", mil(x_trad)); mac("XTec", mil(x_tec))
mac("PropTrad", fmt(100 * p_trad, 1)); mac("PropTec", fmt(100 * p_tec, 1))
mac("PropComb", fmt(100 * p_comb, 1))
mac("ICTradInf", fmt(100 * ic_trad[1], 1)); mac("ICTradSup", fmt(100 * ic_trad[2], 1))
mac("ICTecInf", fmt(100 * ic_tec[1], 1)); mac("ICTecSup", fmt(100 * ic_tec[2], 1))
mac("CondNp", mil(cond_np))
mac("Zp", fmt(z_man, 2)); mac("ZChi", fmt(unname(pr$statistic), 2))
mac("Zcrit", fmt(z_crit, 2)); mac("ZpVal", fp(pr$p.value)); mac("ZpRel", fp_rel(pr$p.value))
mac("DifProp", fmt(100 * (p_trad - p_tec), 1))
mac("DifPropInf", fmt(100 * ic_pr[1], 1)); mac("DifPropSup", fmt(100 * ic_pr[2], 1))
mac("RazonProp", fmt(razon_pp, 2))
mac("PctCosMayor", fmt(100 * pct_cos_mayor, 1))
mac("NAnualTrad", mil(na_trad)); mac("NAnualTec", mil(na_tec))
mac("PropAnualTrad", fmt(100 * xa_trad / na_trad, 1))
mac("PropAnualTec", fmt(100 * xa_tec / na_tec, 1))
mac("DifPropAnual", fmt(100 * (xa_trad / na_trad - xa_tec / na_tec), 1))
mac("ZAnual", fmt(za, 2)); mac("ZAnualP", fp(pra$p.value)); mac("ZAnualPRel", fp_rel(pra$p.value))
mac("DifPropAnualInf", fmt(100 * pra$conf.int[1], 1))
mac("DifPropAnualSup", fmt(100 * pra$conf.int[2], 1))

# Pregunta 3
mac_modelo <- function(r, pref) {
  di <- if (abs(r$cf[1, 1]) < 10) 3 else 2   # decimales del intercepto
  mac(paste0(pref, "N"),      mil(r$gl2 + 2))
  mac(paste0(pref, "IntEst"), num(r$cf[1, 1], di))
  mac(paste0(pref, "IntEE"),  num(r$cf[1, 2], di))
  mac(paste0(pref, "IntT"),   num(r$cf[1, 3], 2))
  mac(paste0(pref, "IntP"),   fp(r$cf[1, 4]))
  mac(paste0(pref, "IntPRel"), fp_rel(r$cf[1, 4]))
  mac(paste0(pref, "IntLi"),  num(r$ic[1, 1], di))
  mac(paste0(pref, "IntLs"),  num(r$ic[1, 2], di))
  mac(paste0(pref, "PenEst"), num(r$cf[2, 1], 2))
  mac(paste0(pref, "PenEE"),  num(r$cf[2, 2], 4))
  mac(paste0(pref, "PenT"),   num(r$cf[2, 3], 2))
  mac(paste0(pref, "PenP"),   fp(r$cf[2, 4]))
  mac(paste0(pref, "PenPRel"), fp_rel(r$cf[2, 4]))
  mac(paste0(pref, "PenLi"),  num(r$ic[2, 1], 2))
  mac(paste0(pref, "PenLs"),  num(r$ic[2, 2], 2))
  mac(paste0(pref, "Rdos"),   fmt(r$r2, 3))
  mac(paste0(pref, "RdosAj"), fmt(r$r2a, 3))
  mac(paste0(pref, "RdosPct"), fmt(100 * r$r2, 0))
  mac(paste0(pref, "F"),      num(r$F, 1))
  mac(paste0(pref, "glNum"),  mil(r$gl1))
  mac(paste0(pref, "glDen"),  mil(r$gl2))
  mac(paste0(pref, "pF"),     fp(r$pF))
  mac(paste0(pref, "pFRel"),  fp_rel(r$pF))
  mac(paste0(pref, "Sigma"),  num(r$sigma, 1))
  mac(paste0(pref, "Tcrit"),  fmt(qt(1 - ALFA / 2, r$gl), 3))
  mac(paste0(pref, "Fcrit"),  fmt(qf(1 - ALFA, r$gl1, r$gl2), 3))
}
mac_modelo(rA, "RG"); mac_modelo(rT, "RTrad"); mac_modelo(rK, "RTec")

mac_boot <- function(b, pref) {
  mac(paste0(pref, "BootLi"), fmt(b$li, 2))
  mac(paste0(pref, "BootLs"), fmt(b$ls, 2))
  mac(paste0(pref, "BootSE"), fmt(b$se, 3))
  mac(paste0(pref, "BootRazonAncho"), fmt(b$razon_ancho, 0))
}
mac_boot(bA, "RG"); mac_boot(bT, "RTrad"); mac_boot(bK, "RTec")
mac("BBoot", mil(B_BOOT))
mac("CorrArea", fmt(cor(d$area_sembrada, d$produccion), 2))
mac("MediaRendGlobal", fmt(mean(d$rendimiento), 2))

# Diagnostico de residuos
mac_diag <- function(x, pref) {
  mac(paste0(pref, "SWW"),     fmt(x$W, 2))
  mac(paste0(pref, "SWp"),     fp(x$p))
  mac(paste0(pref, "SWpRel"),  fp_rel(x$p))
  mac(paste0(pref, "SWn"),     mil(x$n_sw))
  mac(paste0(pref, "DERazon"), fmt(x$razon, 0))
}
mac_diag(dA, "RG"); mac_diag(dT, "RTrad"); mac_diag(dK, "RTec")
mac("RGPenSinGrandes",  fmt(coef(mA99)[2], 2))
mac("RGRdosSinGrandes", fmt(summary(mA99)$r.squared, 2))
mac("CorteArea", mil(corte99)); mac("NGrandes", mil(sum(grandes)))
mac("PctTecGrandes", fmt(100 * pct_tec_grandes, 0))
mac("PctTecTotal",   fmt(100 * n_tec / n4, 0))

# Decisiones (regla: se rechaza H0 si el valor p es menor que alfa)
mac("DecVT", decide(vt$p.value)); mac("DecW", decide(w$p.value))
mac("DecP", decide(tp$p.value)); mac("DecM", decide(wm$p.value))
mac("DecZ", decide(pr$p.value)); mac("DecZA", decide(pra$p.value))
mac("DecRG", decide(rA$cf[2, 4])); mac("DecRTrad", decide(rT$cf[2, 4]))
mac("DecRTec", decide(rK$cf[2, 4]))
mac("BrechaMinIC", fmt(min(li_br), 2)); mac("BrechaMaxIC", fmt(max(ls_br), 2))
mac("Nexcluidos", mil(n1 - n4))

mac("VersionR", R.version.string)
mac("VersionRCorta", paste0("R ", R.version$major, ".", R.version$minor))
mac("Semilla", SEMILLA)

writeLines(macros, file.path(RUTA_SALIDAS, "cifras.tex"))

reg("=========================================================================")
reg(" Fin del analisis inferencial.")
reg(" Figuras en '", RUTA_FIGURAS, "'; cifras LaTeX en '", RUTA_SALIDAS, "'.")
reg(" Se exportaron ", length(macros), " cifras como macros de LaTeX.")
reg(" Version de R: ", R.version.string)
reg("=========================================================================")
