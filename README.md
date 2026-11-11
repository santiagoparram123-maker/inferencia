# Inferencia Estadística

Material del curso y trabajos de Inferencia Estadística (Ciencia de Datos, Universidad Externado de Colombia, 2026-2): diapositivas, cuadernos, scripts de R, bases de datos, parciales y el proyecto integrador.

Este repositorio es privado. Incluye material de la profesora (diapositivas, guías y parciales), así que no lo hagas público.

## Qué hay en cada carpeta

| Carpeta | Contenido |
|---|---|
| `Diapositivas/` | 17 PDF de la profesora: bootstrap, estimadores, intervalos de confianza y de predicción, pruebas de hipótesis, MCMC, ANOVA, regresión, bondad de ajuste, no paramétrica y KDE. |
| `Cuadernos/` | Tres cuadernos en HTML (ANOVA, ANOVA de dos vías y regresión lineal simple) y `Apuntes_Teams/`, con 13 PDF de apuntes manuscritos de clase. Los apuntes son imágenes, sin texto seleccionable. |
| `Scripts_R/` | 14 scripts de R: los de clase y los de los parciales 1 y 2. |
| `Bases_de_Datos/` | Las 17 bases de las clases, las guías y los parciales (CSV y un TXT). |
| `Formulas_y_Parciales/` | Hojas de fórmulas, guías de estudio y de solución del Parcial 2, el microcurrículo, el artículo de Dunlosky y los parciales y preparciales. |
| `Proyecto/` | Guía del proyecto integrador y la entrega final sobre el cultivo de maíz. |
| `Archivos_Zip/` | Copias comprimidas de descargas (bases, corte 3, scripts y proyecto). |

## Proyecto integrador

El proyecto analiza el rendimiento, la pérdida de área y la relación entre área sembrada y producción en el cultivo de maíz en Colombia entre 2019 y 2025. Usa las Evaluaciones Agropecuarias Municipales (EVA) de la UPRA, publicadas en Datos Abiertos Colombia.

- Informe: [`Proyecto/Entrega_final_Inferencia_maiz.pdf`](Proyecto/Entrega_final_Inferencia_maiz.pdf)
- Análisis completo, datos, figuras y código LaTeX: [`Proyecto/proyecto_final/`](Proyecto/proyecto_final/)
- Cómo reproducirlo: [`Proyecto/proyecto_final/README.md`](Proyecto/proyecto_final/README.md). Hace falta R y una distribución de LaTeX; con eso, `bash construir.sh` rehace las cifras, las figuras y el PDF.

`Proyecto/analisis_final.R` y `Proyecto/Entrega_final_Inferencia_maiz.pdf` son copias de los archivos de `proyecto_final/`.

El zip `Archivos_Zip/proyecto_entrega_final_maiz.zip` guarda la versión anterior del proyecto, la que todavía usaba el modelo de regresión en logaritmos.

## Lo que no está aquí

`prueba/` es la app de estudio del Parcial 2. Tiene su propio repositorio privado en GitHub (`santiagoparram123-maker/prueba`) y por eso este repositorio la ignora (ver `.gitignore`).

## Software

El curso usa R. El proyecto solo necesita funciones base de R y el paquete `boot`, que viene instalado con R. Algunos scripts de clase cargan otros paquetes (`tolerance`, `effsize`, `ggplot2`, `agricolae`, `lmtest`, `car`), que hay que instalar aparte.
