# Proyecto integrador — Inferencia Estadística — Entrega final

Universidad Externado de Colombia · Departamento de Matemáticas · Ciencia de Datos · 2026-2
Santiago Bernal · Santiago Parra · Daniel Núñez · Andrés Felipe Baquero

**Tema:** rendimiento, pérdida de área y escala en el cultivo de maíz en
Colombia, 2019–2025, a partir de las Evaluaciones Agropecuarias Municipales
(EVA) de la UPRA.

---

## 1. Qué hay en esta carpeta

```
analisis_final.R            Script de R que produce TODO (cifras, tablas y figuras)
construir.sh                Corre el script y compila el informe en un solo paso
datos/eva_cereales.csv      Base descargada el 6 de noviembre de 2026 (grupo Cereales)
figuras/                    fig1 a fig6, en PDF (para LaTeX) y PNG a 300 ppp
salidas/
  cifras.tex                Todas las cifras del informe como macros de LaTeX
  resultados_inferenciales.txt  Registro completo de cada contraste
informe/
  informe_final.tex         Documento principal
  preambulo.tex             Formato APA 7 (Times 12, doble espacio, márgenes de 1")
  portada.tex               Portada
  referencias.tex           Referencias APA 7
  informe_final.pdf         El informe compilado (lo que se entrega)
```

## 2. Cómo reproducirlo

Con la base ya incluida en `datos/`, basta con:

```bash
bash construir.sh
```

O por separado:

```bash
Rscript analisis_final.R
cd informe && pdflatex informe_final.tex && pdflatex informe_final.tex
```

Para descargar una versión nueva de la base, pegar esta consulta en el
navegador y guardar el archivo como `datos/eva_cereales.csv`:

```
https://www.datos.gov.co/resource/uejq-wxrr.csv?$where=grupo_cultivo='Cereales'&$limit=100000
```

El portal se actualiza; con otra descarga las cifras pueden cambiar
ligeramente. El informe se actualiza solo, porque ningún número está escrito
a mano: todos salen de `salidas/cifras.tex`.

## 3. Qué responde el informe

| Pregunta de la Entrega 1 | Contraste | Resultado |
|---|---|---|
| ¿Difiere el rendimiento medio entre tecnificado y tradicional? | *F* de varianzas → *t* de Welch; complemento *t* pareada | Tecnificado +2,52 t/ha (IC 95 %: 2,45 a 2,58); dentro del mismo municipio y semestre, +2,36 t/ha |
| ¿Difiere la proporción de registros con pérdida de área? | *Z* para dos proporciones (`prop.test`) | Tradicional 43,4 % frente a tecnificado 34,6 %; diferencia de 8,8 p. p. (IC 95 %: 7,1 a 10,5) |
| ¿Cómo se relaciona el área sembrada con la producción? | Regresión lineal simple (todos los registros y dentro de cada sistema): *t*, *F*, R², diagnóstico de residuos con Shapiro-Wilk e IC bootstrap percentil de la pendiente | Cada hectárea adicional se asocia con 3,60 t más (IC bootstrap 95 %: 2,89 a 4,95) y R² = 0,76; los supuestos de normalidad y varianza constante no se cumplen, por eso el IC bootstrap. La pendiente es un rendimiento: 1,83 t/ha en el tradicional y 3,75 t/ha en el tecnificado |

Todos los contrastes son bilaterales, con α = 0,05. Cada estadístico se
calcula con la función de R vista en clase y se verifica a mano con `pt`,
`qt`, `pnorm`, `qnorm` y `pf` (el script se detiene si no coinciden).

## 4. Funciones usadas

Solo funciones vistas en clase: `var.test`, `t.test` (Welch y pareada),
`prop.test`, `lm`, `summary`, `confint`, `anova`, `shapiro.test`, y `boot` y
`boot.ci` del paquete `boot` (viene instalado con R), más `pt`, `qt`,
`pnorm`, `qnorm`, `pf` y `qf` para las verificaciones. Sin transformaciones
logarítmicas, sin regresión múltiple ni modelos predictivos.

El bootstrap (2.000 remuestreos por modelo, tres modelos) hace que el script
tarde alrededor de un minuto y medio.
