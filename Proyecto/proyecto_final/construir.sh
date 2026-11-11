#!/usr/bin/env bash
# =============================================================================
#  Reconstruye la entrega final completa desde cero.
#
#  Requisitos: R (>= 4.0) y una distribucion de LaTeX con pdflatex y el
#              paquete babel-spanish.
#  Uso:        bash construir.sh
#
#  Antes de correrlo, el archivo datos/eva_cereales.csv debe existir.
#  Se descarga del portal Datos Abiertos Colombia con la consulta que aparece
#  en la variable DESCARGA_URL del script de R (o en el README).
# =============================================================================

set -e
cd "$(dirname "$0")"

if [ ! -f datos/eva_cereales.csv ]; then
  echo "ERROR: falta datos/eva_cereales.csv"
  echo "Descarguelo con la consulta indicada en el README y vuelva a correr."
  exit 1
fi

echo "[1/3] Analisis inferencial en R ..."
Rscript analisis_final.R > salidas/consola.log

echo "[2/3] Compilando el informe (pdflatex, dos pasadas) ..."
cd informe
pdflatex -interaction=nonstopmode informe_final.tex > /dev/null
pdflatex -interaction=nonstopmode informe_final.tex > /dev/null
cd ..

echo "[3/3] Listo."
echo "  Informe    : informe/informe_final.pdf"
echo "  Figuras    : figuras/"
echo "  Resultados : salidas/resultados_inferenciales.txt"
