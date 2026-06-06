# ======================================================
# run_dev.R — Ejecutar LDAShiny en modo desarrollo
# ======================================================
# Ejecuta este archivo con: source("dev/run_dev.R")
# o con el atajo Ctrl+Shift+S desde RStudio

# 1. Cargar devtools (instalar si no está)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# 2. Cargar el paquete en modo desarrollo (sin instalar)
devtools::load_all(".")

# 3. Lanzar la app
run_LDAShiny()
