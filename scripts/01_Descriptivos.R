# ==============================================================
# Título:     Cargar datos, análisis exploratorio y estadísticos 
#             básicos
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-05-31
# Base datos: Vaporosas.db
# ==============================================================

# 0. Configuración inicial -----------------------------------------
rm(list = ls())                     # Limpiar entorno
options(scipen = 999)               # Desactivar notación científica
# Esto fuerza a R a usar el estándar UTF-8 (especialmente útil en Windows o Linux)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# 1. Librerías -----------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)

# 2. Conexión ------------------------------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# Verificar conexión
dbListTables(con)

# 3. Consultas -----------------------------------------------------
# Cargar datos
datos <- dbGetQuery(con, "
SELECT
    l.nombre,
    c.agua_L_por_km,
    c.carbon_kg_por_km,
    c.velocidad_media_km_h
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
")

# Limpiar nombres
datos$nombre <- trimws(datos$nombre)

# 4. Funciones auxiliares -----------------------------------------

# Función para calcular estadísticos completos
calcular_estadisticos <- function(df, variable, nombre_variable) {
  
  valores <- df[[variable]]
  valores <- valores[!is.na(valores)]
  
  n <- length(valores)
  media <- mean(valores)
  mediana <- median(valores)
  desv <- sd(valores)
  cv <- (desv / media) * 100
  minimo <- min(valores)
  maximo <- max(valores)
  rango <- maximo - minimo
  q1 <- quantile(valores, 0.25)
  q3 <- quantile(valores, 0.75)
  iqr <- q3 - q1
  p10 <- quantile(valores, 0.10)
  p90 <- quantile(valores, 0.90)
  
  # Asimetría (skewness)
  skewness <- (sum((valores - media)^3) / n) / (desv^3)
  
  result <- data.frame(
    variable = nombre_variable,
    n = n,
    media = round(media, 2),
    mediana = round(mediana, 2),
    desviacion = round(desv, 2),
    cv_porcentaje = round(cv, 1),
    minimo = round(minimo, 2),
    maximo = round(maximo, 2),
    rango = round(rango, 2),
    p10 = round(p10, 2),
    p25 = round(q1, 2),
    p75 = round(q3, 2),
    p90 = round(p90, 2),
    iqr = round(iqr, 2),
    skewness = round(skewness, 3)
  )
  
  return(result)
}

# 5. Análisis ------------------------------------------------------

# Crear carpetas si no existen
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 5.1 Boxplots ----------------------------------------------------
# Boxplot de carbón (pantalla)
boxplot(carbon_kg_por_km ~ nombre, data = datos,
        main = "Consumo de carbón por locomotora",
        xlab = "Locomotora", ylab = "kg/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))

# Boxplot de agua (pantalla)
boxplot(agua_L_por_km ~ nombre, data = datos,
        main = "Consumo de agua por locomotora",
        xlab = "Locomotora", ylab = "L/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))

# Boxplot de velocidad (pantalla) - NUEVO
boxplot(velocidad_media_km_h ~ nombre, data = datos,
        main = "Velocidad media por locomotora",
        xlab = "Locomotora", ylab = "km/h",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))

# 5.2 Estadísticos básicos (media y sd) por locomotora -----------
estadisticos_carbon <- datos %>%
  group_by(nombre) %>%
  summarise(
    media_carbon = mean(carbon_kg_por_km, na.rm = TRUE),
    sd_carbon = sd(carbon_kg_por_km, na.rm = TRUE),
    n = n()
  )

estadisticos_agua <- datos %>%
  group_by(nombre) %>%
  summarise(
    media_agua = mean(agua_L_por_km, na.rm = TRUE),
    sd_agua = sd(agua_L_por_km, na.rm = TRUE),
    n = n()
  )

estadisticos_velocidad <- datos %>%
  group_by(nombre) %>%
  summarise(
    media_velocidad = mean(velocidad_media_km_h, na.rm = TRUE),
    sd_velocidad = sd(velocidad_media_km_h, na.rm = TRUE),
    n = n()
  )

# 5.3 Estadísticos completos por locomotora -----------------------
resultado_completo <- data.frame()

locomotoras <- unique(datos$nombre)

for (loc in locomotoras) {
  
  cat("\n========================================\n")
  cat("LOCOMOTORA:", loc, "\n")
  cat("========================================\n")
  
  df_loc <- datos %>% filter(nombre == loc)
  
  # Agua
  stats_agua <- calcular_estadisticos(df_loc, "agua_L_por_km", "Agua (L/km)")
  stats_agua$locomotora <- loc
  
  # Carbón
  stats_carbon <- calcular_estadisticos(df_loc, "carbon_kg_por_km", "Carbón (kg/km)")
  stats_carbon$locomotora <- loc
  
  # Velocidad
  stats_vel <- calcular_estadisticos(df_loc, "velocidad_media_km_h", "Velocidad (km/h)")
  stats_vel$locomotora <- loc
  
  resultado_completo <- rbind(resultado_completo, stats_agua, stats_carbon, stats_vel)
  
  # Mostrar en consola
  cat("\n--- AGUA (L/km) ---\n")
  print(stats_agua)
  cat("\n--- CARBÓN (kg/km) ---\n")
  print(stats_carbon)
  cat("\n--- VELOCIDAD (km/h) ---\n")
  print(stats_vel)
}

# 5.4 Tablas resumen por variable ---------------------------------
resumen_agua <- resultado_completo %>%
  filter(variable == "Agua (L/km)") %>%
  select(locomotora, media, mediana, desviacion, cv_porcentaje, 
         minimo, maximo, iqr) %>%
  arrange(desc(media))

resumen_carbon <- resultado_completo %>%
  filter(variable == "Carbón (kg/km)") %>%
  select(locomotora, media, mediana, desviacion, cv_porcentaje, 
         minimo, maximo, iqr) %>%
  arrange(desc(media))

resumen_velocidad <- resultado_completo %>%
  filter(variable == "Velocidad (km/h)") %>%
  select(locomotora, media, mediana, desviacion, cv_porcentaje, 
         minimo, maximo, iqr) %>%
  arrange(desc(media))

# 5.5 Interpretación del CV ---------------------------------------
cat("\n\n========================================\n")
cat("INTERPRETACIÓN DEL CV (Coeficiente de Variación)\n")
cat("========================================\n")
cat("CV < 30%: Consumo muy estable, predecible\n")
cat("CV 30-60%: Dispersión moderada\n")
cat("CV > 60%: Consumo muy variable, depende del tramo/conductor\n\n")

for (loc in locomotoras) {
  cv_agua <- resumen_agua %>% filter(locomotora == loc) %>% pull(cv_porcentaje)
  cv_carbon <- resumen_carbon %>% filter(locomotora == loc) %>% pull(cv_porcentaje)
  
  cat(loc, "\n")
  cat("  Agua CV:", cv_agua, "% - ", 
      ifelse(cv_agua < 30, "estable", ifelse(cv_agua < 60, "moderada", "muy variable")), "\n")
  cat("  Carbón CV:", cv_carbon, "% - ",
      ifelse(cv_carbon < 30, "estable", ifelse(cv_carbon < 60, "moderada", "muy variable")), "\n")
}

# 6. Exportar resultados -------------------------------------------

# 6.1 Guardar boxplots como PNG y SVG 
png("exportaciones/graficos/01_boxplot_carbon.png", width = 800, height = 600)
boxplot(carbon_kg_por_km ~ nombre, data = datos,
        main = "Consumo de carbón por locomotora",
        xlab = "Locomotora", ylab = "kg/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

svg("exportaciones/graficos/01_boxplot_carbon.svg", width = 8, height = 6) # <--- Cambio clave
boxplot(carbon_kg_por_km ~ nombre, data = datos,
        main = "Consumo de carbón por locomotora",
        xlab = "Locomotora", ylab = "kg/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

png("exportaciones/graficos/01_boxplot_agua.png", width = 800, height = 600)
boxplot(agua_L_por_km ~ nombre, data = datos,
        main = "Consumo de agua por locomotora",
        xlab = "Locomotora", ylab = "L/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

svg("exportaciones/graficos/01_boxplot_agua.svg", width = 8, height = 6) 
boxplot(agua_L_por_km ~ nombre, data = datos,
        main = "Consumo de agua por locomotora",
        xlab = "Locomotora", ylab = "L/km",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

# Guardar boxplot de velocidad - NUEVO
png("exportaciones/graficos/01_boxplot_velocidad.png", width = 800, height = 600)
boxplot(velocidad_media_km_h ~ nombre, data = datos,
        main = "Velocidad media por locomotora",
        xlab = "Locomotora", ylab = "km/h",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

svg("exportaciones/graficos/01_boxplot_velocidad.svg", width = 8, height = 6) 
boxplot(velocidad_media_km_h ~ nombre, data = datos,
        main = "Velocidad media por locomotora",
        xlab = "Locomotora", ylab = "km/h",
        col = c("cyan2", "chocolate1", "darkseagreen1", "firebrick1"))
dev.off()

# 6.2 Guardar estadísticos básicos (media y sd)
write.csv(estadisticos_carbon, "exportaciones/csv/01_estadisticos_carbon.csv", row.names = FALSE)
write.csv(estadisticos_agua, "exportaciones/csv/01_estadisticos_agua.csv", row.names = FALSE)
write.csv(estadisticos_velocidad, "exportaciones/csv/01_estadisticos_velocidad.csv", row.names = FALSE)

# 6.3 Guardar estadísticos completos
write.csv(resultado_completo, "exportaciones/csv/01_estadisticos_completos.csv", row.names = FALSE)

# 6.4 Guardar resúmenes por variable
write.csv(resumen_agua, "exportaciones/csv/01_resumen_agua.csv", row.names = FALSE)
write.csv(resumen_carbon, "exportaciones/csv/01_resumen_carbon.csv", row.names = FALSE)
write.csv(resumen_velocidad, "exportaciones/csv/01_resumen_velocidad.csv", row.names = FALSE)

# 7. Mostrar resúmenes finales ------------------------------------
cat("\n\n========================================\n")
cat("RESUMEN - CONSUMO DE AGUA (L/km)\n")
cat("========================================\n")
print(resumen_agua)

cat("\n\n========================================\n")
cat("RESUMEN - CONSUMO DE CARBÓN (kg/km)\n")
cat("========================================\n")
print(resumen_carbon)

cat("\n\n========================================\n")
cat("RESUMEN - VELOCIDAD MEDIA (km/h)\n")
cat("========================================\n")
print(resumen_velocidad)

# 8. Desconexión (automática con on.exit)

cat("\n\n✅ Análisis completado\n")
cat("✅ Boxplots guardados en: exportaciones/graficos/\n")
cat("   - boxplot_carbon.svg\n")
cat("   - boxplot_agua.svg\n")
cat("   - boxplot_velocidad.svg\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")
cat("   - estadisticos_carbon.csv\n")
cat("   - estadisticos_agua.csv\n")
cat("   - estadisticos_velocidad.csv\n")
cat("   - estadisticos_completos.csv\n")
cat("   - resumen_agua.csv\n")
cat("   - resumen_carbon.csv\n")
cat("   - resumen_velocidad.csv\n")
