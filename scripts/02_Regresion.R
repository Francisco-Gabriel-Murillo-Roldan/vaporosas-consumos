# ==============================================================
# Título:     Regresión lineal - Consumos vs Velocidad
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-09
# Base datos: Vaporosas.db
# ==============================================================

# 0. Configuración inicial -----------------------------------------
rm(list = ls())
options(scipen = 999)

# 1. Librerías -----------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# 2. Conexión a la base de datos ----------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 3. Cargar datos -------------------------------------------------
datos <- dbGetQuery(con, "
SELECT 
    l.nombre as locomotora,
    c.carbon_kg_por_km,
    c.agua_L_por_km,
    c.velocidad_media_km_h,
    c.relacion_ac_decimal,
    c.corte_porcentaje,
    c.regulador_porcentaje,
    r.nombre_ruta,
    p.clima
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Pruebas p ON c.id_prueba = p.id_prueba
JOIN Rutas r ON p.id_ruta = r.id_ruta
WHERE l.tipo = 'Vapor'
")

# Limpiar nombres
datos$locomotora <- trimws(datos$locomotora)
datos$nombre_ruta <- trimws(datos$nombre_ruta)

# 4. Crear carpetas ------------------------------------------------
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 5. Función para regresión y gráfico -----------------------------
realizar_regresion <- function(df, x_var, y_var, x_nombre, y_nombre, titulo_extra = "", es_todas = FALSE) {
  
  # Eliminar filas con NA
  df_clean <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  
  if (nrow(df_clean) < 3) {
    return(list(error = "Datos insuficientes"))
  }
  
  # Modelo lineal
  modelo <- lm(as.formula(paste(y_var, "~", x_var)), data = df_clean)
  
  # Coeficientes
  intercepto <- coef(modelo)[1]
  pendiente <- coef(modelo)[2]
  r2 <- summary(modelo)$r.squared
  r2_ajustado <- summary(modelo)$adj.r.squared
  p_valor <- summary(modelo)$coefficients[2, 4]
  
  # ECUACIÓN CORREGIDA - siempre muestra el signo del intercepto
  signo_intercepto <- ifelse(intercepto >= 0, "+", "-")
  ecuacion <- sprintf("%s = %.2f · Vel %s %.1f\nR² = %.3f", 
                      y_nombre, pendiente, 
                      signo_intercepto,
                      abs(round(intercepto, 1)), r2)
  
  # Posición para anotación
  x_range <- max(df_clean[[x_var]], na.rm = TRUE) - min(df_clean[[x_var]], na.rm = TRUE)
  y_range <- max(df_clean[[y_var]], na.rm = TRUE) - min(df_clean[[y_var]], na.rm = TRUE)
  x_pos <- min(df_clean[[x_var]], na.rm = TRUE) + x_range * 0.4
  y_pos <- max(df_clean[[y_var]], na.rm = TRUE) - y_range * 0.1
  
  # Gráfico
  p <- ggplot(df_clean, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = nombre_ruta, shape = clima), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "gray80", alpha = 0.3) +
    annotate("text", x = x_pos, y = y_pos,
             label = paste0(ecuacion, "\np = ", format(p_valor, scientific = TRUE, digits = 3)),
             size = 4, hjust = 0, fontface = "bold") +
    labs(
      title = paste(y_nombre, "vs", x_nombre, titulo_extra),
      subtitle = paste("n =", nrow(df_clean)),
      x = x_nombre,
      y = y_nombre,
      color = "Ruta",
      shape = "Clima"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = margin(5, 15, 5, 5)
    )
  
  # Identificar conducción por expansión
  if ("corte_porcentaje" %in% colnames(df_clean) && "regulador_porcentaje" %in% colnames(df_clean)) {
    df_expansion <- df_clean %>%
      mutate(expansion = ifelse(corte_porcentaje < 30 & regulador_porcentaje > 80, "Expansión", "Normal"))
    
    if (sum(df_expansion$expansion == "Expansión", na.rm = TRUE) > 0) {
      p <- p + geom_point(data = subset(df_expansion, expansion == "Expansión"),
                          aes(x = .data[[x_var]], y = .data[[y_var]]),
                          color = "gold", size = 4, shape = 8, alpha = 0.8) +
        annotate("text", x = x_pos, y = y_pos - y_range * 0.15,
                 label = "★ Expansión", size = 3, color = "gold", hjust = 0)
    }
  }
  
  # Determinar el nombre de locomotora para los resultados
  if (es_todas) {
    nombre_loc <- "Todas"
  } else {
    nombre_loc <- unique(df_clean$locomotora)[1]
  }
  
  # Resultados
  resultados <- data.frame(
    locomotora = nombre_loc,
    x_variable = x_nombre,
    y_variable = y_nombre,
    n = nrow(df_clean),
    pendiente = round(pendiente, 4),
    intercepto = round(intercepto, 2),
    r2 = round(r2, 4),
    r2_ajustado = round(r2_ajustado, 4),
    p_valor = format(p_valor, scientific = TRUE, digits = 3),
    ecuacion = ecuacion
  )
  
  return(list(grafico = p, modelo = modelo, resultados = resultados))
}

# 6. Análisis por LOCOMOTORA --------------------------------------
locomotoras <- unique(datos$locomotora)
resultados_individuales <- data.frame()

for (loc in locomotoras) {
  
  cat("\n========================================\n")
  cat("LOCOMOTORA:", loc, "\n")
  cat("========================================\n")
  
  df_loc <- datos %>% filter(locomotora == loc)
  
  # 6.1 Carbón vs Velocidad
  cat("\n--- Carbón (kg/km) vs Velocidad (km/h) ---\n")
  res_carbon_vs_vel <- realizar_regresion(df_loc, 
                                          "velocidad_media_km_h", 
                                          "carbon_kg_por_km",
                                          "Velocidad (km/h)", 
                                          "Carbón (kg/km)",
                                          paste("-", loc),
                                          es_todas = FALSE)
  
  if (!is.null(res_carbon_vs_vel$grafico)) {
    print(res_carbon_vs_vel$grafico)
    ggsave(paste0("exportaciones/graficos/02_regresion_carbon_vs_vel_", 
                  gsub(" ", "_", gsub("-", "", loc)), ".svg"),
           res_carbon_vs_vel$grafico, width = 11, height = 8, device = "svg")
    resultados_individuales <- rbind(resultados_individuales, res_carbon_vs_vel$resultados)
  }
  
  # 6.2 Agua vs Velocidad
  cat("\n--- Agua (L/km) vs Velocidad (km/h) ---\n")
  res_agua_vs_vel <- realizar_regresion(df_loc,
                                        "velocidad_media_km_h",
                                        "agua_L_por_km",
                                        "Velocidad (km/h)",
                                        "Agua (L/km)",
                                        paste("-", loc),
                                        es_todas = FALSE)
  
  if (!is.null(res_agua_vs_vel$grafico)) {
    print(res_agua_vs_vel$grafico)
    ggsave(paste0("exportaciones/graficos/02_regresion_agua_vs_vel_", 
                  gsub(" ", "_", gsub("-", "", loc)), ".svg"),
           res_agua_vs_vel$grafico, width = 11, height = 8, device = "svg")
    resultados_individuales <- rbind(resultados_individuales, res_agua_vs_vel$resultados)
  }
  
  # 6.3 Carbón vs Agua
  cat("\n--- Carbón (kg/km) vs Agua (L/km) ---\n")
  res_carbon_vs_agua <- realizar_regresion(df_loc,
                                           "agua_L_por_km",
                                           "carbon_kg_por_km",
                                           "Agua (L/km)",
                                           "Carbón (kg/km)",
                                           paste("-", loc),
                                           es_todas = FALSE)
  
  if (!is.null(res_carbon_vs_agua$grafico)) {
    print(res_carbon_vs_agua$grafico)
    ggsave(paste0("exportaciones/graficos/02_regresion_carbon_vs_agua_", 
                  gsub(" ", "_", gsub("-", "", loc)), ".svg"),
           res_carbon_vs_agua$grafico, width = 11, height = 8, device = "svg")
    resultados_individuales <- rbind(resultados_individuales, res_carbon_vs_agua$resultados)
  }
}

# 7. Análisis con TODAS las locomotoras juntas --------------------
cat("\n\n========================================\n")
cat("TODAS LAS LOCOMOTORAS JUNTAS\n")
cat("========================================\n")

# 7.1 Carbón vs Velocidad
cat("\n--- Carbón (kg/km) vs Velocidad (km/h) - Todas ---\n")
res_all_carbon_vel <- realizar_regresion(datos,
                                         "velocidad_media_km_h",
                                         "carbon_kg_por_km",
                                         "Velocidad (km/h)",
                                         "Carbón (kg/km)",
                                         "- Todas las locomotoras",
                                         es_todas = TRUE)

if (!is.null(res_all_carbon_vel$grafico)) {
  print(res_all_carbon_vel$grafico)
  ggsave("exportaciones/graficos/02_regresion_carbon_vs_vel_todas.svg",
         res_all_carbon_vel$grafico, width = 11, height = 8, device = "svg")
}

# 7.2 Agua vs Velocidad - Todas
cat("\n--- Agua (L/km) vs Velocidad (km/h) - Todas ---\n")
res_all_agua_vel <- realizar_regresion(datos,
                                       "velocidad_media_km_h",
                                       "agua_L_por_km",
                                       "Velocidad (km/h)",
                                       "Agua (L/km)",
                                       "- Todas las locomotoras",
                                       es_todas = TRUE)

if (!is.null(res_all_agua_vel$grafico)) {
  print(res_all_agua_vel$grafico)
  ggsave("exportaciones/graficos/02_regresion_agua_vs_vel_todas.svg",
         res_all_agua_vel$grafico, width = 11, height = 8, device = "svg")
}

# 7.3 Carbón vs Agua - Todas
cat("\n--- Carbón (kg/km) vs Agua (L/km) - Todas ---\n")
res_all_carbon_agua <- realizar_regresion(datos,
                                          "agua_L_por_km",
                                          "carbon_kg_por_km",
                                          "Agua (L/km)",
                                          "Carbón (kg/km)",
                                          "- Todas las locomotoras",
                                          es_todas = TRUE)

if (!is.null(res_all_carbon_agua$grafico)) {
  print(res_all_carbon_agua$grafico)
  ggsave("exportaciones/graficos/02_regresion_carbon_vs_agua_todas.svg",
         res_all_carbon_agua$grafico, width = 11, height = 8, device = "svg")
}

# 8. Tabla resumen de regresiones ---------------------------------
resultados_todas <- rbind(
  res_all_carbon_vel$resultados,
  res_all_agua_vel$resultados,
  res_all_carbon_agua$resultados
)

# Unir resultados individuales y totales
resultados_final <- rbind(resultados_individuales, resultados_todas)

# Limpiar nombres y eliminar duplicados
resultados_final$locomotora <- trimws(resultados_final$locomotora)

# Eliminar duplicados exactos (misma locomotora, misma relación, misma pendiente)
resultados_final <- resultados_final %>%
  distinct(locomotora, x_variable, y_variable, pendiente, .keep_all = TRUE)

# Añadir columna 'relacion' para facilitar filtros
resultados_final <- resultados_final %>%
  mutate(relacion = paste(y_variable, "vs", x_variable))

# Guardar tabla resumen
write.csv(resultados_final, "exportaciones/csv/02_resultados_regresiones.csv", row.names = FALSE)

# 9. Gráfico comparativo de pendientes ----------------------------
# Excluir la locomotora "Todas" para no duplicar
comparacion_pendientes <- resultados_final %>%
  filter(x_variable != y_variable,
         locomotora != "Todas") %>%
  select(locomotora, x_variable, y_variable, pendiente, r2, relacion)

p_pendientes <- ggplot(comparacion_pendientes, 
                       aes(x = locomotora, y = pendiente, fill = relacion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(pendiente, 3)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Comparación de pendientes de regresión",
    subtitle = "Pendiente = cambio en consumo por unidad de velocidad (o agua)",
    x = "Locomotora",
    y = "Pendiente",
    fill = "Relación"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/02_comparacion_pendientes.svg", p_pendientes, width = 12, height = 7, device = "svg")
print(p_pendientes)

# 10. Mostrar tabla resumen en consola ----------------------------
cat("\n\n========================================\n")
cat("TABLA RESUMEN DE REGRESIONES\n")
cat("========================================\n")
print(resultados_final %>% select(locomotora, relacion, pendiente, r2, p_valor))

# 11. Resumen ejecutivo -------------------------------------------
cat("\n\n========================================\n")
cat("RESUMEN EJECUTIVO - REGRESIONES\n")
cat("========================================\n")

mejor_r2 <- resultados_final %>% filter(r2 == max(r2))
peor_r2 <- resultados_final %>% filter(r2 == min(r2))

cat("\n--- CALIDAD DEL AJUSTE (R²) ---\n")
cat("Mejor ajuste:", mejor_r2$relacion[1], "en", mejor_r2$locomotora[1], 
    "→ R² =", mejor_r2$r2[1], "\n")
cat("Peor ajuste:", peor_r2$relacion[1], "en", peor_r2$locomotora[1], 
    "→ R² =", peor_r2$r2[1], "\n")

# Sensibilidad de pendientes (solo locomotoras individuales)
cat("\n--- SENSIBILIDAD (pendientes) ---\n")

carbon_vs_vel <- resultados_final %>% 
  filter(grepl("Carbón.*Velocidad", relacion), locomotora != "Todas")
agua_vs_vel <- resultados_final %>% 
  filter(grepl("Agua.*Velocidad", relacion), locomotora != "Todas")

if (nrow(carbon_vs_vel) > 0) {
  cat("Carbón vs Velocidad:\n")
  for (i in 1:nrow(carbon_vs_vel)) {
    cat("  ", carbon_vs_vel$locomotora[i], ": pendiente =", carbon_vs_vel$pendiente[i], 
        "kg/km por km/h\n")
  }
}

if (nrow(agua_vs_vel) > 0) {
  cat("\nAgua vs Velocidad:\n")
  for (i in 1:nrow(agua_vs_vel)) {
    cat("  ", agua_vs_vel$locomotora[i], ": pendiente =", agua_vs_vel$pendiente[i], 
        "L/km por km/h\n")
  }
}

# 12. Conducción por expansión ------------------------------------
cat("\n\n========================================\n")
cat("CONDUCCIÓN POR EXPANSIÓN\n")
cat("========================================\n")
cat("Los puntos marcados con ★ en los gráficos indican tramos donde:\n")
cat("  • Corte < 30%\n")
cat("  • Regulador > 80%\n")
cat("Esta técnica permite circular con consumo mínimo de vapor.\n")
cat("La presencia de estos puntos indica buena técnica del maquinista.\n")

cat("\n\n✅ Regresiones completadas\n")
cat("✅ Gráficos guardados en: exportaciones/graficos/\n")
cat("   - regresion_carbon_vs_vel_*.svg (por locomotora)\n")
cat("   - regresion_agua_vs_vel_*.svg (por locomotora)\n")
cat("   - regresion_carbon_vs_agua_*.svg (por locomotora)\n")
cat("   - regresion_*_todas.svg (todas las locomotoras)\n")
cat("   - comparacion_pendientes.svg\n")
cat("✅ CSV guardado en: exportaciones/csv/resultados_regresiones.csv\n")
