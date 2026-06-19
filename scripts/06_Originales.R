# ==============================================================
# Título:     Gráficas originales (con Perfiles_Simulador)
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-10
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

# 2. Conexión ------------------------------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 3. Función para cargar datos de una ruta ------------------------
cargar_ruta <- function(id_ruta, nombre_ruta) {
  
  cat("\n--- Cargando:", nombre_ruta, "---\n")
  
  # Perfil del simulador
  perfil <- dbGetQuery(con, sprintf("
    SELECT 
        distancia_acumulada_m / 1000 as distancia_km,
        altura_msnm as cota,
        pendiente_porcentaje as pendiente,
        curvatura,
        elemento_via_texto as elemento,
        elemento_via_tipo as tipo_elemento
    FROM Perfiles_Simulador 
    WHERE id_ruta = %d
    ORDER BY distancia_acumulada_m
  ", id_ruta))
  
  # Zonas
  zonas <- dbGetQuery(con, sprintf("
    SELECT 
        nombre_zona,
        distancia_inicio_km,
        distancia_fin_km,
        tipo
    FROM Zonas 
    WHERE id_ruta = %d
    ORDER BY distancia_inicio_km
  ", id_ruta))
  
  cat("  Perfil:", nrow(perfil), "puntos\n")
  cat("  Distancia total:", round(max(perfil$distancia_km), 2), "km\n")
  cat("  Zonas:", nrow(zonas), "\n")
  
  return(list(perfil = perfil, zonas = zonas, nombre = nombre_ruta, id = id_ruta))
}

# 4. Cargar las tres rutas -----------------------------------------
rutas <- list(
  cargar_ruta(1, "Vallarizasoria (Soria - Burgos)"),
  cargar_ruta(2, "Eje Central Andaluces (Granada - Loja)"),
  cargar_ruta(3, "CTN Test Route")
)

# 5. Crear carpetas ------------------------------------------------
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 6. Generar gráficos para cada ruta ------------------------------
generar_graficos <- function(ruta) {
  
  cat("\n========================================\n")
  cat("Generando gráficos para:", ruta$nombre, "\n")
  cat("========================================\n")
  
  perfil <- ruta$perfil
  zonas <- ruta$zonas
  
  # Añadir curvatura magnitud
  perfil$curvatura_magnitud <- abs(perfil$curvatura)
  
  # 6.1 Perfil topográfico con zonas
  p_perfil <- ggplot() +
    geom_line(data = perfil, aes(x = distancia_km, y = cota),
              color = "gray30", linewidth = 1) +
    geom_rect(data = zonas, 
              aes(xmin = distancia_inicio_km, xmax = distancia_fin_km,
                  ymin = -Inf, ymax = Inf, fill = tipo),
              alpha = 0.3) +
    geom_point(data = perfil %>% filter(tipo_elemento == 1 & !is.na(elemento) & elemento != ""),
               aes(x = distancia_km, y = cota), color = "blue", size = 2, shape = 17) +
    scale_fill_manual(values = c("Ascenso" = "firebrick",
                                 "Descenso" = "steelblue",
                                 "Ondulado" = "darkgreen",
                                 "Mixto" = "purple")) +
    labs(
      title = paste("Perfil topográfico -", ruta$nombre),
      subtitle = "Datos desde Perfiles_Simulador (Open Rails) | Zonas superpuestas",
      x = "Distancia (km)",
      y = "Cota (msnm)",
      fill = "Tipo de zona"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  ggsave(paste0("exportaciones/graficos/06_perfil_", ruta$id, "_", gsub(" ", "_", ruta$nombre), ".svg"),
         p_perfil, width = 14, height = 7, device = "svg")
  print(p_perfil)
  
  # 6.2 Perfil de pendientes
  p_pendiente <- ggplot() +
    geom_line(data = perfil, aes(x = distancia_km, y = pendiente),
              color = "darkgreen", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_rect(data = zonas, 
              aes(xmin = distancia_inicio_km, xmax = distancia_fin_km,
                  ymin = -Inf, ymax = Inf, fill = tipo),
              alpha = 0.2) +
    scale_fill_manual(values = c("Ascenso" = "firebrick",
                                 "Descenso" = "steelblue",
                                 "Ondulado" = "darkgreen",
                                 "Mixto" = "purple")) +
    labs(
      title = paste("Perfil de pendientes -", ruta$nombre),
      subtitle = "Pendiente positiva = subida | Negativa = bajada",
      x = "Distancia (km)",
      y = "Pendiente (%)",
      fill = "Tipo de zona"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  ggsave(paste0("exportaciones/graficos/06_pendientes_", ruta$id, "_", gsub(" ", "_", ruta$nombre), ".svg"),
         p_pendiente, width = 14, height = 6, device = "svg")
  print(p_pendiente)
  
  # 6.3 Perfil de curvatura
  p_curvatura <- ggplot(perfil, aes(x = distancia_km, y = curvatura_magnitud)) +
    geom_line(color = "purple", linewidth = 0.8) +
    labs(
      title = paste("Magnitud de curvatura -", ruta$nombre),
      subtitle = "Valor alto = curva cerrada (menor radio)",
      x = "Distancia (km)",
      y = "|Curvatura| (1/m)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  ggsave(paste0("exportaciones/graficos/06_curvatura_", ruta$id, "_", gsub(" ", "_", ruta$nombre), ".svg"),
         p_curvatura, width = 14, height = 6, device = "svg")
  print(p_curvatura)
  
  # 6.4 Exportar CSV
  write.csv(perfil, paste0("exportaciones/csv/06_perfil_", ruta$id, "_", gsub(" ", "_", ruta$nombre), ".csv"), row.names = FALSE)
  write.csv(zonas, paste0("exportaciones/csv/06_zonas_", ruta$id, "_", gsub(" ", "_", ruta$nombre), ".csv"), row.names = FALSE)
  
  # 6.5 Resumen estadístico
  cat("\n--- RESUMEN:", ruta$nombre, "---\n")
  cat("  Longitud total:", round(max(perfil$distancia_km), 2), "km\n")
  cat("  Cota mínima:", round(min(perfil$cota), 1), "m\n")
  cat("  Cota máxima:", round(max(perfil$cota), 1), "m\n")
  cat("  Pendiente máxima:", round(max(perfil$pendiente, na.rm = TRUE), 2), "%\n")
  cat("  Pendiente mínima:", round(min(perfil$pendiente, na.rm = TRUE), 2), "%\n")
  
  max_curv <- max(perfil$curvatura_magnitud, na.rm = TRUE)
  if (max_curv > 0) {
    cat("  Radio mínimo:", round(1 / max_curv, 0), "m\n")
  }
  
  estaciones <- perfil %>% filter(tipo_elemento == 1 & !is.na(elemento) & elemento != "")
  cat("  Estaciones:", nrow(estaciones), "\n")
}

# 7. Generar gráficos para todas las rutas ------------------------
for (ruta in rutas) {
  generar_graficos(ruta)
}

# 8. Gráfico comparativo de perfiles (las tres rutas juntas) -----
cat("\n========================================\n")
cat("GRÁFICO COMPARATIVO DE PERFILES\n")
cat("========================================\n")

# Normalizar perfiles para comparar (misma escala en X)
perfiles_comparacion <- data.frame()

for (ruta in rutas) {
  df_temp <- ruta$perfil %>%
    mutate(
      nombre_ruta = ruta$nombre,
      distancia_normalizada = distancia_km / max(distancia_km) * 100  # Porcentaje del recorrido
    ) %>%
    select(nombre_ruta, distancia_normalizada, cota)
  
  perfiles_comparacion <- rbind(perfiles_comparacion, df_temp)
}

p_comparativa <- ggplot(perfiles_comparacion, aes(x = distancia_normalizada, y = cota, color = nombre_ruta)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Comparativa de perfiles topográficos",
    subtitle = "Distancia normalizada al 100% del recorrido",
    x = "Recorrido (%)",
    y = "Cota (msnm)",
    color = "Ruta"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("exportaciones/graficos/06_comparativa_perfiles.svg", p_comparativa, width = 12, height = 7, device = "svg")
print(p_comparativa)

# 9. Cerrar conexión -----------------------------------------------
dbDisconnect(con)

cat("\n========================================\n")
cat("✅ Script 06 completado\n")
cat("========================================\n")
cat("✅ Gráficos guardados en: exportaciones/graficos/\n")
cat("   - 06_perfil_*.svg (perfil por ruta)\n")
cat("   - 06_pendientes_*.svg (pendientes por ruta)\n")
cat("   - 06_curvatura_*.svg (curvatura por ruta)\n")
cat("   - 06_comparativa_perfiles.svg (comparativa)\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")
