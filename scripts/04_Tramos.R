# ==============================================================
# Título:     Comparativa Zonas vs Tramos
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-06
# Base datos: Vaporosas.db
# Objetivo:   Comparar si los consumos por zonas se corresponden 
#             con los consumos por tramos originales
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

# 2. Conexión ------------------------------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 3. Cargar datos -------------------------------------------------

# 3.1 Consumos por TRAMO (original, entre estaciones)
consumos_tramos <- dbGetQuery(con, "
SELECT 
    c.id_consumo,
    l.nombre as locomotora,
    c.agua_L_por_km,
    c.carbon_kg_por_km,
    c.velocidad_media_km_h,
    t.id_ruta,
    t.nombre_tramo,
    t.distancia_inicio_km,
    t.distancia_fin_km,
    (t.distancia_fin_km - t.distancia_inicio_km) as longitud_km,
    r.nombre_ruta
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Tramos t ON c.id_tramo = t.id_tramo
JOIN Rutas r ON t.id_ruta = r.id_ruta
WHERE l.tipo = 'Vapor'
")

consumos_tramos$locomotora <- trimws(consumos_tramos$locomotora)

# 3.2 Consumos por ZONA (agrupación manual)
# Necesitamos asignar cada consumo a una zona
zonas <- dbGetQuery(con, "
SELECT 
    id_zona,
    id_ruta,
    nombre_zona,
    distancia_inicio_km,
    distancia_fin_km,
    tipo
FROM Zonas
")

# Función para asignar zona
asignar_zona <- function(dist_inicio, dist_fin, zonas_ruta) {
  zonas_ruta <- zonas_ruta %>%
    mutate(
      interseccion_inicio = pmax(distancia_inicio_km, dist_inicio),
      interseccion_fin = pmin(distancia_fin_km, dist_fin),
      longitud_interseccion = pmax(0, interseccion_fin - interseccion_inicio)
    ) %>%
    filter(longitud_interseccion > 0) %>%
    arrange(desc(longitud_interseccion))
  
  if (nrow(zonas_ruta) == 0) return(NA)
  return(zonas_ruta$tipo[1])
}

# Asignar zona a cada consumo
consumos_tramos$zona <- NA

for (i in 1:nrow(consumos_tramos)) {
  zonas_ruta <- zonas %>% filter(id_ruta == consumos_tramos$id_ruta[i])
  if (nrow(zonas_ruta) > 0) {
    consumos_tramos$zona[i] <- asignar_zona(
      consumos_tramos$distancia_inicio_km[i],
      consumos_tramos$distancia_fin_km[i],
      zonas_ruta
    )
  }
}

# Eliminar consumos sin zona asignada
consumos_tramos <- consumos_tramos %>% filter(!is.na(zona))

# 4. Estadísticos por ZONA (agrupando consumos por zona y locomotora)
estadisticos_zona <- consumos_tramos %>%
  group_by(locomotora, zona, nombre_ruta) %>%
  summarise(
    n = n(),
    agua_zona = round(mean(agua_L_por_km, na.rm = TRUE), 1),
    agua_sd_zona = round(sd(agua_L_por_km, na.rm = TRUE), 1),
    carbon_zona = round(mean(carbon_kg_por_km, na.rm = TRUE), 1),
    carbon_sd_zona = round(sd(carbon_kg_por_km, na.rm = TRUE), 1),
    velocidad_zona = round(mean(velocidad_media_km_h, na.rm = TRUE), 1),
    .groups = "drop"
  )

# 5. Estadísticos por TRAMO (agrupando por tramo original)
estadisticos_tramo <- consumos_tramos %>%
  group_by(locomotora, nombre_tramo, nombre_ruta, zona) %>%
  summarise(
    n = n(),
    agua_tramo = round(mean(agua_L_por_km, na.rm = TRUE), 1),
    carbon_tramo = round(mean(carbon_kg_por_km, na.rm = TRUE), 1),
    velocidad_tramo = round(mean(velocidad_media_km_h, na.rm = TRUE), 1),
    .groups = "drop"
  )

# 6. Comparativa directa Zona vs Tramo (misma locomotora, misma zona)
# Agregamos los tramos por zona para comparar con la zona completa
comparativa_zona_tramo <- estadisticos_tramo %>%
  group_by(locomotora, zona, nombre_ruta) %>%
  summarise(
    n_tramos = n(),
    agua_tramos_promedio = round(mean(agua_tramo, na.rm = TRUE), 1),
    agua_tramos_sd = round(sd(agua_tramo, na.rm = TRUE), 1),
    carbon_tramos_promedio = round(mean(carbon_tramo, na.rm = TRUE), 1),
    carbon_tramos_sd = round(sd(carbon_tramo, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(estadisticos_zona, by = c("locomotora", "zona", "nombre_ruta")) %>%
  mutate(
    diferencia_agua = round(agua_tramos_promedio - agua_zona, 1),
    diferencia_carbon = round(carbon_tramos_promedio - carbon_zona, 1),
    pct_diferencia_agua = round((diferencia_agua / agua_zona) * 100, 1),
    pct_diferencia_carbon = round((diferencia_carbon / carbon_zona) * 100, 1),
    correspondencia = case_when(
      abs(pct_diferencia_agua) <= 5 & abs(pct_diferencia_carbon) <= 5 ~ "Excelente",
      abs(pct_diferencia_agua) <= 10 & abs(pct_diferencia_carbon) <= 10 ~ "Buena",
      abs(pct_diferencia_agua) <= 20 & abs(pct_diferencia_carbon) <= 20 ~ "Aceptable",
      TRUE ~ "Pobre"
    )
  )

# 7. Resumen por tipo de zona (comparativa agregada)
resumen_por_zona <- comparativa_zona_tramo %>%
  group_by(zona) %>%
  summarise(
    n_casos = n(),
    dif_agua_media = round(mean(diferencia_agua, na.rm = TRUE), 1),
    dif_carbon_media = round(mean(diferencia_carbon, na.rm = TRUE), 1),
    excelente = sum(correspondencia == "Excelente"),
    buena = sum(correspondencia == "Buena"),
    aceptable = sum(correspondencia == "Aceptable"),
    pobre = sum(correspondencia == "Pobre"),
    .groups = "drop"
  )

# 8. Gráficos -----------------------------------------------------

# 8.1 Correlación entre consumo Zona vs Tramo (agua)
p_correlacion_agua <- ggplot(comparativa_zona_tramo, 
                              aes(x = agua_zona, y = agua_tramos_promedio)) +
  geom_point(aes(color = zona, shape = locomotora), size = 4, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
  labs(
    title = "Correlación: Consumo de agua por Zona vs Tramo",
    subtitle = "Cada punto = una zona | Línea roja = correspondencia perfecta",
    x = "Agua Zona (L/km)",
    y = "Agua Tramo promedio (L/km)",
    color = "Tipo zona",
    shape = "Locomotora"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("exportaciones/graficos/04_correlacion_zona_tramo_agua.svg", 
       p_correlacion_agua, width = 10, height = 7, device = "svg")

# 8.2 Correlación entre consumo Zona vs Tramo (carbón)
p_correlacion_carbon <- ggplot(comparativa_zona_tramo, 
                                aes(x = carbon_zona, y = carbon_tramos_promedio)) +
  geom_point(aes(color = zona, shape = locomotora), size = 4, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
  labs(
    title = "Correlación: Consumo de carbón por Zona vs Tramo",
    subtitle = "Cada punto = una zona | Línea roja = correspondencia perfecta",
    x = "Carbón Zona (L/km)",
    y = "Carbón Tramo promedio (kg/km)",
    color = "Tipo zona",
    shape = "Locomotora"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("exportaciones/graficos/04_correlacion_zona_tramo_carbon.svg", 
       p_correlacion_carbon, width = 10, height = 7, device = "svg")

# 8.3 Gráfico de diferencias por zona
p_diferencias <- comparativa_zona_tramo %>%
  select(locomotora, zona, diferencia_agua, diferencia_carbon) %>%
  pivot_longer(cols = c(diferencia_agua, diferencia_carbon),
               names_to = "variable", values_to = "diferencia") %>%
  mutate(variable = ifelse(variable == "diferencia_agua", "Agua (L/km)", "Carbón (kg/km)"))

ggplot(p_diferencias, aes(x = zona, y = diferencia, fill = locomotora)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~variable, scales = "free_y") +
  # ==============================================================
# AÑADE ESTAS LÍNEAS (paleta de colores personalizada)
# ==============================================================
scale_fill_manual(values = c("AND240-4253" = "firebrick1",
                             "RN141-2106" = "cyan2",
                             "MZA 1701" = "darkseagreen1",
                             "MZA 1801" = "chocolate1")) +
  # ==============================================================
labs(
  title = "Diferencia: Tramo promedio vs Zona",
  subtitle = "Valor positivo = Tramo consume más que la Zona | Negativo = Tramo consume menos",
  x = "Tipo de zona",
  y = "Diferencia",
  fill = "Locomotora"
) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/04_diferencias_zona_tramo.svg", width = 12, height = 7, device = "svg")

# 8.4 Mapa de calor de correspondencia
p_correspondencia <- comparativa_zona_tramo %>%
  group_by(zona, correspondencia) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = zona, y = correspondencia, fill = n)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = n), size = 5) +
  scale_fill_gradient(low = "steelblue", high = "firebrick", name = "Número de casos") +
  labs(
    title = "Calidad de correspondencia Zona vs Tramo",
    subtitle = "Excelente = diferencia <5% | Buena = 5-10% | Aceptable = 10-20% | Pobre >20%",
    x = "Tipo de zona",
    y = "Correspondencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("exportaciones/graficos/04_correspondencia_zona_tramo.svg", 
       p_correspondencia, width = 8, height = 5, device = "svg")

# 9. Exportar CSV -------------------------------------------------
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

write.csv(estadisticos_zona, "exportaciones/csv/04_estadisticos_por_zona.csv", row.names = FALSE)
write.csv(estadisticos_tramo, "exportaciones/csv/04_estadisticos_por_tramo.csv", row.names = FALSE)
write.csv(comparativa_zona_tramo, "exportaciones/csv/04_comparativa_zona_tramo.csv", row.names = FALSE)
write.csv(resumen_por_zona, "exportaciones/csv/04_resumen_correspondencia.csv", row.names = FALSE)

# 10. Mostrar resultados ------------------------------------------
cat("\n\n========================================\n")
cat("COMPARATIVA ZONAS vs TRAMOS\n")
cat("========================================\n")

cat("\n--- CORRESPONDENCIA POR TIPO DE ZONA ---\n")
print(resumen_por_zona)

cat("\n\n--- CASOS CON DIFERENCIAS SIGNIFICATIVAS ---\n")
diferencias_significativas <- comparativa_zona_tramo %>%
  filter(abs(pct_diferencia_agua) > 20 | abs(pct_diferencia_carbon) > 20) %>%
  select(locomotora, zona, nombre_ruta, agua_zona, agua_tramos_promedio, 
         carbon_zona, carbon_tramos_promedio, pct_diferencia_agua, pct_diferencia_carbon)

print(diferencias_significativas)

# 11. Resumen ejecutivo -------------------------------------------
cat("\n\n========================================\n")
cat("CONCLUSIONES\n")
cat("========================================\n")

# Calcular correlación global
cor_agua <- cor(comparativa_zona_tramo$agua_zona, 
                comparativa_zona_tramo$agua_tramos_promedio, 
                use = "complete.obs")
cor_carbon <- cor(comparativa_zona_tramo$carbon_zona, 
                  comparativa_zona_tramo$carbon_tramos_promedio, 
                  use = "complete.obs")

cat("\nCorrelación Zona vs Tramo:\n")
cat("  • Agua: r =", round(cor_agua, 3), "\n")
cat("  • Carbón: r =", round(cor_carbon, 3), "\n")

cat("\nInterpretación:\n")
if (cor_agua > 0.8 & cor_carbon > 0.8) {
  cat("  ✅ CORRESPONDENCIA EXCELENTE. Las zonas representan bien los tramos.\n")
} else if (cor_agua > 0.6 & cor_carbon > 0.6) {
  cat("  ✅ CORRESPONDENCIA BUENA. Las zonas capturan tendencias generales.\n")
} else if (cor_agua > 0.4 & cor_carbon > 0.4) {
  cat("  ⚠️ CORRESPONDENCIA MODERADA. Discrepancias en algunos casos.\n")
} else {
  cat("  ❌ CORRESPONDENCIA POBRE. Zonas y tramos no son equivalentes.\n")
}

cat("\n✅ Análisis comparativo Zonas vs Tramos completado\n")
cat("✅ Gráficos guardados en: exportaciones/graficos/\n")
cat("   - correlacion_zona_tramo_agua.svg\n")
cat("   - correlacion_zona_tramo_carbon.svg\n")
cat("   - diferencias_zona_tramo.svg\n")
cat("   - correspondencia_zona_tramo.svg\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")

