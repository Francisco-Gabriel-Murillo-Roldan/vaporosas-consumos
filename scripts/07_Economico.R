# ==============================================================
# Título:     Análisis económico por locomotora (3 rutas)
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-11
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

# 2. Conexión ------------------------------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 3. Cargar precios ------------------------------------------------
precios <- dbGetQuery(con, "
SELECT recurso, precio_unidad 
FROM Precios 
WHERE recurso IN ('agua', 'carbon')
")

precio_agua <- precios$precio_unidad[precios$recurso == "agua"]
precio_carbon <- precios$precio_unidad[precios$recurso == "carbon"]

cat("\n========================================\n")
cat("PRECIOS UNITARIOS\n")
cat("========================================\n")
cat("Agua: ", precio_agua, "€/L\n")
cat("Carbón: ", precio_carbon, "€/kg\n")

# 4. Cargar consumos medios por locomotora y ruta -----------------
consumos_por_ruta <- dbGetQuery(con, "
SELECT 
    l.nombre as locomotora,
    r.nombre_ruta,
    r.id_ruta,
    ROUND(AVG(c.agua_L_por_km), 1) as agua_media_L_km,
    ROUND(AVG(c.carbon_kg_por_km), 2) as carbon_media_kg_km,
    ROUND(AVG(c.velocidad_media_km_h), 1) as velocidad_media_km_h
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Tramos t ON c.id_tramo = t.id_tramo
JOIN Rutas r ON t.id_ruta = r.id_ruta
WHERE l.tipo = 'Vapor'
GROUP BY l.id_locomotora, r.id_ruta
")

consumos_por_ruta$locomotora <- trimws(consumos_por_ruta$locomotora)

# 5. Longitudes de las rutas (desde Perfiles_Simulador) - CORREGIDO
longitudes <- dbGetQuery(con, "
SELECT 
    r.id_ruta,
    r.nombre_ruta,
    ROUND(MAX(ps.distancia_acumulada_m / 1000), 4) as longitud_km
FROM Rutas r
JOIN Perfiles_Simulador ps ON r.id_ruta = ps.id_ruta
GROUP BY r.id_ruta, r.nombre_ruta
")

cat("\nLongitudes obtenidas:\n")
print(longitudes)

# 6. Unir consumos con longitudes ---------------------------------
datos_costes <- consumos_por_ruta %>%
  inner_join(longitudes, by = c("id_ruta", "nombre_ruta"))

# Verificar que no hay NA
if(any(is.na(datos_costes$longitud_km))) {
  cat("\n⚠️ ADVERTENCIA: Hay rutas sin longitud asignada\n")
  print(datos_costes %>% filter(is.na(longitud_km)) %>% select(nombre_ruta))
}

# 7. Calcular costes por km y por trayecto ------------------------
datos_costes <- datos_costes %>%
  mutate(
    # Costes por km
    coste_agua_km = round(agua_media_L_km * precio_agua, 4),
    coste_carbon_km = round(carbon_media_kg_km * precio_carbon, 4),
    coste_total_km = round(coste_agua_km + coste_carbon_km, 4),
    # Costes por trayecto completo
    coste_agua_trayecto = round(coste_agua_km * longitud_km, 2),
    coste_carbon_trayecto = round(coste_carbon_km * longitud_km, 2),
    coste_total_trayecto = round(coste_total_km * longitud_km, 2),
    # Porcentajes
    pct_agua = round(coste_agua_km / coste_total_km * 100, 1),
    pct_carbon = round(coste_carbon_km / coste_total_km * 100, 1)
  )

# 8. Mostrar resultados por ruta ----------------------------------
cat("\n\n========================================\n")
cat("COSTE POR TRAYECTO COMPLETO\n")
cat("========================================\n")
print(datos_costes %>% 
        select(locomotora, nombre_ruta, longitud_km, coste_total_trayecto, 
               coste_agua_trayecto, coste_carbon_trayecto) %>%
        arrange(nombre_ruta, coste_total_trayecto))

# 9. Ranking económico por ruta ----------------------------------
cat("\n\n========================================\n")
cat("RANKING POR RUTA (más barata a más cara)\n")
cat("========================================\n")

for (ruta in unique(datos_costes$nombre_ruta)) {
  cat("\n---", ruta, "---\n")
  ranking_ruta <- datos_costes %>%
    filter(nombre_ruta == ruta) %>%
    arrange(coste_total_trayecto)
  
  for (i in 1:nrow(ranking_ruta)) {
    cat(i, "º:", ranking_ruta$locomotora[i], 
        "-", round(ranking_ruta$coste_total_trayecto[i], 2), "€\n")
  }
}

# 10. Tabla de ganadores por ruta ---------------------------------
ganadores_ruta <- datos_costes %>%
  group_by(nombre_ruta) %>%
  filter(coste_total_trayecto == min(coste_total_trayecto)) %>%
  select(nombre_ruta, locomotora, coste_total_trayecto, longitud_km)

cat("\n\n========================================\n")
cat("GANADORES POR RUTA (más económica)\n")
cat("========================================\n")
print(ganadores_ruta)

# 11. Crear carpetas ----------------------------------------------
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 12. Gráficos ----------------------------------------------------

# 12.1 Coste por trayecto (barras agrupadas por ruta)
p_trayecto <- ggplot(datos_costes, aes(x = nombre_ruta, y = coste_total_trayecto, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(coste_total_trayecto, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Coste por trayecto completo",
    subtitle = paste0("Agua: ", precio_agua, " €/L | Carbón: ", precio_carbon, " €/kg"),
    x = "Ruta",
    y = "Coste (€)",
    fill = "Locomotora"
  ) +
  scale_fill_manual(values = c("AND240-4253" = "firebrick1",
                               "RN141-2106" = "cyan2",
                               "MZA 1701" = "darkseagreen1",
                               "MZA 1801" = "chocolate1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/07_coste_por_trayecto.svg", p_trayecto, width = 10, height = 6, device = "svg")
print(p_trayecto)

# 12.2 Coste por km (barras agrupadas por ruta)
p_km <- ggplot(datos_costes, aes(x = nombre_ruta, y = coste_total_km, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(coste_total_km, 4)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Coste por kilómetro",
    subtitle = paste0("Agua: ", precio_agua, " €/L | Carbón: ", precio_carbon, " €/kg"),
    x = "Ruta",
    y = "Coste (€/km)",
    fill = "Locomotora"
  ) +
  scale_fill_manual(values = c("AND240-4253" = "firebrick1",
                               "RN141-2106" = "cyan2",
                               "MZA 1701" = "darkseagreen1",
                               "MZA 1801" = "chocolate1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/07_coste_por_km.svg", p_km, width = 10, height = 6, device = "svg")
print(p_km)

# 12.3 Mapa de calor de costes por ruta
matriz_costes <- datos_costes %>%
  select(locomotora, nombre_ruta, coste_total_trayecto) %>%
  pivot_wider(names_from = nombre_ruta, values_from = coste_total_trayecto)

p_heatmap <- datos_costes %>%
  ggplot(aes(x = nombre_ruta, y = locomotora, fill = coste_total_trayecto)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(coste_total_trayecto, 0)), size = 4) +
  scale_fill_gradient(low = "steelblue1", high = "firebrick3", name = "Coste (€)") +
  labs(
    title = "Coste por trayecto (€) - comparativa por ruta",
    x = "Ruta",
    y = "Locomotora"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/07_mapa_calor_costes.svg", p_heatmap, width = 8, height = 5, device = "svg")
print(p_heatmap)

# 13. Exportar CSV ------------------------------------------------
write.csv(datos_costes, "exportaciones/csv/07_analisis_economico_3_rutas.csv", row.names = FALSE)
write.csv(ganadores_ruta, "exportaciones/csv/07_ganadores_por_ruta.csv", row.names = FALSE)

# 14. Tabla para el informe (formato ancho)
tabla_informe <- datos_costes %>%
  select(locomotora, nombre_ruta, coste_total_trayecto) %>%
  pivot_wider(names_from = nombre_ruta, values_from = coste_total_trayecto, values_fill = NA)

cat("\n\n========================================\n")
cat("TABLA PARA EL INFORME (coste por trayecto en €)\n")
cat("========================================\n")
print(tabla_informe)

# 15. Interpretación final ----------------------------------------
cat("\n\n========================================\n")
cat("CONCLUSIONES ECONÓMICAS\n")
cat("========================================\n")

cat("\n1. LARGA DISTANCIA (Vallarizasoria, 152 km):")
cat("\n   → Locomotora más económica:", ganadores_ruta$locomotora[ganadores_ruta$nombre_ruta == "Vallarizasoria"])

cat("\n\n2. SERVICIO ÓMNIBUS/REGIONAL (Eje Central, 53 km):")
cat("\n   → Locomotora más económica:", ganadores_ruta$locomotora[ganadores_ruta$nombre_ruta == "Eje central andaluces"])

cat("\n\n3. SERVICIO EXPERIMENTAL/RAMPA (Test Route, 8 km):")
cat("\n   → Locomotora más económica:", ganadores_ruta$locomotora[ganadores_ruta$nombre_ruta == "CTN test route"])

cat("\n\n✅ Script 07 corregido y completado\n")
cat("✅ Gráficos guardados en: exportaciones/graficos/\n")
cat("   - 07_coste_por_trayecto.svg\n")
cat("   - 07_coste_por_km.svg\n")
cat("   - 07_mapa_calor_costes.svg\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")
cat("   - 07_analisis_economico_3_rutas.csv\n")
cat("   - 07_ganadores_por_ruta.csv\n")
