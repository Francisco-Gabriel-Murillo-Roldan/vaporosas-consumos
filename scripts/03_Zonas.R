# ==============================================================
# Título:     Análisis por Zonas (con Perfiles_Simulador)
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

# 2. Conexión ------------------------------------------------------
con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 3. Cargar datos -------------------------------------------------

# 3.1 Consumos con información de tramos
consumos <- dbGetQuery(con, "
SELECT 
    c.id_consumo,
    l.nombre as locomotora,
    c.agua_L_por_km,
    c.carbon_kg_por_km,
    c.velocidad_media_km_h,
    t.id_ruta,
    t.id_tramo,
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

consumos$locomotora <- trimws(consumos$locomotora)
consumos$nombre_ruta <- trimws(consumos$nombre_ruta)

# 3.2 Zonas
zonas <- dbGetQuery(con, "
SELECT 
    id_zona,
    id_ruta,
    nombre_zona,
    distancia_inicio_km,
    distancia_fin_km,
    tipo,
    observaciones
FROM Zonas
")

# 3.3 Perfil del simulador (con cotas, pendientes y curvatura)
perfil_sim <- dbGetQuery(con, "
SELECT 
    id_ruta,
    distancia_acumulada_m / 1000 as distancia_km,
    altura_msnm as cota,
    pendiente_porcentaje as pendiente,
    curvatura,
    elemento_via_texto as elemento,
    elemento_via_tipo as tipo_elemento
FROM Perfiles_Simulador
ORDER BY id_ruta, distancia_acumulada_m
")

# 4. Función para asignar zona a cada consumo --------------------
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

# 5. Asignar zona a cada consumo ---------------------------------
consumos_con_zona <- consumos

for (i in 1:nrow(consumos_con_zona)) {
  zonas_ruta <- zonas %>% filter(id_ruta == consumos_con_zona$id_ruta[i])
  if (nrow(zonas_ruta) > 0) {
    consumos_con_zona$tipo_terreno[i] <- asignar_zona(
      consumos_con_zona$distancia_inicio_km[i],
      consumos_con_zona$distancia_fin_km[i],
      zonas_ruta
    )
  } else {
    consumos_con_zona$tipo_terreno[i] <- NA
  }
  if (i %% 50 == 0) cat("Procesando consumos:", i, "/", nrow(consumos_con_zona), "\n")
}

consumos_con_zona <- consumos_con_zona %>% filter(!is.na(tipo_terreno))
cat("\n✅ Consumos con zona asignada:", nrow(consumos_con_zona), "\n")

# 6. Estadísticos por tipo de terreno y locomotora ---------------
estadisticos_terreno <- consumos_con_zona %>%
  group_by(locomotora, tipo_terreno, nombre_ruta) %>%
  summarise(
    n = n(),
    agua_media = round(mean(agua_L_por_km, na.rm = TRUE), 1),
    agua_sd = round(sd(agua_L_por_km, na.rm = TRUE), 1),
    carbon_media = round(mean(carbon_kg_por_km, na.rm = TRUE), 1),
    carbon_sd = round(sd(carbon_kg_por_km, na.rm = TRUE), 1),
    velocidad_media = round(mean(velocidad_media_km_h, na.rm = TRUE), 1),
    velocidad_sd = round(sd(velocidad_media_km_h, na.rm = TRUE), 1),
    .groups = "drop"
  )

# 7. Resumen por tipo de terreno (todas las rutas juntas) --------
resumen_terreno <- consumos_con_zona %>%
  group_by(locomotora, tipo_terreno) %>%
  summarise(
    n = n(),
    agua_media = round(mean(agua_L_por_km, na.rm = TRUE), 1),
    carbon_media = round(mean(carbon_kg_por_km, na.rm = TRUE), 1),
    velocidad_media = round(mean(velocidad_media_km_h, na.rm = TRUE), 1),
    .groups = "drop"
  )

# 8. Estadísticos de pendiente por zona (desde Perfiles_Simulador)
pendientes_por_zona <- data.frame()

for (i in 1:nrow(zonas)) {
  zona <- zonas[i, ]
  
  perfil_zona <- perfil_sim %>%
    filter(id_ruta == zona$id_ruta,
           distancia_km >= zona$distancia_inicio_km,
           distancia_km <= zona$distancia_fin_km)
  
  if (nrow(perfil_zona) > 0) {
    pendientes_por_zona <- rbind(pendientes_por_zona, data.frame(
      id_zona = zona$id_zona,
      nombre_zona = zona$nombre_zona,
      tipo = zona$tipo,
      longitud_km = round(zona$distancia_fin_km - zona$distancia_inicio_km, 2),
      pendiente_media = round(mean(perfil_zona$pendiente, na.rm = TRUE), 2),
      pendiente_max = round(max(perfil_zona$pendiente, na.rm = TRUE), 2),
      pendiente_min = round(min(perfil_zona$pendiente, na.rm = TRUE), 2),
      curvatura_media = round(mean(abs(perfil_zona$curvatura), na.rm = TRUE), 6),
      n_puntos = nrow(perfil_zona)
    ))
  }
}

# 9. Crear carpetas -----------------------------------------------
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 10. Gráficos ----------------------------------------------------

# 10.1 Consumo de agua por tipo de terreno
p_agua_terreno <- ggplot(resumen_terreno, 
                         aes(x = tipo_terreno, y = agua_media, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(agua_media, 0)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Consumo de agua por tipo de terreno",
    subtitle = "Datos desde Perfiles_Simulador (Open Rails)",
    x = "Tipo de terreno",
    y = "Agua (L/km)",
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

ggsave("exportaciones/graficos/03_agua_por_terreno.svg", p_agua_terreno, width = 10, height = 6, device = "svg")
print(p_agua_terreno)

# 10.2 Consumo de carbón por tipo de terreno
p_carbon_terreno <- ggplot(resumen_terreno, 
                           aes(x = tipo_terreno, y = carbon_media, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(carbon_media, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Consumo de carbón por tipo de terreno",
    subtitle = "Datos desde Perfiles_Simulador (Open Rails)",
    x = "Tipo de terreno",
    y = "Carbón (kg/km)",
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

ggsave("exportaciones/graficos/03_carbon_por_terreno.svg", p_carbon_terreno, width = 10, height = 6, device = "svg")
print(p_carbon_terreno)

# 10.3 Velocidad por tipo de terreno
p_vel_terreno <- ggplot(resumen_terreno, 
                        aes(x = tipo_terreno, y = velocidad_media, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(velocidad_media, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Velocidad media por tipo de terreno",
    subtitle = "Datos desde Perfiles_Simulador (Open Rails)",
    x = "Tipo de terreno",
    y = "Velocidad (km/h)",
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

ggsave("exportaciones/graficos/03_velocidad_por_terreno.svg", p_vel_terreno, width = 10, height = 6, device = "svg")
print(p_vel_terreno)

# 11. Exportar CSV ------------------------------------------------
write.csv(estadisticos_terreno, "exportaciones/csv/03_estadisticos_por_terreno.csv", row.names = FALSE)
write.csv(resumen_terreno, "exportaciones/csv/03_resumen_terreno.csv", row.names = FALSE)
write.csv(pendientes_por_zona, "exportaciones/csv/03_pendientes_por_zona.csv", row.names = FALSE)

# 12. Mostrar resultados ------------------------------------------
cat("\n\n========================================\n")
cat("RESUMEN POR TIPO DE TERRENO\n")
cat("========================================\n")
print(resumen_terreno)

cat("\n\n========================================\n")
cat("PENDIENTES POR ZONA (desde Perfiles_Simulador)\n")
cat("========================================\n")
print(pendientes_por_zona)

# 13. Resumen ejecutivo -------------------------------------------
cat("\n\n========================================\n")
cat("CONCLUSIONES DEL ANÁLISIS POR TERRENO\n")
cat("========================================\n")

# Mejor locomotora en ascenso
ascenso <- resumen_terreno %>% filter(tipo_terreno == "Ascenso")
mejor_ascenso <- ascenso %>% filter(carbon_media == min(carbon_media))
cat("Mejor en ascenso (carbón):", mejor_ascenso$locomotora, 
    "con", mejor_ascenso$carbon_media, "kg/km\n")

# Mejor locomotora en descenso
descenso <- resumen_terreno %>% filter(tipo_terreno == "Descenso")
mejor_descenso <- descenso %>% filter(carbon_media == min(carbon_media))
cat("Mejor en descenso (carbón):", mejor_descenso$locomotora, 
    "con", mejor_descenso$carbon_media, "kg/km\n")

cat("\n✅ Análisis por terreno completado\n")
cat("✅ Gráficos guardados en: exportaciones/graficos/\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")

