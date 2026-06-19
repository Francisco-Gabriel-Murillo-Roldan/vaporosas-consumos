# ==============================================================
# Título:     Análisis económico por zonas
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-11
# Base datos: Vaporosas.db
# ==============================================================

rm(list = ls())
options(scipen = 999)

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

con <- dbConnect(SQLite(), "Vaporosas.db")
on.exit(dbDisconnect(con), add = TRUE)

# 1. Precios
precios <- dbGetQuery(con, "SELECT recurso, precio_unidad FROM Precios")
precio_agua <- precios$precio_unidad[precios$recurso == "agua"]
precio_carbon <- precios$precio_unidad[precios$recurso == "carbon"]

cat("PRECIOS:\n")
cat("  Agua:", precio_agua, "€/L\n")
cat("  Carbón:", precio_carbon, "€/kg\n\n")

# 2. Cargar consumos por zona (desde Script 03)
# Primero, obtener los datos de consumo por zona desde la DB
consumos_zona <- dbGetQuery(con, "
SELECT 
    l.nombre as locomotora,
    z.tipo as zona,
    ROUND(AVG(c.agua_L_por_km), 1) as agua_media,
    ROUND(AVG(c.carbon_kg_por_km), 2) as carbon_media,
    COUNT(*) as n_muestras
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Tramos t ON c.id_tramo = t.id_tramo
JOIN Zonas z ON z.id_ruta = t.id_ruta 
    AND t.distancia_inicio_km >= z.distancia_inicio_km 
    AND t.distancia_fin_km <= z.distancia_fin_km
WHERE l.tipo = 'Vapor'
GROUP BY l.nombre, z.tipo
")

consumos_zona$locomotora <- trimws(consumos_zona$locomotora)

# 3. Calcular costes por zona
costes_zona <- consumos_zona %>%
  mutate(
    coste_agua_km = round(agua_media * precio_agua, 4),
    coste_carbon_km = round(carbon_media * precio_carbon, 4),
    coste_total_km = round(coste_agua_km + coste_carbon_km, 4),
    pct_agua = round(coste_agua_km / coste_total_km * 100, 1),
    pct_carbon = round(coste_carbon_km / coste_total_km * 100, 1)
  )

cat("========================================\n")
cat("COSTE POR ZONA (€/km)\n")
cat("========================================\n")
print(costes_zona %>% 
        select(locomotora, zona, coste_total_km, coste_agua_km, coste_carbon_km, n_muestras))

# 4. Ratio ascenso/descenso (lo que cuesta subir vs bajar)
ratio_zonas <- costes_zona %>%
  select(locomotora, zona, coste_total_km) %>%
  pivot_wider(names_from = zona, values_from = coste_total_km) %>%
  mutate(
    ratio_ascenso_descenso = round(Ascenso / Descenso, 2),
    ratio_ascenso_ondulado = round(Ascenso / Ondulado, 2),
    ratio_descenso_ondulado = round(Descenso / Ondulado, 2)
  )

cat("\n\n========================================\n")
cat("RATIOS ECONÓMICOS POR ZONA\n")
cat("========================================\n")
cat("Ratio > 1 = más caro que la referencia\n")
print(ratio_zonas)

# 5. Ahorro en descenso (respecto al ondulado)
ahorro_descenso <- costes_zona %>%
  filter(zona %in% c("Descenso", "Ondulado")) %>%
  select(locomotora, zona, coste_total_km) %>%
  pivot_wider(names_from = zona, values_from = coste_total_km) %>%
  mutate(
    ahorro_descenso = round((Ondulado - Descenso) / Ondulado * 100, 1)
  )

cat("\n\n========================================\n")
cat("AHORRO EN DESCENSO (% respecto a ondulado)\n")
cat("========================================\n")
print(ahorro_descenso %>% select(locomotora, Descenso, Ondulado, ahorro_descenso))

# 6. Sobre-coste de ascenso (respecto al ondulado)
sobrecoste_ascenso <- costes_zona %>%
  filter(zona %in% c("Ascenso", "Ondulado")) %>%
  select(locomotora, zona, coste_total_km) %>%
  pivot_wider(names_from = zona, values_from = coste_total_km) %>%
  mutate(
    sobrecoste_ascenso = round((Ascenso - Ondulado) / Ondulado * 100, 1)
  )

cat("\n\n========================================\n")
cat("SOBRECOSTE EN ASCENSO (% respecto a ondulado)\n")
cat("========================================\n")
print(sobrecoste_ascenso %>% select(locomotora, Ascenso, Ondulado, sobrecoste_ascenso))

# 7. Coste por kilómetro de rampa (pendiente específica)
# Para la rampa del 2.6% en Vallarizasoria
rampa_26 <- costes_zona %>%
  filter(zona == "Ascenso") %>%
  select(locomotora, coste_total_km) %>%
  mutate(
    pendiente = "2.6%",
    coste_por_km_rampa = coste_total_km
  )

cat("\n\n========================================\n")
cat("COSTE EN RAMPA DEL 2.6% (€/km)\n")
cat("========================================\n")
print(rampa_26 %>% select(locomotora, pendiente, coste_por_km_rampa))

# 8. Crear gráficos
if (!dir.exists("exportaciones/graficos")) dir.create("exportaciones/graficos", recursive = TRUE)
if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

# 8.1 Coste por zona (barras agrupadas)
p_zonas <- ggplot(costes_zona, aes(x = zona, y = coste_total_km, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(coste_total_km, 3)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Coste por tipo de terreno",
    subtitle = paste0("Agua: ", precio_agua, " €/L | Carbón: ", precio_carbon, " €/kg"),
    x = "Tipo de terreno",
    y = "Coste (€/km)",
    fill = "Locomotora"
  ) +
  scale_fill_manual(values = c("AND240-4253" = "firebrick1",
                               "RN141-2106" = "cyan2",
                               "MZA 1701" = "chocolate1",
                               "MZA 1801" = "darkseagreen1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/08_coste_por_zona.svg", p_zonas, width = 10, height = 6, device = "svg")
print(p_zonas)

# 8.2 Ratio ascenso/descenso (mapa de calor)
p_ratios <- ratio_zonas %>%
  select(locomotora, ratio_ascenso_descenso, ratio_ascenso_ondulado) %>%
  pivot_longer(cols = -locomotora, names_to = "ratio", values_to = "valor") %>%
  ggplot(aes(x = ratio, y = locomotora, fill = valor)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(valor, 2)), size = 4) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", 
                       midpoint = 1, name = "Ratio") +
  labs(
    title = "Ratios económicos por locomotora",
    subtitle = "Ascenso/Descenso: >1 = subir cuesta más del doble que bajar",
    x = "",
    y = "Locomotora"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("exportaciones/graficos/08_ratios_economicos.svg", p_ratios, width = 8, height = 5, device = "svg")
print(p_ratios)

# 9. Exportar CSV
write.csv(costes_zona, "exportaciones/csv/08_coste_por_zona.csv", row.names = FALSE)
write.csv(ratio_zonas, "exportaciones/csv/08_ratios_economicos.csv", row.names = FALSE)

# 10. Conclusiones
cat("\n\n========================================\n")
cat("CONCLUSIONES ECONÓMICAS POR ZONA\n")
cat("========================================\n")

# Locomotora más barata en ascenso
mejor_ascenso <- costes_zona %>% filter(zona == "Ascenso") %>% arrange(coste_total_km) %>% slice(1)
cat("\n🚂 LOCOMOTORA MÁS BARATA EN ASCENSO (subida):", mejor_ascenso$locomotora, 
    "-", mejor_ascenso$coste_total_km, "€/km\n")

# Locomotora más barata en descenso
mejor_descenso <- costes_zona %>% filter(zona == "Descenso") %>% arrange(coste_total_km) %>% slice(1)
cat("🚂 LOCOMOTORA MÁS BARATA EN DESCENSO (bajada):", mejor_descenso$locomotora, 
    "-", mejor_descenso$coste_total_km, "€/km\n")

# Locomotora que más ahorra en descenso
mejor_ahorro <- ahorro_descenso %>% arrange(desc(ahorro_descenso)) %>% slice(1)
cat("🚂 LOCOMOTORA QUE MÁS AHORRA EN DESCENSO:", mejor_ahorro$locomotora, 
    "- ahorra un", mejor_ahorro$ahorro_descenso, "% respecto al ondulado\n")

# Locomotora que menos penaliza en ascenso
menor_penalizacion <- sobrecoste_ascenso %>% arrange(sobrecoste_ascenso) %>% slice(1)
cat("🚂 LOCOMOTORA QUE MENOS PENALIZA EN ASCENSO:", menor_penalizacion$locomotora, 
    "- subir cuesta un", menor_penalizacion$sobrecoste_ascenso, "% más que el ondulado\n")

cat("\n✅ Script 08 completado\n")
