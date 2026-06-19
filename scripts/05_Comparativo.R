# ==============================================================
# Título:     Análisis comparativo de locomotoras
# Autor:      Francisco Gabriel Murillo Roldán
# Fecha:      2026-06-03
# Base datos: estadisticos_completos.csv
# ==============================================================

# 0. Configuración inicial -----------------------------------------
rm(list = ls())
options(scipen = 999)

# 1. Librerías -----------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# 2. Cargar datos --------------------------------------------------
df <- read.csv("exportaciones/csv/01_estadisticos_completos.csv", stringsAsFactors = FALSE)

# Limpiar nombres de locomotoras
df$locomotora <- trimws(df$locomotora)

# 3. Función para identificar ganadores/perdedores ----------------
identificar_ranking <- function(df, variable_interes, columna, menor_es_mejor = TRUE) {
  
  tmp <- df %>% filter(variable == variable_interes)
  
  if (menor_es_mejor) {
    tmp <- tmp %>% arrange(!!sym(columna))
    mejor <- tmp$locomotora[1]
    peor <- tmp$locomotora[nrow(tmp)]
    diferencia <- abs(tmp[[columna]][nrow(tmp)] - tmp[[columna]][1])
    porcentaje <- (diferencia / tmp[[columna]][1]) * 100
    interpretacion <- "menor es mejor"
  } else {
    tmp <- tmp %>% arrange(desc(!!sym(columna)))
    mejor <- tmp$locomotora[1]
    peor <- tmp$locomotora[nrow(tmp)]
    diferencia <- abs(tmp[[columna]][1] - tmp[[columna]][nrow(tmp)])
    porcentaje <- (diferencia / tmp[[columna]][nrow(tmp)]) * 100
    interpretacion <- "mayor es mejor"
  }
  
  return(data.frame(
    variable = variable_interes,
    metrica = columna,
    mejor_locomotora = mejor,
    mejor_valor = round(tmp[[columna]][1], 2),
    peor_locomotora = peor,
    peor_valor = round(tmp[[columna]][nrow(tmp)], 2),
    diferencia_abs = round(diferencia, 2),
    diferencia_porcentaje = round(porcentaje, 1),
    interpretacion = interpretacion
  ))
}

# 4. Generar ranking de eficiencia ---------------------------------

cat("\n========================================\n")
cat("RANKING DE EFICIENCIA\n")
cat("========================================\n")

# Agua (menor es mejor)
ranking_agua <- identificar_ranking(df, "Agua (L/km)", "media", menor_es_mejor = TRUE)
print(ranking_agua)

# Carbón (menor es mejor)
ranking_carbon <- identificar_ranking(df, "Carbón (kg/km)", "media", menor_es_mejor = TRUE)
print(ranking_carbon)

# Velocidad (mayor es mejor)
ranking_velocidad <- identificar_ranking(df, "Velocidad (km/h)", "media", menor_es_mejor = FALSE)
print(ranking_velocidad)

# Estabilidad agua (menor CV = más estable)
ranking_estabilidad_agua <- identificar_ranking(df, "Agua (L/km)", "cv_porcentaje", menor_es_mejor = TRUE)
print(ranking_estabilidad_agua)

# Estabilidad carbón (menor CV = más estable)
ranking_estabilidad_carbon <- identificar_ranking(df, "Carbón (kg/km)", "cv_porcentaje", menor_es_mejor = TRUE)
print(ranking_estabilidad_carbon)

# 5. Tabla resumen por locomotora ---------------------------------

cat("\n\n========================================\n")
cat("PERFIL COMPLETO POR LOCOMOTORA\n")
cat("========================================\n")

perfil_locomotoras <- df %>%
  select(locomotora, variable, media, mediana, cv_porcentaje, minimo, maximo, skewness) %>%
  pivot_wider(
    names_from = variable,
    values_from = c(media, mediana, cv_porcentaje, minimo, maximo, skewness)
  )

# Renombrar columnas para claridad
names(perfil_locomotoras) <- c(
  "locomotora",
  "agua_media", "carbon_media", "velocidad_media",
  "agua_mediana", "carbon_mediana", "velocidad_mediana",
  "agua_cv", "carbon_cv", "velocidad_cv",
  "agua_min", "carbon_min", "velocidad_min",
  "agua_max", "carbon_max", "velocidad_max",
  "agua_skewness", "carbon_skewness", "velocidad_skewness"
)

print(perfil_locomotoras)

# 6. Interpretación de asimetría (skewness) -----------------------

cat("\n\n========================================\n")
cat("INTERPRETACIÓN DE ASIMETRÍA (Skewness)\n")
cat("========================================\n")
cat("Skewness > 0: Cola derecha larga (valores extremadamente altos)\n")
cat("Skewness < 0: Cola izquierda larga (valores extremadamente bajos)\n")
cat("Skewness ≈ 0: Distribución simétrica\n\n")

for (i in 1:nrow(perfil_locomotoras)) {
  loc <- perfil_locomotoras$locomotora[i]
  
  cat(loc, ":\n")
  
  if (perfil_locomotoras$agua_skewness[i] > 0.5) {
    cat("  - Agua: Asimetría positiva (", perfil_locomotoras$agua_skewness[i], 
        ") → algunos tramos con consumo extremadamente alto\n", sep = "")
  } else if (perfil_locomotoras$agua_skewness[i] < -0.5) {
    cat("  - Agua: Asimetría negativa (", perfil_locomotoras$agua_skewness[i], 
        ") → algunos tramos con consumo extremadamente bajo\n", sep = "")
  } else {
    cat("  - Agua: Distribución simétrica (", perfil_locomotoras$agua_skewness[i], ")\n", sep = "")
  }
  
  if (perfil_locomotoras$carbon_skewness[i] > 0.5) {
    cat("  - Carbón: Asimetría positiva (", perfil_locomotoras$carbon_skewness[i], 
        ") → algunos tramos con consumo extremadamente alto\n", sep = "")
  } else if (perfil_locomotoras$carbon_skewness[i] < -0.5) {
    cat("  - Carbón: Asimetría negativa (", perfil_locomotoras$carbon_skewness[i], 
        ") → algunos tramos con consumo extremadamente bajo\n", sep = "")
  } else {
    cat("  - Carbón: Distribución simétrica (", perfil_locomotoras$carbon_skewness[i], ")\n", sep = "")
  }
  cat("\n")
}

# 7. Tabla de ganadores por categoría -----------------------------

categorias <- data.frame(
  categoria = c("Eficiencia agua", "Eficiencia carbón", "Velocidad", 
                "Estabilidad agua", "Estabilidad carbón", "Menor consumo máximo agua",
                "Menor consumo máximo carbón"),
  ganadora = c(
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$agua_media)],
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$carbon_media)],
    perfil_locomotoras$locomotora[which.max(perfil_locomotoras$velocidad_media)],
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$agua_cv)],
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$carbon_cv)],
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$agua_max)],
    perfil_locomotoras$locomotora[which.min(perfil_locomotoras$carbon_max)]
  ),
  valor = c(
    round(min(perfil_locomotoras$agua_media), 1),
    round(min(perfil_locomotoras$carbon_media), 1),
    round(max(perfil_locomotoras$velocidad_media), 1),
    round(min(perfil_locomotoras$agua_cv), 1),
    round(min(perfil_locomotoras$carbon_cv), 1),
    round(min(perfil_locomotoras$agua_max), 1),
    round(min(perfil_locomotoras$carbon_max), 1)
  ),
  unidad = c("L/km", "kg/km", "km/h", "%", "%", "L/km", "kg/km")
)

cat("\n\n========================================\n")
cat("GANADORES POR CATEGORÍA\n")
cat("========================================\n")
print(categorias)

# 8. Gráfico de radar comparativo ---------------------------------

# Normalizar datos para radar (0-1)
radar_data <- perfil_locomotoras %>%
  select(locomotora, agua_media, carbon_media, velocidad_media, agua_cv, carbon_cv) %>%
  mutate(
    agua_media_norm = 1 - (agua_media - min(agua_media)) / (max(agua_media) - min(agua_media)),
    carbon_media_norm = 1 - (carbon_media - min(carbon_media)) / (max(carbon_media) - min(carbon_media)),
    velocidad_media_norm = (velocidad_media - min(velocidad_media)) / (max(velocidad_media) - min(velocidad_media)),
    agua_cv_norm = 1 - (agua_cv - min(agua_cv)) / (max(agua_cv) - min(agua_cv)),
    carbon_cv_norm = 1 - (carbon_cv - min(carbon_cv)) / (max(carbon_cv) - min(carbon_cv))
  ) %>%
  select(locomotora, agua_media_norm, carbon_media_norm, velocidad_media_norm, 
         agua_cv_norm, carbon_cv_norm)

# Para gráfico de barras comparativo (más fácil de interpretar)
comparativa_larga <- radar_data %>%
  pivot_longer(cols = -locomotora, names_to = "metrica", values_to = "puntuacion")

# Renombrar métricas
comparativa_larga$metrica <- factor(comparativa_larga$metrica,
                                    levels = c("agua_media_norm", "carbon_media_norm", 
                                               "velocidad_media_norm", "agua_cv_norm", "carbon_cv_norm"),
                                    labels = c("Eficiencia Agua", "Eficiencia Carbón", 
                                               "Velocidad", "Estabilidad Agua", "Estabilidad Carbón"))

# Gráfico de barras comparativo
p_radar <- ggplot(comparativa_larga, aes(x = metrica, y = puntuacion, fill = locomotora)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(puntuacion, 2)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("AND240-4253" = "firebrick1",
                               "RN141-2106" = "cyan2",
                               "MZA 1701" = "darkseagreen1",
                               "MZA 1801" = "chocolate1")) +
  labs(
    title = "Comparativa de locomotoras (puntuación normalizada)",
    subtitle = "1 = mejor, 0 = peor en cada categoría",
    x = "",
    y = "Puntuación normalizada",
    fill = "Locomotora"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Guardar gráfico
ggsave("exportaciones/graficos/05_comparativa_locomotoras.svg", p_radar, width = 12, height = 7, device = "svg")
print(p_radar)

# 9. Resumen ejecutivo para informe -------------------------------

cat("\n\n========================================\n")
cat("RESUMEN EJECUTIVO PARA INFORME FINAL\n")
cat("========================================\n")

cat("\n--- HALLAZGOS PRINCIPALES ---\n\n")

cat("1. EFICIENCIA AGUA:\n")
cat("   • Mejor: RN141-2106 (Mikado) con", ranking_agua$mejor_valor, "L/km\n")
cat("   • Peor: AND240-4253 con", ranking_agua$peor_valor, "L/km\n")
cat("   • Diferencia:", ranking_agua$diferencia_porcentaje, "%\n\n")

cat("2. EFICIENCIA CARBÓN:\n")
cat("   • Mejor: MZA 1701 con", ranking_carbon$mejor_valor, "kg/km\n")
cat("   • Peor: MZA 1801 con", ranking_carbon$peor_valor, "kg/km\n")
cat("   • Diferencia:", ranking_carbon$diferencia_porcentaje, "%\n\n")

cat("3. VELOCIDAD MEDIA:\n")
cat("   • Mejor: MZA 1801 con", ranking_velocidad$mejor_valor, "km/h\n")
cat("   • Peor: AND240-4253 con", ranking_velocidad$peor_valor, "km/h\n\n")

cat("4. PREDECIBILIDAD (menor CV):\n")
cat("   • Agua más estable: AND240-4253 (CV", ranking_estabilidad_agua$mejor_valor, "%)\n")
cat("   • Carbón más estable: AND240-4253 (CV", ranking_estabilidad_carbon$mejor_valor, "%)\n\n")

cat("5. PERFIL DE CADA LOCOMOTORA:\n")
cat("   • RN141-2106 (Mikado): La más equilibrada. Excelente en agua y carbón.\n")
cat("   • MZA 1701: Muy eficiente en carbón, buena en agua.\n")
cat("   • MZA 1801: La más rápida, pero consume ~3x más carbón.\n")
cat("   • AND240-4253: La más predecible, pero consume ~2x más agua.\n")

# 10. Exportar resultados -----------------------------------------

if (!dir.exists("exportaciones/csv")) dir.create("exportaciones/csv", recursive = TRUE)

write.csv(ranking_agua, "exportaciones/csv/05_ranking_agua.csv", row.names = FALSE)
write.csv(ranking_carbon, "exportaciones/csv/05_ranking_carbon.csv", row.names = FALSE)
write.csv(ranking_velocidad, "exportaciones/csv/05_ranking_velocidad.csv", row.names = FALSE)
write.csv(ranking_estabilidad_agua, "exportaciones/csv/05_ranking_estabilidad_agua.csv", row.names = FALSE)
write.csv(ranking_estabilidad_carbon, "exportaciones/csv/05_ranking_estabilidad_carbon.csv", row.names = FALSE)
write.csv(categorias, "exportaciones/csv/05_ganadores_por_categoria.csv", row.names = FALSE)
write.csv(perfil_locomotoras, "exportaciones/csv/05_perfil_locomotoras.csv", row.names = FALSE)

cat("\n\n✅ Análisis comparativo completado\n")
cat("✅ Gráfico guardado en: exportaciones/graficos/comparativa_locomotoras.svg\n")
cat("✅ CSVs guardados en: exportaciones/csv/\n")
cat("   - ranking_agua.csv\n")
cat("   - ranking_carbon.csv\n")
cat("   - ranking_velocidad.csv\n")
cat("   - ranking_estabilidad_agua.csv\n")
cat("   - ranking_estabilidad_carbon.csv\n")
cat("   - ganadores_por_categoria.csv\n")
cat("   - perfil_locomotoras.csv\n")
