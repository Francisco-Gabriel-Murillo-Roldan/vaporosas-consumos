CREATE TABLE sqlite_sequence(name,seq);
CREATE TABLE IF NOT EXISTS "Tenderes" (
	"id_tender"	INTEGER NOT NULL,
	"nombre"	TEXT,
	"capacidad_agua_L"	REAL,
	"capacidad_carbon_kg"	REAL,
	"masa_vacia_t"	REAL,
	"coeficiente_Davis_a"	REAL,
	"coeficiente_Davis_b"	REAL,
	"coeficiente_Davis_c"	REAL,
	PRIMARY KEY("id_tender" AUTOINCREMENT)
);
CREATE TABLE IF NOT EXISTS "Locomotoras" (
	"id_locomotora"	INTEGER,
	"nombre"	TEXT,
	"id_tender"	INTEGER,
	"tipo"	TEXT,
	"configuracion"	TEXT,
	"potencia_kW"	INTEGER,
	"potencia_HP"	INTEGER,
	"potencia_CV"	INTEGER,
	"peso_total_t"	REAL,
	"peso_adherente_t"	REAL,
	"modelo_ck_a"	REAL,
	"modelo_ck_b"	REAL,
	"modelo_ck_c"	REAL,
	"coeficiente_Davis_a"	REAL,
	"coeficiente_Davis_b"	REAL,
	"coeficiente_Davis_c"	REAL,
	PRIMARY KEY("id_locomotora"),
	FOREIGN KEY("id_tender") REFERENCES "Tenderes"
);
CREATE TABLE IF NOT EXISTS "Material_Remolcado" (
	"id_material_remolcado"	INTEGER NOT NULL,
	"nombre"	TEXT NOT NULL,
	"nombre_wag"	TEXT NOT NULL,
	"tipo"	TEXT,
	"masa_t"	REAL NOT NULL,
	"num_ejes"	INTEGER NOT NULL,
	"freno_tipo"	TEXT,
	"coeficientes_davis"	TEXT,
	"observaciones"	TEXT,
	PRIMARY KEY("id_material_remolcado" AUTOINCREMENT)
);
CREATE TABLE Composicion_detalle (
    id_composicion INTEGER NOT NULL,
    id_material_remolcado INTEGER NOT NULL,
    cantidad INTEGER NOT NULL DEFAULT 1,
    FOREIGN KEY (id_composicion) REFERENCES composiciones(id_composicion),
    FOREIGN KEY (id_material_remolcado) REFERENCES Material_Remolcado(id_material_remolcado)
);
CREATE TABLE IF NOT EXISTS "Composiciones" (
	"id_composicion"	INTEGER,
	"nombre"	TEXT NOT NULL,
	"descripcion"	TEXT,
	"masa_remolcada_t"	REAL,
	"observaciones"	TEXT,
	PRIMARY KEY("id_composicion" AUTOINCREMENT)
);
CREATE TABLE Balances (
    id_balance INTEGER PRIMARY KEY AUTOINCREMENT,
    id_locomotora INTEGER NOT NULL,
    id_composicion INTEGER NOT NULL,
    balance_ET_R_kN REAL,
    velocidad_objetivo_km_h REAL,
    observaciones TEXT,
    FOREIGN KEY (id_locomotora) REFERENCES locomotoras(id_locomotora),
    FOREIGN KEY (id_composicion) REFERENCES composiciones(id_composicion),
    UNIQUE(id_locomotora, id_composicion)
);
CREATE TABLE Rutas (
    "id_ruta" INTEGER PRIMARY KEY AUTOINCREMENT,
    "nombre_ruta" TEXT NOT NULL
, descripcion TEXT);
CREATE TABLE IF NOT EXISTS "Pruebas" (
	"id_prueba"	INTEGER,
	"id_ruta"	INTEGER,
	"fecha"	INTEGER,
	"ruta"	TEXT,
	"condiciones_fijas"	INTEGER,
	"clima"	TEXT,
	"correccion_factor_adherencia"	REAL,
	"objetivo"	TEXT,
	"observaciones"	TEXT,
	PRIMARY KEY("id_prueba")
);
CREATE TABLE IF NOT EXISTS "Perfiles_QGIS" (
    id_perfil INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ruta INTEGER NOT NULL,
    distancia_km REAL NOT NULL,                -- Distancia recorrida desde el inicio
    punto_km_linea REAL,                       -- El punto kilométrico de la vía
    cota_suavizada_msnm REAL,                  -- Cota en metros sobre el nivel del mar
    coordenada_x REAL,                         -- Coordenada UTM X (o geográfica)
    coordenada_y REAL,                         -- Coordenada UTM Y
    pendiente_por_cien REAL,                   -- Pendiente en porcentaje
    desnivel_instantaneo_m REAL,               -- Diferencia de cota con el punto anterior
    desnivel_acumulado_m REAL, observaciones TEXT,                 -- Suma de todos los desniveles positivos hasta este punto
    FOREIGN KEY (id_ruta) REFERENCES Rutas(id_ruta)
);
CREATE TABLE Tramos (
    id_tramo INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ruta INTEGER NOT NULL,
    id_prueba INTEGER NOT NULL,
    nombre_tramo TEXT NOT NULL,
    distancia_inicio_km REAL NOT NULL,
    distancia_fin_km REAL NOT NULL, observaciones TEXT,
    FOREIGN KEY (id_ruta) REFERENCES Rutas(id_ruta)
    FOREIGN KEY (id_prueba) REFERENCES Pruebas(id_prueba)
);
CREATE TABLE IF NOT EXISTS "Consumos" (
	"id_consumo"	INTEGER NOT NULL,
	"id_locomotora"	INTEGER NOT NULL,
	"id_prueba"	INTEGER NOT NULL,
	"id_tramo"	INTEGER,
	"agua_consumida_L"	REAL NOT NULL,
	"carbon_consumido_kg"	REAL NOT NULL,
	"agua_L_por_km"	REAL,
	"carbon_kg_por_km"	REAL,
	"tiempo_horas"	REAL,
	"velocidad_media_km_h"	REAL,
	"relacion_ac_decimal"	REAL,
	"relacion_ac_for"	TEXT,
	"corte_porcentaje"	REAL,
	"regulador_porcentaje"	REAL,
	PRIMARY KEY("id_consumo" AUTOINCREMENT),
	FOREIGN KEY("id_locomotora") REFERENCES "Locomotoras"("id_locomotora"),
	FOREIGN KEY("id_prueba") REFERENCES "Pruebas"("id_prueba")
);
CREATE TABLE Observaciones (
    id_observacion INTEGER PRIMARY KEY AUTOINCREMENT,
    tabla_afectada TEXT NOT NULL,      -- 'Perfiles', 'Consumos', 'Locomotoras', etc.
    registro_id TEXT,                  -- Identificador del registro (ej. 'id_ruta=1, distancia_km=242.663')
    tipo TEXT NOT NULL,                -- 'Corrección', 'Anomalía', 'Decisión', 'Nota'
    observacion TEXT NOT NULL,
    fecha_creacion TEXT DEFAULT CURRENT_TIMESTAMP,
    usuario TEXT DEFAULT 'Estroncio'
);
CREATE TABLE Zonas (
    id_zona INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ruta INTEGER NOT NULL,
    nombre_zona TEXT NOT NULL,
    descripcion TEXT,
    distancia_inicio_km REAL NOT NULL,
    distancia_fin_km REAL NOT NULL,
    tipo TEXT CHECK(tipo IN ('Ascenso', 'Descenso', 'Ondulado', 'Llano', 'Mixto')),
    observaciones TEXT,
    FOREIGN KEY (id_ruta) REFERENCES Rutas(id_ruta)
);
CREATE TABLE Precios (
    id_precio INTEGER PRIMARY KEY AUTOINCREMENT,
    recurso TEXT NOT NULL UNIQUE,  -- 'agua', 'carbon'
    precio_unidad REAL NOT NULL,
    moneda TEXT NOT NULL DEFAULT 'EUR',  -- 'EUR', 'USD', 'PTA', 'UM'
    observaciones TEXT
);
CREATE VIEW "Estadisticas_completas_consumo_por_locomotora" AS SELECT
    l.nombre,

    ROUND(AVG(c.agua_L_por_km), 1) AS agua_media_L_km,
    ROUND(AVG(c.agua_L_por_km * c.agua_L_por_km) - AVG(c.agua_L_por_km) * AVG(c.agua_L_por_km), 1) AS agua_varianza,
    ROUND(SQRT(AVG(c.agua_L_por_km * c.agua_L_por_km) - AVG(c.agua_L_por_km) * AVG(c.agua_L_por_km)), 1) AS agua_desviacion_tipica,
    MAX(c.agua_L_por_km) AS agua_max_L_km,
    MIN(c.agua_L_por_km) AS agua_min_L_km,

    ROUND(AVG(c.carbon_kg_por_km), 2) AS carbon_media_kg_km,
    ROUND(AVG(c.carbon_kg_por_km * c.carbon_kg_por_km) - AVG(c.carbon_kg_por_km) * AVG(c.carbon_kg_por_km), 2) AS carbon_varianza,
    ROUND(SQRT(AVG(c.carbon_kg_por_km * c.carbon_kg_por_km) - AVG(c.carbon_kg_por_km) * AVG(c.carbon_kg_por_km)), 2) AS carbon_desviacion_tipica,
    MAX(c.carbon_kg_por_km) AS carbon_max_kg_km,
    MIN(c.carbon_kg_por_km) AS carbon_min_kg_km,

    ROUND(AVG(c.velocidad_media_km_h), 1) AS velocidad_media_km_h,
    ROUND(AVG(c.velocidad_media_km_h * c.velocidad_media_km_h) - AVG(c.velocidad_media_km_h) * AVG(c.velocidad_media_km_h), 1) AS velocidad_varianza,
    ROUND(SQRT(AVG(c.velocidad_media_km_h * c.velocidad_media_km_h) - AVG(c.velocidad_media_km_h) * AVG(c.velocidad_media_km_h)), 1) AS velocidad_desviacion_tipica,
    MAX(c.velocidad_media_km_h) AS velocidad_max_km_h,
    MIN(c.velocidad_media_km_h) AS velocidad_min_km_h
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
GROUP BY l.id_locomotora
/* Estadisticas_completas_consumo_por_locomotora(nombre,agua_media_L_km,agua_varianza,agua_desviacion_tipica,agua_max_L_km,agua_min_L_km,carbon_media_kg_km,carbon_varianza,carbon_desviacion_tipica,carbon_max_kg_km,carbon_min_kg_km,velocidad_media_km_h,velocidad_varianza,velocidad_desviacion_tipica,velocidad_max_km_h,velocidad_min_km_h) */;
CREATE VIEW "Desglose_consumo_por_ruta" AS SELECT
    l.nombre,
    r.nombre_ruta,
    ROUND(AVG(c.agua_L_por_km), 1) AS agua_media_L_km,
    ROUND(AVG(c.carbon_kg_por_km), 2) AS carbon_media_kg_km,
    ROUND(AVG(c.velocidad_media_km_h), 1) AS velocidad_media_km_h
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Tramos t ON c.id_tramo = t.id_tramo
JOIN Rutas r ON t.id_ruta = r.id_ruta
GROUP BY l.id_locomotora, r.id_ruta
ORDER BY l.id_locomotora, r.id_ruta
/* Desglose_consumo_por_ruta(nombre,nombre_ruta,agua_media_L_km,carbon_media_kg_km,velocidad_media_km_h) */;
CREATE TABLE Perfiles_Simulador (
    id_perfil_sim INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ruta INTEGER NOT NULL,
    distancia_acumulada_m REAL NOT NULL,
    distancia_siguiente_seccion_m REAL,
    altura_msnm REAL NOT NULL,
    curvatura REAL,
    pendiente_porcentaje REAL,
    elemento_via_texto TEXT,
    elemento_via_tipo INTEGER,
    FOREIGN KEY (id_ruta) REFERENCES Rutas(id_ruta)
);
CREATE VIEW "Coste_por_ruta" AS SELECT
    l.nombre as locomotora,
    r.nombre_ruta,
    ROUND(AVG(c.agua_L_por_km), 1) as agua_media,
    ROUND(AVG(c.carbon_kg_por_km), 2) as carbon_media,
    CASE r.nombre_ruta
        WHEN 'Vallarizasoria' THEN 152.26
        WHEN 'Eje central andaluces' THEN 52.74
        WHEN 'CTN test route' THEN 8.30
    END as longitud_km,
    ROUND(AVG(c.agua_L_por_km) * 0.0036 + AVG(c.carbon_kg_por_km) * 0.24, 4) as coste_km
FROM Consumos c
JOIN Locomotoras l ON c.id_locomotora = l.id_locomotora
JOIN Tramos t ON c.id_tramo = t.id_tramo
JOIN Rutas r ON t.id_ruta = r.id_ruta
GROUP BY l.nombre, r.nombre_ruta
ORDER BY r.nombre_ruta, coste_km
/* Coste_por_ruta(locomotora,nombre_ruta,agua_media,carbon_media,longitud_km,coste_km) */;
