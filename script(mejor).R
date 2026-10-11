#paquetes####
library(dplyr)
library(lubridate)
library(tidyr)
library(stargazer)
library(mfx)
library(oddsratio)
library(marginaleffects)
library(ggplot2)

#===============================
#DATA WRANGLING
setwd("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2")
diputados_diputados<-read.csv2("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2/diputados-diputados.csv")
votaciones_diputados<-read.csv2("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2/votaciones-diputados.csv")
bloques_diputados<-read.csv2("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2/bloques-diputados.csv")

#Selección de Proyectos y merging de hojas#####
votaciones_diputados_merged <- votaciones_diputados %>%
  left_join(
    diputados_diputados %>%
      select(diputadoId, nombre, distrito, sexo),
    by = "diputadoId"
  )


votaciones_diputados_merged <- votaciones_diputados_merged %>%
  left_join(
    bloques_diputados %>%
      select(bloqueId, bloque),
    by = "bloqueId"
  )


asuntos_seleccionados <- c(
  3034, 2996, 2992, 2994, 2974, 2989, 2892, 2882,
  2879, 2880, 2819, 2821, 2786, 2796, 2750, 2700, 2717, 2779
)

votaciones_diputados_filtrado <- votaciones_diputados_merged %>%
  filter(asuntoId %in% asuntos_seleccionados)

asuntos<-read_xlsx("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2/FILTRADO-asuntos-diputados-2.xlsx")


votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  left_join(
   asuntos %>%
      select(asuntoId, ano, fecha, resultado, titulo),
    by = "asuntoId"
  )

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    ano_camara = if_else(
      asuntoId %in% c(2974, 2989),
      2026,
      ano
    )
  )




#Merge con base de datos de gobernadores####
gobs<-read_xlsx("~/Library/CloudStorage/OneDrive-UniversidadTorcuatoDiTella/Segundo Semestre/Actores y Procesos Politicos/tp_actores/DecadaVotadaCSV 2/gobs.xlsx")



votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  left_join(
    gobs,
    by = c("ano_camara", "distrito")
  )

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  select(-"Fecha de Asunción", -"Observaciones")


#Armado de variable copartidarios####

# 1. Importar el CSV de coaliciones (usamos skip = 1 para saltear la primera línea de título)
coaliciones <- read.csv2(
  "Coaliciones_Políticas_v2.csv", 
  skip = 1, 
  stringsAsFactors = FALSE
)

# 2. Pasar a formato largo: separar los partidos que están en una misma celda (por ";" o ",")
coaliciones_largo <- coaliciones %>%
  filter(!is.na(coalicion) & coalicion != "") %>%
  separate_rows(coalicion, sep = "[;,]") %>% # Separa tanto por punto y coma como por coma
  mutate(
    bloque = trimws(bloque),
    coalicion = trimws(coalicion),
    es_copartidario = 1
  ) %>%
  distinct(bloque, coalicion, .keep_all = TRUE)

# 3. Unir con la base principal y crear la variable dummy 'copartidario'
# NOTA: Reemplaza "Partido" por el nombre exacto de la columna del partido del gobernador en 'gobs.xlsx'
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    bloque_clean = trimws(bloque),
    partido_gob_clean = trimws(`pp_gob`) # <-- CAMBIAR `Partido` SI TU COLUMNA SE LLAMA DISTINTO
  ) %>%
  left_join(
    coaliciones_largo,
    by = c("bloque_clean" = "bloque", "partido_gob_clean" = "coalicion")
  ) %>%
  mutate(
    # Si hizo match es 1, si no hizo match (y sabemos el partido del gobernador) es 0
    copartidario = if_else(!is.na(es_copartidario), 1, 0)
  ) %>%
  select(-bloque_clean, -partido_gob_clean, -es_copartidario)




#Armado de dummies Regionales#####
noa <- c("Jujuy", "Salta", "Tucumán", "Catamarca",
         "La Rioja", "Santiago del Estero")

nea <- c("Formosa", "Chaco", "Corrientes", "Misiones")

patagonia_norte <- c("Rio Negro", "Neuquén", "La Pampa")

patagonia_sur <- c("Chubut", "Santa Cruz", "Tierra del Fuego")


votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    `NOA` = if_else(distrito %in% noa, 1, 0),
    `NEA` = if_else(distrito %in% nea, 1, 0),
    `Patagonia Norte` = if_else(distrito %in% patagonia_norte, 1, 0),
    `Patagonia Sur` = if_else(distrito %in% patagonia_sur, 1, 0),
    Cordoba = if_else(distrito == "Córdoba", 1, 0)
  )





#Construcción del tamaño del bloque#########

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  group_by(asuntoId, bloque) %>%
  mutate(
    tam_bloque = n_distinct(diputadoId)
  ) %>%
  ungroup()


# Construcción de los presidentes de bloque######
presidentes_bloque <- tibble::tribble(
  ~bloque, ~fecha_inicio, ~fecha_fin, ~presidente,
  "La Libertad Avanza", "2024-01-01", "2024-04-09", "ZAGO, Oscar",
  "La Libertad Avanza", "2024-04-10", "2026-12-31", "BORNORONI, Gabriel",
  "Unión por la Patria", "2024-01-01", "2026-12-31", "MARTÍNEZ, Germán Pedro",
  "PRO", "2024-01-01", "2026-12-31", "RITONDO, Cristian",
  "Unión Cívica Radical", "2024-01-01", "2025-12-09", "DE LOREDO, Rodrigo",
  "Unión Cívica Radical", "2025-12-10", "2026-12-31", "VERASAY, Pamela Fernanda",
  "Hacemos Coalicion Federal", "2024-01-01", "2026-12-31", "PICHETTO, Miguel Angel",
  "Encuentro Federal", "2024-01-01", "2026-12-31", "PICHETTO, Miguel Angel",
  "Coalición Cívica", "2024-01-01", "2026-12-31", "FERRARO, Maximiliano Carlos Francisco",
  "Democracia Para Siempre", "2024-01-01", "2026-12-31", "JULIANO, Pablo",
  "Innovacion Federal", "2024-01-01", "2025-12-09", "CALLETTI, Pamela",
  "Innovacion Federal", "2025-12-10", "2026-12-31", "ARRUA, Alberto",
  "Provincias Unidas", "2024-01-01", "2026-12-31", "SCAGLIA, Gisela",
  "Mov. Integ. y Desarrollo", "2024-01-01", "2026-12-31", "ZAGO, Oscar",
  "Independencia", "2025-12-10", "2026-12-31", "MEDINA, Gladys",
  "Frente de Izquierda", "2025-12-10", "2026-12-31", "BREGMAN, Myriam",
  "Frente Producción y Trabajo", "2024-01-01", "2025-12-09", "PICON MARTINEZ, Nancy Viviana"
) %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio),
    fecha_fin = as.Date(fecha_fin)
  )

# 2. Normalize names for matching
# Assumes the deputy name column is called "nombre"
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    nombre_match = toupper(trimws(nombre)),
    fecha = as.Date(fecha)
  )

presidentes_bloque <- presidentes_bloque %>%
  mutate(
    presidente_match = toupper(trimws(presidente))
  )


# Armar variable del voto presidentes de bloque####
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  left_join(
    presidentes_bloque,
    by = join_by(
      bloque,
      fecha >= fecha_inicio,
      fecha <= fecha_fin
    ),
    relationship = "many-to-many"
  ) %>%
  mutate(
    presidente_bloque = as.integer(
      !is.na(nombre_match) &
        !is.na(presidente_match) &
        nombre_match == presidente_match
    )
  ) %>%
  select(-fecha_inicio, -fecha_fin, -presidente,
         -nombre_match, -presidente_match)

# 4. Inspect observations where the deputy was a bloc president
votaciones_diputados_filtrado %>%
  filter(presidente_bloque == 1) %>%
  distinct(diputadoId, asuntoId, nombre, bloque, fecha)




votos_presidentes_bloque <- votaciones_diputados_filtrado %>%
  filter(presidente_bloque == 1) %>%
  select(asuntoId, bloque, voto) %>%
  distinct() %>%
  pivot_wider(
    names_from = bloque,
    values_from = voto
  )

# View the resulting table
View(votos_presidentes_bloque)
write.csv(votos_presidentes_bloque, file="votos_presidentes_bloque.csv")






# 1. Extract each bloc president's vote for each asuntoId
votos_presidentes <- votaciones_diputados_filtrado %>%
  filter(presidente_bloque == 1) %>%
  select(asuntoId, bloqueId, voto) %>%
  rename(voto_presidente = voto)

# 2. Add the president's vote to every deputy's observation
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  left_join(
    votos_presidentes,
    by = c("asuntoId", "bloqueId")
  )



#Variable voto distinto####
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    voto_distinto = case_when(
      voto_presidente == 3 ~ NA_real_,
      is.na(voto_presidente) ~ NA_real_,
      is.na(voto) ~ NA_real_,
      voto != voto_presidente ~ 1,
      TRUE ~ 0
    )
  )



#Variablo voto con ejecutivo####
votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    voto_con_ejecutivo = if_else(
      voto_distinto == 1 & voto == 0,
      1,
      0,
      missing = NA_integer_
    )
  )


#Tabla resultante#####




tabla_proyecto_bloque <- votaciones_diputados_filtrado %>%
  group_by(asuntoId, bloque) %>%
  summarise(
    voto_presidente = first(voto_presidente[!is.na(voto_presidente)]),
    n_voto_distinto = sum(voto_distinto == 1, na.rm = TRUE),
    n_voto_con_ejecutivo = sum(voto_con_ejecutivo == 1, na.rm = TRUE),
    tam_bloque = n_distinct(diputadoId),
    .groups = "drop"
  ) %>%
  arrange(asuntoId, bloque)

View(tabla_proyecto_bloque)
write.csv(tabla_proyecto_bloque, file="tabla_proyecto_bloque.csv")


#Base de datos final####

write.csv(votaciones_diputados_filtrado, file="votaciones_diputados_filtrado.csv")


#==========================

#ESTADISTICAS DESCRIPTIVAS###### 


tabla_frecuencias_vd <- votaciones_diputados_filtrado %>%
  group_by(bloque) %>%
  summarise(
    Frecuencia = sum(voto_distinto == 1, na.rm = TRUE),
    Porcentaje = mean(voto_distinto == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

stargazer(
  as.data.frame(
    tabla_frecuencias_vd),
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Voto distinto por bloque",
  digits = 2,
  out = "tabla_frecuencias.html"
)


tabla_frecuencias_vd_ac <- votaciones_diputados_filtrado %>%
  group_by(ano_camara) %>%
  summarise(
    Frecuencia = sum(voto_distinto == 1, na.rm = TRUE),
    Porcentaje = mean(voto_distinto == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

tabla_frecuencias_vd

tabla_frecuencias_vce <- votaciones_diputados_filtrado %>%
  group_by(bloque) %>%
  summarise(
    Frecuencia = sum(voto_con_ejecutivo == 1, na.rm = TRUE),
    Porcentaje = mean(voto_con_ejecutivo == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

tabla_frecuencias_vd_ac

tabla_frecuencias_vce_ac <- votaciones_diputados_filtrado %>%
  group_by(ano_camara) %>%
  summarise(
    Frecuencia = sum(voto_con_ejecutivo == 1, na.rm = TRUE),
    Porcentaje = mean(voto_con_ejecutivo == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

tabla_frecuencias_vce_ac 

#===============================REGRESIONES======================================

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(
    bloque = relevel(as.factor(bloque), ref = "La Libertad Avanza"),
    ano_camara = relevel(as.factor(ano_camara), ref = "2024")
  )

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  mutate(sexo = case_when(
    sexo == "m" ~ 0,
    sexo == "f" ~ 1,
    TRUE ~ NA_real_
  ))

votaciones_diputados_filtrado <- votaciones_diputados_filtrado %>%
  filter(tam_bloque>=3)


votaciones_diputados_filtrado <- na.omit(votaciones_diputados_filtrado)

analysis <- votaciones_diputados_filtrado %>%
  select(
    sexo, 
    bloque, 
    ano_camara, 
    agp, 
    NOA, 
    NEA, 
    `Patagonia Norte`, 
    `Patagonia Sur`, 
    Cordoba, 
    tam_bloque, 
    voto_distinto, 
    copartidario,
    voto_con_ejecutivo,
    asuntoId
  )

analysis.nopaso <- analysis %>%
  filter(asuntoId != 2879)


#Regresión LOGIT para voto distinto
logit <- glm(
  voto_distinto ~ sexo + bloque + ano_camara +
    agp*copartidario + NOA + NEA +
    `Patagonia Sur` + `Patagonia Norte` +
    Cordoba + tam_bloque,
  data = analysis,
  family = binomial(link = "logit")
)

summary(logit)


logit.nopaso <- glm(
  voto_distinto ~ sexo + bloque + ano_camara +
    agp*copartidario + NOA + NEA +
    `Patagonia Sur` + `Patagonia Norte` +
    Cordoba + tam_bloque,
  data = analysis.nopaso,
  family = binomial(link = "logit")
)

summary(logit.nopaso)


logitor<-mfx::logitor(voto_distinto ~ sexo + bloque + ano_camara +
                        agp*copartidario + NOA + NEA +
                        `Patagonia Sur` + `Patagonia Norte` +
                        Cordoba + tam_bloque,
                      data = analysis)

logitor


logitor.nopaso<-mfx::logitor(voto_distinto ~ sexo + bloque + ano_camara +
                               agp*copartidario + NOA + NEA +
                               `Patagonia Sur` + `Patagonia Norte` +
                               Cordoba + tam_bloque,
                             data = analysis.nopaso)

logitor.nopaso

#logit para voto con ejecutivo

analysis.nobloc <- votaciones_diputados_filtrado %>%
  select(
    sexo, 
    ano_camara, 
    agp, 
    NOA, 
    NEA, 
    `Patagonia Norte`, 
    `Patagonia Sur`, 
    Cordoba, 
    tam_bloque, 
    voto_distinto, 
    copartidario,
    voto_con_ejecutivo, 
    asuntoId
  )

analysis.nobloc <- analysis.nobloc %>%
  filter(asuntoId != 2879)

logit.nobloc <- glm(
  voto_con_ejecutivo ~ sexo + ano_camara +
    agp*copartidario + NOA + NEA +
    `Patagonia Sur` + `Patagonia Norte` +
    Cordoba + tam_bloque,
  data = analysis.nobloc,
  family = binomial(link = "logit")
)

summary(logit.nobloc)

logitor.nobloc<-mfx::logitor(voto_con_ejecutivo ~ sexo  + ano_camara +
                        agp*copartidario + NOA + NEA +
                        `Patagonia Sur` + `Patagonia Norte` +
                        Cordoba + tam_bloque,
                      data = analysis.nobloc)

logitor.nobloc

tabla_frecuencias_vce_nopaso <- analysis.nopaso %>%
  group_by(asuntoId) %>%
  summarise(
    Frecuencia = sum(voto_con_ejecutivo == 1, na.rm = TRUE),
    Porcentaje = mean(voto_con_ejecutivo == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

tabla_frecuencias_vce_nopaso 
