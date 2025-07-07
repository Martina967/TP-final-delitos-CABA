# Cargar librerías necesarias
library(tidyverse)
library(lubridate)  
library(scales)     
library(dplyr)

# Rutas archivos 
instub <- 'input'
outstub <- 'input'

# Cargar datos
library(readr)

datos_delitos <- read_csv(file.path(instub, "dataset_completo_2019_2023.csv"),
                          col_types = cols(
                            latitud = col_character(),
                            longitud = col_character()
                          ))


# === EXPLORACIÓN DE DATOS ==

# 1. Tipo de dato de cada campo ------------------------------------------------
glimpse(datos_delitos)

# 2. Resumen de datos ----------------------------------------------------------
summary(datos_delitos)

# 3. Contenido de ciertas columnas ---------------------------------------------
table(datos_delitos$anio)
table(datos_delitos$mes)
table(datos_delitos$dia)
table(datos_delitos$franja)
table(datos_delitos$tipo)
table(datos_delitos$subtipo)
table(datos_delitos$uso_arma)
table(datos_delitos$uso_moto)
table(datos_delitos$barrio)
table(datos_delitos$comuna)
table(datos_delitos$cantidad)

# 4. Chequear las primeras filas
head(datos_delitos, 5)

# 5. Chequear si hay datos faltantes
colSums(is.na(datos_delitos))

# ¿Cúantos NULL hay en nuestro dataset? 
sum(apply(datos_delitos, 1, function(fila) any(fila == "NULL", na.rm = TRUE)))

# === LIMPIEZA DE DATOS ===

# 1. Cambiar tipo de datos -----------------------------------------------------

# a. Convertir fechas a tipo date
delitos <- datos_delitos %>%
  mutate(fecha = ymd(fecha))
str(delitos)

# b. Convertir campos binarios a tipo boolean

delitos <- delitos %>%
  mutate(
    uso_arma = uso_arma == "SI",
    uso_moto = uso_moto == "SI"
  )

# c. Convertir campos de anio y franja a tipo integer

delitos <- delitos %>%
  mutate(franja = ifelse(franja == "NULL", NA, franja),
         franja = as.numeric(franja))

delitos <- delitos %>%
  mutate(anio = as.integer(anio),
         franja = as.integer(franja),
         )

# d. Convertir campos de lontitud y latitud a tipo double
# Extraer coordenadas más frecuentes por barrio
coordenadas_barrios <- delitos %>%
  filter(!is.na(latitud), !is.na(longitud)) %>%
  count(barrio, latitud, longitud) %>%
  group_by(barrio) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(barrio, latitud, longitud)

# Corregir manualmente longitudes y latitudes incorrectas
coordenadas_fijas <- tibble::tibble(
  barrio = c("CABALLITO", "PARQUE CHAS", "PUERTO MADERO", "RETIRO",
             "VERSALLES", "VILLA ORTUZAR", "VILLA SOLDATI", "VILLA URQUIZA"),
  latitud = c("-34.608797", "-34.586000", "-34.610000", "-34.588420",
              "-34.631330", "-34.579900", "-34.678820", "-34.577880"),
  longitud = c("-58.449165", "-58.479310", "-58.362000", "-58.375940",
               "-58.514160", "-58.474306", "-58.456270", "-58.479800"))

coordenadas_barrios <- coordenadas_barrios %>%
  left_join(coordenadas_fijas, by = "barrio", suffix = c("_vieja", "")) %>%
  mutate(
    latitud = if_else(!is.na(latitud), latitud, latitud_vieja),
    longitud = if_else(!is.na(longitud), longitud, longitud_vieja)
  ) %>%
  select(barrio, latitud, longitud)

# Reemplazar en el dataset original
delitos <- delitos %>%
  select(-latitud, -longitud) %>%
  left_join(coordenadas_barrios, by = "barrio")

# Asegurar tipo numérico
delitos <- delitos %>%
  mutate(
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  )

# e. Chequear cambios
glimpse(delitos)

# 2. Tratar NULL values -----------------------------------------------------

# a. Reemplazar NULL por "No específicado" en las columnas tipo texto
table(delitos$comuna)

#delitos <- delitos %>%
#  mutate(across(where(is.character), ~ ifelse(. == "NULL", "No especificado", .)))

delitos <- delitos %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "" | . == "NULL", "No especificado", .)))
table(delitos$comuna)

# Corregir filas con comunas no especificadas pero barrio especificado

delitos <- delitos %>%
  mutate(comuna = ifelse(comuna == "No especificado" & barrio == "SAN TELMO", "1",
                         ifelse(comuna == "No especificado" & barrio == "VILLA GRAL. MITRE", "11", comuna)))

# 3. Normalizar valores --------------------------------------------------------

# a. Normalizar nombres propios: convertir a formato título (primera letra mayúscula, resto minúsculas)
# en los campos "mes", "dia" y "barrio", que estaban originalmente en mayúsculas.

delitos <- delitos %>%
  mutate(
    mes = str_to_title(mes),
    dia = str_to_title(dia),
    barrio = str_to_title(barrio)
  )

# 4. Eliminar duplicados -------------------------------------------------------

# a. Eliminar filas duplicadas completas (todas las columnas iguales).

delitos <- delitos %>%
  distinct()

# Guardar datos en archivo csv
write.csv(delitos, file.path(outstub, "dataset_completo_2019_2023_limpio.csv"), row.names = FALSE, fileEncoding = "UTF-8")



