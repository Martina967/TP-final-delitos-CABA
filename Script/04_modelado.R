# Cargar librerías necesarias
library(tidyverse)
library(lubridate)  
library(scales)     
library(dplyr)

# Rutas archivos 
instub <- 'input'
outstub <- 'output'

# Cargar datos
datos_delitos <- read.csv(file.path(instub,"dataset_completo_2019_2023_limpio.csv"))
cantidad_habitantes_por_comuna <- read.csv(file.path(instub,"cantidad_habitantes_por_comuna.csv"), sep = ";")
clasificacion_comunas_por_zona <- read.csv(file.path(instub,"clasificacion_comunas_por_zona.csv"), sep = ";")

# === TABLAS DE DIMENSIONES ===

# 1. dim_delito ----------------------------------------------------------------

# a. Crear combinaciones únicas de tipo, subtipo y uso de arma/moto, asignando un 
# identificador único (id_delito) a cada combinación.
dim_delito <- datos_delitos %>%
  select(tipo, subtipo, uso_arma, uso_moto) %>%
  distinct() %>%
  mutate(id_delito = row_number()) %>%
  select(id_delito, everything())

# 2. dim_ubicacion -------------------------------------------------------------

# a. Crear combinaciones únicas de barrio, comuna, latitud y longitud, asignando un
# identificador único (id_ubicacion) a cada combinación.

dim_ubicacion <- datos_delitos %>%
  select(barrio, comuna, latitud, longitud) %>%
  distinct() %>%
  mutate(id_ubicacion = row_number()) %>%
  select(id_ubicacion, everything())

# b. Agregar "zona" a dim_ubicacion
clasificacion_comunas_por_zona <- clasificacion_comunas_por_zona %>%
  mutate(comuna = as.character(comuna))
dim_ubicacion <- dim_ubicacion %>%
  left_join(clasificacion_comunas_por_zona, by = "comuna")
cantidad_habitantes_por_comuna <- cantidad_habitantes_por_comuna %>%
  mutate(comuna = as.character(comuna))
dim_ubicacion <- dim_ubicacion %>%
  left_join(cantidad_habitantes_por_comuna, by = "comuna")
dim_ubicacion <- dim_ubicacion %>%
  mutate(poblacion= as.numeric(poblacion))

glimpse(dim_ubicacion)
glimpse(clasificacion_comunas_por_zona)

# 3. dim_horario ----------------------------------------------------------------

# a. Crear combinaciones únicas de franja horaria, siendo esta el identificador único (id_franja)
dim_horario <- datos_delitos %>%
  filter(!is.na(franja)) %>%
  distinct(franja) %>%
  mutate(
    franja = as.integer(franja),
    hora_inicio = franja,
    hora_fin = franja +1,
    tramo = paste0(hora_inicio, " - ", hora_fin)
  ) %>%
  arrange(hora_inicio) %>%
  #Row number empieza en 1
  mutate(id_franja = row_number()) %>%
  select(id_franja, franja, hora_inicio, hora_fin, tramo)

# b. Agregar fila para franjas horarias no especificadas (NA)
dim_horario <- dim_horario %>%
  bind_rows(tibble(
    id_franja = max(dim_horario$id_franja) + 1,
    hora_inicio = NA,
    hora_fin = NA,
    tramo = "No especificado"
  ))

# 4. dim_fecha -----------------------------------------------------------------

# a. Agregar datos de feriados en Argentina

feriados_arg <- as.Date(c(
  # ---- Año 2019 ----
  "2019-01-01", "2019-03-04", "2019-03-05", "2019-03-24", "2019-04-02",
  "2019-04-19", "2019-05-01", "2019-05-25", "2019-06-17", "2019-06-20",
  "2019-07-09", "2019-08-17", "2019-10-12", "2019-11-20", "2019-12-08", "2019-12-25",
  
  # ---- Año 2020 ----
  "2020-01-01", "2020-02-24", "2020-02-25", "2020-03-24", "2020-04-02",
  "2020-04-10", "2020-05-01", "2020-05-25", "2020-06-17", "2020-06-20",
  "2020-07-09", "2020-08-17", "2020-10-12", "2020-11-20", "2020-12-08", "2020-12-25",
  
  # ---- Año 2021 ----
  "2021-01-01", "2021-02-15", "2021-02-16", "2021-03-24", "2021-04-02",
  "2021-05-01", "2021-05-25", "2021-06-17", "2021-06-20", "2021-07-09",
  "2021-08-16", "2021-10-11", "2021-11-20", "2021-12-08", "2021-12-25",
  
  # ---- Año 2022 ----
  "2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02",
  "2022-04-15", "2022-05-01", "2022-05-25", "2022-06-17", "2022-06-20",
  "2022-07-09", "2022-08-15", "2022-10-10", "2022-11-20", "2022-12-08", "2022-12-25",
  
  # ---- Año 2023 ----
  "2023-01-01", "2023-02-20", "2023-02-21", "2023-03-24", "2023-04-02",
  "2023-04-07", "2023-05-01", "2023-05-25", "2023-06-17", "2023-06-20",
  "2023-07-09", "2023-08-21", "2023-10-16", "2023-11-20", "2023-12-08", "2023-12-25"
))

# b. Crear combinaciones únicas de fecha, dia, mes y año asignando un
# identificador único (id_fecha) a cada combinación.

dim_fecha <- datos_delitos %>%
  select(fecha, anio, mes, dia) %>%
  distinct() %>%
  rename(id_fecha = fecha) %>%
  mutate(
    mes_nombre = mes,
    dia_nombre = dia,
    mes_numero = month(id_fecha),
    dia_numero = wday(id_fecha, week_start = 1),  # lunes = 1
    semana_mes = ceiling(day(id_fecha) / 7),
    semana_mes_nombre = case_when(
      semana_mes == 1 ~ "Semana 1",
      semana_mes == 2 ~ "Semana 2",
      semana_mes == 3 ~ "Semana 3",
      semana_mes == 4 ~ "Semana 4",
      TRUE ~ "Semana 5"
    ),
    trimestre = quarter(id_fecha),
    trimestre_nombre = case_when(
      trimestre == 1 ~ "Primer trimestre",
      trimestre == 2 ~ "Segundo trimestre",
      trimestre == 3 ~ "Tercer trimestre",
      trimestre == 4 ~ "Cuarto trimestre"
    ),
    cuatrimestre = case_when(
      mes_numero %in% 1:4 ~ 1,
      mes_numero %in% 5:8 ~ 2,
      mes_numero %in% 9:12 ~ 3
    ),
    cuatrimestre_nombre = case_when(
      cuatrimestre == 1 ~ "Primer cuatrimestre",
      cuatrimestre == 2 ~ "Segundo cuatrimestre",
      cuatrimestre == 3 ~ "Tercer cuatrimestre",
      cuatrimestre == 4 ~ "Cuarto cuatrimestre"
    ),
    fin_de_semana = dia_nombre %in% c("Sabado", "Domingo"),
    feriado = id_fecha %in% feriados_arg
  ) %>%
  select(id_fecha, anio, mes_numero, mes_nombre,
         dia_numero, dia_nombre, semana_mes, semana_mes_nombre, trimestre,trimestre_nombre, cuatrimestre, cuatrimestre_nombre,
         fin_de_semana, feriado)

# === TABLAS DE HECHOS ===

# 1. Crear tabla de hechos que contiene las claves foráneas y la 'cantidad' de 
# delitos registrados.

delitos_joined <- datos_delitos %>%
  left_join(dim_fecha %>% select(id_fecha), by = c("fecha" = "id_fecha")) %>%
  # c. Modificar franja (que empiza en 0) como franja + 1 para unir con id_franja de dim_ubicación que empieza con 1
  mutate(franja = franja+1) %>%
  left_join(dim_horario %>% select(id_franja), by = c("franja" = "id_franja")) %>%
  left_join(dim_delito %>% select(tipo, subtipo, uso_arma, uso_moto, id_delito), by = c("tipo", "subtipo", "uso_arma", "uso_moto")) %>%
  left_join(dim_ubicacion %>% select(barrio, comuna, latitud, longitud, id_ubicacion), by = c("barrio", "comuna", "latitud", "longitud"))


hechos_delito <- delitos_joined %>%
  select(        
    fecha,       
    franja,      
    id_delito,      
    id_ubicacion,
    cantidad        
  )

hechos_delito <- hechos_delito %>%
  rename(id_fecha = fecha,
         id_franja = franja,
         id_delito = id_delito,
         id_ubicacion = id_ubicacion,
         cantidad = cantidad)

# 2. Ajustes finales 
# Reemplazar nulls de "id_franja" por 25 para que se mapee como "No especificado"
hechos_delito <- hechos_delito %>%
mutate(id_franja = ifelse(is.na(id_franja), 25, id_franja)) 
# Reemplezar nulls de "franja" de dim_horario por "No especificado"
dim_horario <- dim_horario %>%
  mutate(franja = ifelse(is.na(franja), "No especificado", franja)) 

glimpse(dim_fecha)


# 3. Guardar datos en archivo csv
write.csv(hechos_delito, file.path(outstub, "hechos_delito.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(dim_delito, file.path(outstub, "dim_delito.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(dim_ubicacion, file.path(outstub, "dim_ubicacion.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(dim_horario, file.path(outstub, "dim_horario.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(dim_fecha, file.path(outstub, "dim_fecha.csv"), row.names = FALSE, fileEncoding = "UTF-8")

