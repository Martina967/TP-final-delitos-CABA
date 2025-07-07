# Cargar librerías necesarias
library(tidyverse)
library(lubridate) 
library(scales)     
library(dplyr)
library(ggplot2)

# Rutas archivos 
instub <- 'input'
outstub <- 'output'

# Cargar datos
datos_delitos <- read.csv(file.path(instub,"dataset_completo_2019_2023_limpio.csv"))

# Chequear no outliers en columna "cantidad" -----------------------------------
resumen_cantidad <- datos_delitos %>%
  summarise(
    Total = sum(cantidad, na.rm = TRUE),
    Media = mean(cantidad, na.rm = TRUE),
    Desvio = sd(cantidad, na.rm = TRUE)
  )
print(resumen_cantidad)

# Explorar principales categorías de cada columna ------------------------------
sort(table(datos_delitos$anio), decreasing = TRUE)
sort(table(datos_delitos$mes), decreasing = TRUE)
sort(table(datos_delitos$dia), decreasing = TRUE)
sort(table(datos_delitos$franja), decreasing = TRUE)
sort(table(datos_delitos$tipo), decreasing = TRUE)
sort(table(datos_delitos$subtipo), decreasing = TRUE)
sort(table(datos_delitos$uso_arma), decreasing = TRUE)
sort(table(datos_delitos$uso_moto), decreasing = TRUE)
sort(table(datos_delitos$barrio), decreasing = TRUE)
sort(table(datos_delitos$comuna), decreasing = TRUE)
table(datos_delitos$cantidad)

# Mostrar el resultado 
print(resumen_cantidad)


# Explorar tendencia temporal 2019-2023 ----------------------------------------
# Agrupar por año y mes
datos_delitos$fecha <- as.Date(datos_delitos$fecha)

datos_delitos <- datos_delitos %>%
  mutate(
    anio = year(fecha),
    mes = month(fecha),
    año_mes = as.Date(fecha) 
  )


delitos_mensuales <- datos_delitos %>%
  group_by(año_mes) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  arrange(año_mes)

# Graficar
ggplot(delitos_mensuales, aes(x = año_mes, y = total_delitos)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  labs(
    title = "Evolución mensual de delitos en CABA (2019–2023)",
    x = "Fecha",
    y = "Cantidad de delitos"
  ) +
  theme_minimal()

# Explorar estacionalidad ------------------------------------------------------

# Agrupar por mes
datos_delitos <- datos_delitos %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) 

# Filtrar 2020, ya que en el análisis anterior parecía un año con tendencia particular (inusual)
datos_filtrados <- datos_delitos %>%
  filter(year(fecha) != 2020) %>%
  mutate(mes = month(fecha, label = TRUE, abbr = FALSE))

# Agrupar y sumar
delitos_mensual_sin_2020 <- datos_filtrados %>%
  group_by(mes) %>%
  summarise(total_delitos = sum(cantidad), .groups = "drop")

# Graficar
ggplot(delitos_mensual_sin_2020, aes(x = mes, y = total_delitos)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    title = "Total de delitos por mes (sin 2020)",
    x = "Mes",
    y = "Cantidad de delitos"
  ) +
  theme_minimal()




