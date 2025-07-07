# Cargar librerías necesarias
library(tidyverse)
library(lubridate)  # Para manejo de fechas
library(scales)     # Para formateo de valores

# Rutas archivos 
instub <- 'raw'
outstub <- 'input'

# Cargar datos
datos_2019 <- read.csv(file.path(instub,"delitos_2019.csv"))
datos_2020 <- read.csv(file.path(instub,"delitos_2020.csv"))
datos_2021 <- read.csv(file.path(instub,"delitos_2021.csv"))
datos_2022 <- read.csv(file.path(instub,"delitos_2022.csv"))
datos_2023 <- read.csv(file.path(instub,"delitos_2023.csv"))

# Juntar datos en un solo dataframe
df_datos <- rbind(datos_2019, datos_2020, datos_2021, datos_2022, datos_2023)

# Guardar datos en archivo csv
write.csv(df_datos, file.path(outstub, "dataset_completo_2019_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")


