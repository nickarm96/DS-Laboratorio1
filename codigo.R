# Practica 1
# Data Science
# Grupo 5

epa_http <- read_table("C:/Users/auguriora/Downloads/epa-http/epa-http.csv",
col_names = FALSE)

# renombrar columnas

names(epa_http) <- c("ORIGEN","FECHA","TIPO","RUTA","PROTOCOLO","PETICIONES","BYTES")

# Pregunta 1:

# 1. Cuales son las dimensiones del dataset cargado (número de filas y columnas)

dimensiones <- dim(epa_http)
num_filas <- dimensiones[1] 
num_columnas <- dimensiones[2]

# Mostrar las dimensiones
cat("El dataset tiene", num_filas, "filas y", num_columnas, "columnas.\n")



# 2. Valor medio de la columna Bytes

epa_http$bytes_sent <- as.numeric(epa_http$BYTES) 

unique(epa_http$BYTES) 

valor_medio_bytes <- mean(epa_http$bytes_sent,na.rm = TRUE) 



# Pregunta 2:
# De las diferentes IPs de origen accediendo al servidor, ¿cuantas pertenecen a una IP claramente educativa (que contenga ".edu")?

cantidad_edu <- sum(grepl(".edu",epa_http$ORIGEN, fixed = TRUE))
print(cantidad_edu)



# Pregunta 3:
# De todas las peticiones recibidas por el servidor cual es la hora en la que hay mayor volumen de peticiones HTTP de tipo "GET"?

epa_http$FECHA <- gsub("\\[|\\]", "", epa_http$FECHA)

epa_http <- tidyr::separate(epa_http, FECHA, c("DIA","H","M","S"), ":")
epa_http <- tidyr::unite(epa_http, "HORA", H, M, S, sep = ":")

epa_http$HORA <- as.POSIXct(epa_http$HORA, tz = "", format = "%H:%M:%S")


horas <- table(datos$DIA,datos$HORA)
hora_max <- names(horas)[horas == max(horas)]


epa_http$TIPO <- gsub("\"", "", epa_http$TIPO)
peticiones_GET <- epa_http %>%
  filter(TIPO == "GET")


# Extraer la hora de las peticiones "GET"
horas_GET <- hour(peticiones_GET$HORA)

# Contar las ocurrencias de cada hora
frecuencia_horas <- table(horas_GET)

# Encontrar la hora con mayor volumen de peticiones "GET"
hora_max_peticiones <- names(frecuencia_horas)[which.max(frecuencia_horas)]

# Mostrar la hora con mayor volumen de peticiones "GET"
print(paste("La hora con mayor volumen de peticiones GET es:", hora_max_peticiones,":00 horas"))


