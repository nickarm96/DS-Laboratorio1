# Practica 1
# Data Science
# Grupo 5

epa_http <- read_table("C:/Users/auguriora/Downloads/epa-http/epa-http.csv",
col_names = FALSE)

# renombrar columnas

names(epa_http) <- c("ORIGEN","FECHA","TIPO","URL","PROTOCOLO","PETICIONES","BYTES")

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



# Agrupar peticiones "GET"
epa_http$TIPO <- gsub("\"", "", epa_http$TIPO)
peticiones_GET <- epa_http %>%
  filter(TIPO == "GET")


HORA <- dplyr::select(peticiones_GET, HORA) %>% mutate(day(HORA), hour(HORA))

colnames(HORA) <- c('tiempo', 'dia', 'hora')

HORA_P3 <- HORA %>% group_by(hora) %>% mutate(hora) %>% summarise(n = n())

HORA_P3 <- HORA %>% group_by(dia, hora) %>% mutate(dia, hora) %>% summarise(n = n())

# Encontrar la hora con mayor volumen de peticiones "GET"

select(HORA_P3$hora max(HORA_P3$n)
       
hora_maxima <- HORA_P3[HORA_P3$n == max(HORA_P3$n), "hora"]

hora_maxima



# Pregunta 4:
# De las peticiones hechas por instituciones educativas (.edu), ¿Cuantos bytes en total se han transmitido, en peticiones de descarga de ficheros de texto ".txt"?

grep(".edu", epa_http$ORIGEN) 

aa <- dplyr::filter(epa_http,  grepl(".edu", ORIGEN, ignore.case = TRUE), grepl(".txt",URL, ignore.case = TRUE)) 
aa$BYTES <- as.numeric(aa$BYTES)

cantidad_bytes_txt <- sum(as.numeric(aa$BYTES),na.rm = TRUE)

print(paste("Total de bytes transmitidos en ficheros de texto:", cantidad_bytes_txt))




# Pregunta 5:
# Si separamos la petición en 3 partes (Tipo, URL, Protocolo), usando str_split y el separador " " (espacio), ¿cuantas peticiones buscan directamente la URL = "/"?

peticion_p5 <- dplyr::filter(epa_http, grepl("^/$", URL, ignore.case = TRUE)) 

cantidad_peticiones_p5 <- nrow(peticion_p5)



# Pregunta 6:
# Aprovechando que hemos separado la petición en 3 partes (Tipo, URL, Protocolo) ¿Cuantas peticiones NO tienen como protocolo "HTTP/0.2"

epa_http$PROTOCOLO <- gsub('"', "", epa_http$PROTOCOLO)

peticion_p6 <- dplyr::filter(epa_http[!grepl("HTTP/0.2", epa_http$PROTOCOLO), ])
  
cantidad_peticiones_p6 <- nrow(peticion_p6)
