# Instalar y cargar librerías necesarias
install.packages("data.table")
install.packages("readr")
install.packages("vroom")
library(data.table)

# 1. Cargar los datos
# --------------------
# Leer archivo CSV y cargar los datos en un data table
datos <- fread("Capitulo_I.csv")

# 2. Limpiar valores NA
# --------------------
# Nota: No se deben reemplazar los datos NA por 0 directamente en todo el dataset.
# Esto podría causar problemas en el análisis. Ejemplo de lo que **no se debe hacer**:
## datos[is.na(datos)] <- 0 

# 3. Identificar columnas de actividades y ajustar el dataset
# --------------------
# Identificar columnas relacionadas con horas (terminadas en A1) y minutos (terminadas en A2)
actividades_horas <- grep("A1$", names(datos), value = TRUE) # Columnas con horas
actividades_minutos <- grep("A2$", names(datos), value = TRUE) # Columnas con minutos

# Verificar el número de columnas identificadas
cat("Número de columnas terminadas en A1 (horas):", length(actividades_horas), "\n")
cat("Número de columnas terminadas en A2 (minutos):", length(actividades_minutos), "\n")

# 3.1. Eliminar columnas innecesarias del dataset
# --------------------
# Definir columnas específicas a eliminar (listas únicas)
columnas_unicas <- c(
  "DIRECTORIO", "ORDEN", "SECUENCIA_P", 
  "P1143S5", "P1142S5", "P1136S7", "P1141S5", "P1140S8", 
  "P1137S4", "P1114S5", "P1097", "P1096", "P1095", "P1094", 
  "P1113S3", "P1112S3", "P1111S6", "P1110S9"
)

# Definir rangos de columnas a eliminar
rangos <- list(
  c("P1139S1", "P1138S4"),
  c("P1137S5", "P1131S4"),
  c("P1126S7", "P1126S7A3"),
  c("P1125S7", "P1125S7A3"),
  c("P1122", "F_EXP")
)

# Crear una lista de columnas para eliminar con base en los rangos
columnas_rango <- unlist(lapply(rangos, function(rango) {
  names(datos)[which(names(datos) == rango[1]):which(names(datos) == rango[2])]
}))

# Combinar columnas únicas y de rango para eliminarlas
columnas_a_eliminar <- unique(c(columnas_unicas, columnas_rango))

# Eliminar las columnas seleccionadas
datos <- datos[, !(names(datos) %in% columnas_a_eliminar), with = FALSE]

# 3.2. Confirmar que las columnas de actividades coinciden
# --------------------
# Actualizar listas de columnas con horas (A1) y minutos (A2)
actividades_horas <- grep("A1$", names(datos), value = TRUE)
actividades_minutos <- grep("A2$", names(datos), value = TRUE)

# Validar si las bases de las columnas de horas (A1) y minutos (A2) coinciden
coinciden <- all(sub("A1$", "", actividades_horas) == sub("A2$", "", actividades_minutos))

# Mostrar resultado de la validación
if (coinciden) {
  cat("Todas las columnas de horas (A1) tienen un par correspondiente de minutos (A2).\n")
} else {
  cat("Hay desajustes entre las columnas de horas (A1) y minutos (A2).\n")
}

# Guardar el dataset ajustado
write.csv(datos, "datos_limpios.csv", row.names = FALSE)

# 4. Unificar columnas de horas y minutos
# --------------------
# Crear una nueva columna con el tiempo total en minutos para cada actividad
for (i in seq_along(actividades_horas)) {
  actividad <- sub("A1$", "", actividades_horas[i])  # Extraer nombre base de la actividad
  
  # Manejar valores NA para los cálculos: reemplazar por 0 temporalmente
  horas <- ifelse(is.na(datos[[actividades_horas[i]]]), 0, datos[[actividades_horas[i]]])
  minutos <- ifelse(is.na(datos[[actividades_minutos[i]]]), 0, datos[[actividades_minutos[i]]])
  
  # Calcular el tiempo total en minutos
  datos[[paste0(actividad, "_total_min")]] <- horas * 60 + minutos
}

# Eliminar columnas originales de horas y minutos
datos <- datos[, !(names(datos) %in% c(actividades_horas, actividades_minutos)), with = FALSE]

# Guardar los datos procesados
write.csv(datos, "datos_procesados.csv", row.names = FALSE)

# Verificar la estructura final del dataset
print("Estructura final del dataset:")
print(head(datos))

# 5. Manejo de datos y resumen de actividades
# --------------------
# Cargar el archivo procesado
datos <- fread("datos_procesados.csv")

# Identificar columnas que contienen "_total_min"
columnas_tiempo <- grep("_total_min$", names(datos), value = TRUE)

# Crear tabla para almacenar resultados
resultados <- data.table(Actividad = character(), Promedio_Tiempo = numeric(), Numero_Personas = integer())

# Calcular estadísticas por actividad
for (col in columnas_tiempo) {
  promedio <- mean(datos[[col]][datos[[col]] > 0], na.rm = TRUE) # Promedio ignorando ceros y NA
  num_personas <- sum(!is.na(datos[[col]]) & datos[[col]] > 0)  # Número de personas con datos válidos
  
  # Agregar los resultados a la tabla
  resultados <- rbind(resultados, data.table(
    Actividad = sub("_total_min$", "", col),
    Promedio_Tiempo = promedio,
    Numero_Personas = num_personas
  ))
}

# Guardar resumen general de actividades
fwrite(resultados, "resumen_actividades_corregido.csv", sep = ";", dec = ",")

# 6. Generar archivos para las 5 actividades con más tiempo promedio
# --------------------
# Ordenar actividades por cantidad de datos
resultados <- resultados[order(-Numero_Personas)]

# Identificar las 5 actividades con mayor cantidad de datos
top_5_actividades <- resultados[1:5, Actividad]

# Crear un archivo CSV para cada actividad
for (actividad in top_5_actividades) {
  columna <- paste0(actividad, "_total_min") # Nombre de la columna correspondiente
  datos_filtrados <- datos[!is.na(get(columna)) & get(columna) > 0, .(Tiempo_Minutos = get(columna))]
  
  # Guardar los datos filtrados en un archivo
  fwrite(datos_filtrados, paste0("datos_", actividad, ".csv", sep = ";", dec = ","))
}

# Funcion para generar CSV de una columna especifica

generar_csv_actividad_solicitada <- function(actividad){
  columna <- paste0(actividad, "_total_min")
  
  # Validar si la columna existe en el dataset
  if (!columna %in% names(datos)){
    cat("La columna correspondiente a la actividad no existe en los datos procesados.\n")
    return(NULL)
  }
  print(columna)
  datos_filtrados <- datos[!is.na(get(columna)) & get(columna) > 0, .(Tiempo_Minutos = get(columna))]
  print(datos_filtrados)
  # Verificar si hay datos disponibles después del filtrado
  if (nrow(datos_filtrados) == 0) {
    cat("No hay datos válidos para la actividad especificada.\n")
    return(NULL)
  }
  # Guardar los datos filtrados en un archivo CSV
  nombre_archivo <- paste0("datos_", actividad, ".csv")
  fwrite(datos_filtrados, nombre_archivo, sep = ";", dec = ",")
  
  # Mensaje de éxito
  cat("Archivo generado exitosamente:", nombre_archivo, "\n")
}
generar_csv_actividad_solicitada('P1144S1')
resultsCsv <- fread("datos_P1144S1.csv")
prom <- mean(resultsCsv$Tiempo_Minutos)
# Mostrar mensaje de finalización
cat("Archivos generados: 'resumen_actividades_corregido.csv' y datos individuales para las top 5 actividades.\n")
