#EXPLICACIÓN DE LA ORGANIZACIÓN DEL CÓDIGO:

#EL CÓDIGO SE DIVIDE EN DOS PARTES

#PARTE 1: Código con el cual se realizó el pre-procesamiento y se generaron los csv individuales 
#por cada variable de modo que se pudiese trabajr de mejor manera.

#PARTE 2: Sección en la cual a partir de los csv individuales se realizó el análisis estadístico
#de cada una de las variables

#EXPLICACIÓN PARTE 2:

## El código se encuentra dividido por variables es decir, cada sección tiene su título y
## debajo sus respectivos análisis. En la parte inferior se encuentran unos análisis independientes
## que no corresponden a ninguna variable en específico.

#PARTE 1

Instalar y cargar librerías necesarias
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
#Se introduce el código de la actividad a la cual se le quiera generar el csv individual
generar_csv_actividad_solicitada('P1144S1')
resultsCsv <- fread("datos_P1144S1.csv")
prom <- mean(resultsCsv$Tiempo_Minutos)
# Mostrar mensaje de finalización
cat("Archivos generados: 'resumen_actividades_corregido.csv' y datos individuales para las top 5 actividades.\n")





#PARTE 2

###ANALISIS PARA CSV DEL TIEMPO DE COMIDAS DE LAS PERSONAS

#Ruta donde se encuentra el CSV  
setwd("C:/Users/aleja/Downloads")

#Nombre del CSV
datos_comer <- read.csv("datos_P1144S2.csv")

##ANALISIS DE ESTADÍSTICAS BÁSICAS

summary(datos_comer$Tiempo_Minutos)
sd(datos_comer$Tiempo_Minutos)
range(datos_comer$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_comer$Tiempo_Minutos), decreasing = TRUE)[1]))
moda



##Gráfico de barras acerca de los tiempos que la gente se demora comiendo

# Crear categorías
datos_comer$categoria <- cut(datos_comer$Tiempo_Minutos, 
                       breaks = c(0, 15, 30, 45, Inf), 
                       labels = c("Más de 15 min", "15-30 min", "30-45 min", "Más de 45 min"))

# Graficar
barplot(table(datos_comer$categoria), 
        main = "Categorías de Tiempo de Comida", 
        ylab = "Número de personas", 
        col = c("#C2FFC7", "#9EDF9C", "#62825D", "#526E48"))


##Boxplot para identificar outliers
boxplot(datos_comer$Tiempo_Minutos, 
        main = "Boxplot del Tiempo Dedicado a Comer", 
        ylab = "Tiempo (minutos)", 
        col = "#3498DB")  # Azul celeste

##Boxplot en el que se ajusta el rango del eje Y

boxplot(datos_comer$Tiempo_Minutos, 
        main = "Boxplot del Tiempo Dedicado a Comer", 
        ylab = "Tiempo (minutos)", 
        col = "#3498DB", 
        ylim = c(0, 150))  # Ajusta el valor según tus datos para mejorar la visualización



#Histograma de distribución del tiempo de sueño
hist(datos_comer$Tiempo_Minutos, 
     main = "Distribución del Tiempo de comidas", 
     xlab = "Tiempo de comidas (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_comer$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a comer", 
                   xlab = "Tiempo de comer (minutos)", 
                   ylab = "Número de personas", 
                   col = "lightgreen", 
                   border = "black", 
                   breaks = 5,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a comer", 
     xlab = "Tiempo de comer (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea




##Histograma más ilustrativo
# Crear un histograma con más barras y ajustes en el eje X
hist(datos_comer$Tiempo_Minutos, 
     breaks = 20,  # Incrementa el número de barras dividiendo en más intervalos
     main = "Distribución del Tiempo dedicado a comer", 
     xlab = "Tiempo de comer (minutos)", 
     ylab = "Frecuencia (# Personas)", 
     col = "lightgreen", 
     border = "black", 
     xaxt = "n")  # Suprimir el eje X predeterminado

# Redibujar el eje X con valores consecutivos
axis(1, at = seq(0, max(datos_comer$Tiempo_Minutos), by = 25))



####ANALISIS PARA EL CSV DEL TIEMPO DE SUEÑO
setwd("C:/Users/aleja/Downloads")


datos_dormir <- read.csv("C:/Users/aleja/Downloads/datos_P1144S1.csv")



#Cálculo de estadísticas básicas
summary(datos_dormir$Tiempo_Minutos)
sd(datos_dormir$Tiempo_Minutos)
range(datos_dormir$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_dormir$Tiempo_Minutos), decreasing = TRUE)[1]))
moda


#Histograma de distribución del tiempo de sueño
hist(datos_dormir$Tiempo_Minutos, 
     main = "Distribución del Tiempo dedicado a dormir", 
     xlab = "Tiempo de sueño (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_dormir$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a dormir", 
                   xlab = "Tiempo de sueño (minutos)", 
                   ylab = "Número de personas", 
                   col = "skyblue", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a dormir", 
     xlab = "Tiempo de sueño (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea

##Análisis para ver a qué distribución se ajusta más si a la normal o la exponencial
#install.packages("fitdistrplus")
library(fitdistrplus)
ajuste_normal <- fitdist(datos_dormir$Tiempo_Minutos, "norm")
ajuste_exponencial <- fitdist(datos_dormir$Tiempo_Minutos, "exp")
gofstat(list(ajuste_normal, ajuste_exponencial), fitnames = c("Normal", "Exponencial"))

###AGRUPACIÓN DE DATOS DEL SUEÑO POR CATEGORÍAS


# Paso 2: Crear rangos de 60 minutos para la columna Tiempo_Minutos
datos_sueño$categoria <- cut(datos_sueño$Tiempo_Minutos, 
                             breaks = seq(0, max(datos_sueño$Tiempo_Minutos), by = 60), 
                             labels = paste(seq(0, max(datos_sueño$Tiempo_Minutos) - 60, by = 60), 
                                            "-", 
                                            seq(60, max(datos_sueño$Tiempo_Minutos), by = 60)),
                             right = FALSE)  # El límite superior no se incluye

# Paso 3: Contar cuántas personas están en cada categoría
tabla_categoria <- table(datos_sueño$categoria)

# Excluir categorías con menos de 1000 personas
tabla_categoria_filtrada <- tabla_categoria[tabla_categoria >= 1000]

# Paso 4: Crear gráfico de torta
colores <- rainbow(length(tabla_categoria_filtrada))  # Colores para las secciones

# Usar `par()` para dividir la ventana gráfica
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))  # Divide en 2 columnas

# Crear gráfico de torta
pie(tabla_categoria_filtrada, 
    main = "Distribución del Tiempo de Sueño 
    por Rangos de 60 Minutos", 
    col = colores,  # Colores para las secciones
    labels = NA)     # No mostrar nombres en los segmentos de la torta

# Crear la tabla al lado del gráfico de torta
# Crear un gráfico vacío para la tabla
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, length(tabla_categoria_filtrada)), axes = FALSE)

# Escribir los intervalos con los colores al lado del gráfico
for (i in 1:length(tabla_categoria_filtrada)) {
  rect(0, length(tabla_categoria_filtrada) - i, 0.1, length(tabla_categoria_filtrada) - i + 1, col = colores[i], border = "white")  # Rectángulo con el color
  text(0.15, length(tabla_categoria_filtrada) - i + 0.5, paste(names(tabla_categoria_filtrada)[i], ":", tabla_categoria_filtrada[i]), pos = 4)
}



####ANALISIS PARA VER TELEVISIÓN SIN HACER OTRA ACTIVIDAD

setwd("C:/Users/aleja/Downloads")


datos_television <- read.csv("datos_P1110S1.csv")

###Cálculo de estadísticas básicas
summary(datos_television$Tiempo_Minutos)
sd(datos_television$Tiempo_Minutos)
range(datos_television$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_television$Tiempo_Minutos), decreasing = TRUE)[1]))
moda


#Histograma de distribución del tiempo que las personas ven televisión
hist(datos_television$Tiempo_Minutos, 
     main = "Distribución del Tiempo dedicado a ver TV", 
     xlab = "Tiempo de TV (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_television$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a ver TV", 
                   xlab = "Tiempo de sueño (minutos)", 
                   ylab = "Número de personas", 
                   col = "skyblue", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a ver TV", 
     xlab = "Tiempo de televisión (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea


####ANALISIS PARA CHARLAR CON AMIGOS O FAMILIARES
setwd("C:/Users/aleja/Downloads")


datos_charlar <- read.csv("datos_P1113S2.csv")

###Cálculo de estadísticas básicas
summary(datos_charlar$Tiempo_Minutos)
sd(datos_charlar$Tiempo_Minutos)
range(datos_charlar$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_charlar$Tiempo_Minutos), decreasing = TRUE)[1]))
moda


#Histograma de distribución del tiempo de charlar
hist(datos_charlar$Tiempo_Minutos, 
     main = "Distribución del Tiempo de charlar con amigos", 
     xlab = "Tiempo de charlar (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")

# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_charlar$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a charlar", 
                   xlab = "Tiempo de charlar (minutos)", 
                   ylab = "Número de personas", 
                   col = "skyblue", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a charlar", 
     xlab = "Tiempo de charlar (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea



####ANALISIS ASISTIR A BARES
setwd("C:/Users/aleja/Downloads")


datos_bares <- read.csv("datos_P1113S1.csv")

###Cálculo de estadísticas básicas
summary(datos_bares$Tiempo_Minutos)
sd(datos_bares$Tiempo_Minutos)
range(datos_bares$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_bares$Tiempo_Minutos), decreasing = TRUE)[1]))
moda



####ANALISIS HACER DEPORTE.
setwd("C:/Users/aleja/Downloads")


datos_deporte <- read.csv("datos_P1112S1.csv")

###Cálculo de estadísticas básicas
summary(datos_deporte$Tiempo_Minutos)
sd(datos_deporte$Tiempo_Minutos)
range(datos_deporte$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_deporte$Tiempo_Minutos), decreasing = TRUE)[1]))
moda



#Histograma de distribución del tiempo de deportes
hist(datos_deporte$Tiempo_Minutos, 
     main = "Distribución del Tiempo de hacer deporte", 
     xlab = "Tiempo de deporte (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")

# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_deporte$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a hacer deporte", 
                   xlab = "Tiempo de deporte (minutos)", 
                   ylab = "Número de personas", 
                   col = "lightgreen", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a hacer deporte", 
     xlab = "Tiempo de deporte (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea


##Histograma más ilustrativo
# Crear un histograma con más barras y ajustes en el eje X
hist(datos_deporte$Tiempo_Minutos, 
     breaks = 20,  # Incrementa el número de barras dividiendo en más intervalos
     main = "Distribución del Tiempo dedicado a deporte", 
     xlab = "Tiempo de deporte (minutos)", 
     ylab = "Frecuencia (# Personas)", 
     col = "lightgreen", 
     border = "black", 
     xaxt = "n")  # Suprimir el eje X predeterminado

# Redibujar el eje X con valores consecutivos
axis(1, at = seq(0, max(datos_deporte$Tiempo_Minutos), by = 25))


####ANALISIS LEER LIBROS O REVISTAS.
setwd("C:/Users/aleja/Downloads")


datos_leer <- read.csv("datos_P1110S8.csv")

###Cálculo de estadísticas básicas
summary(datos_leer$Tiempo_Minutos)
sd(datos_leer$Tiempo_Minutos)
range(datos_leer$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_leer$Tiempo_Minutos), decreasing = TRUE)[1]))
moda

#Histograma de distribución del tiempo de leer
hist(datos_leer$Tiempo_Minutos, 
     main = "Distribución del Tiempo dedicado a leer", 
     xlab = "Tiempo de lectura (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_leer$Tiempo_Minutos, 
                   main = "Distribución del Tiempo dedicado a leer", 
                   xlab = "Tiempo de lectura (minutos)", 
                   ylab = "Número de personas", 
                   col = "lightgreen", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$counts,        # Medias de los intervalos
     histograma$mids,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo dedicado a leer", 
     xlab = "Tiempo de lectura (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea


##Histograma más ilustrativo
# Crear un histograma con más barras y ajustes en el eje X
hist(datos_leer$Tiempo_Minutos, 
     breaks = 20,  # Incrementa el número de barras dividiendo en más intervalos
     main = "Distribución del Tiempo dedicado a leer", 
     xlab = "Tiempo de lectura (minutos)", 
     ylab = "Frecuencia (# Personas)", 
     col = "lightgreen", 
     border = "black", 
     xaxt = "n")  # Suprimir el eje X predeterminado

# Redibujar el eje X con valores consecutivos
axis(1, at = seq(0, max(datos_leer$Tiempo_Minutos), by = 25))


####ANALISIS NAVEGAR POR INTERNET.
setwd("C:/Users/aleja/Downloads/")


datos_internet <- read.csv("datos_P1110S5.csv;,")

###Cálculo de estadísticas básicas
summary(datos_internet$Tiempo_Minutos)
sd(datos_internet$Tiempo_Minutos)
range(datos_internet$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_internet$Tiempo_Minutos), decreasing = TRUE)[1]))
moda


#Histograma de distribución del tiempo de navegar en internet
hist(datos_internet$Tiempo_Minutos, 
     main = "Distribución del Tiempo de navegación en internet", 
     xlab = "Tiempo de internet (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_internet$Tiempo_Minutos, 
                   main = "Distribución del Tiempo navegando en internet", 
                   xlab = "Tiempo de navegación (minutos)", 
                   ylab = "Número de personas", 
                   col = "skyblue", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo navegando en internet", 
     xlab = "Tiempo de navegación (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea


##Histograma más ilustrativo
# Crear un histograma con más barras y ajustes en el eje X
hist(datos_internet$Tiempo_Minutos, 
     breaks = 20,  # Incrementa el número de barras dividiendo en más intervalos
     main = "Distribución del Tiempo dedicado a internet", 
     xlab = "Tiempo de lectura (minutos)", 
     ylab = "Frecuencia (# Personas)", 
     col = "lightgreen", 
     border = "black", 
     xaxt = "n")  # Suprimir el eje X predeterminado

# Redibujar el eje X con valores consecutivos
axis(1, at = seq(0, max(datos_internet$Tiempo_Minutos), by = 25))

####ANALISIS ESCUCHAR MÚSICA HACIENDO OTRAS ACTIVIDADES

setwd("C:/Users/aleja/Downloads")


datos_musica1 <- read.csv("datos_P1110S4.csv")

###Cálculo de estadísticas básicas
summary(datos_musica1$Tiempo_Minutos)
sd(datos_musica1$Tiempo_Minutos)
range(datos_musica1$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_musica1$Tiempo_Minutos), decreasing = TRUE)[1]))
moda

####ANALISIS ESCUCHAR MÚSICA SIN HACER OTRAS ACTIVIDADES

setwd("C:/Users/aleja/Downloads")


datos_musica2 <- read.csv("datos_P1110S3.csv;,")

###Cálculo de estadísticas básicas
summary(datos_musica2$Tiempo_Minutos)
sd(datos_musica2$Tiempo_Minutos)
range(datos_musica2$Tiempo_Minutos)

##Cálculo dela moda
moda <- as.numeric(names(sort(table(datos_musica2$Tiempo_Minutos), decreasing = TRUE)[1]))
moda



#Histograma de distribución del tiempo de escuchar música
hist(datos_musica2$Tiempo_Minutos, 
     main = "Distribución del Tiempo de escuchar música", 
     xlab = "Tiempo de música (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "lightgreen", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_musica2$Tiempo_Minutos, 
                   main = "Distribución del Tiempo de escuchar música", 
                   xlab = "Tiempo de música (minutos)", 
                   ylab = "Número de personas", 
                   col = "skyblue", 
                   border = "black", 
                   breaks = 10,    # Número de intervalos
                   plot = FALSE)   # No graficar el histograma aún

# Graficar la distribución con líneas
plot(histograma$mids,        # Medias de los intervalos
     histograma$counts,      # Frecuencia (número de personas) en cada intervalo
     type = "o",             # 'o' para gráfico de línea con puntos
     main = "Distribución del Tiempo de música", 
     xlab = "Tiempo de música (minutos)", 
     ylab = "Número de personas", 
     col = "black", 
     pch = 16,               # Estilo de los puntos
     lwd = 2)                # Grosor de la línea




####Diagrama de barras con los promedios de cada actividad


tabla_q3_actividades <- data.frame(
  Actividad = c("Dormir", "Comer", "Leer", "Internet", "Música", "Charlar","Otro"),
  Promedio = c(496.1, 58.64, 58.14, 99.86,107.1,125.6,494.56)
)

library(ggplot2)

ggplot(tabla_q3_actividades, aes(x = Actividad, y = Promedio, fill = Actividad)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de tiempo por Actividad", x = "Actividad", y = "Promedio(Minutos)") +
  theme_minimal()
# Cálculo de porcentajes
tabla_q3_actividades$Porcentaje <- (tabla_q3_actividades$Promedio / sum(tabla_q3_actividades$Promedio)) * 100

# Diagrama de torta con porcentajes
ggplot(tabla_q3_actividades, aes(x = "", y = Promedio, fill = Actividad)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Borde negro
  coord_polar("y", start = 0) +
  labs(title = "Distribución del Tiempo en un día") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold")


#Suma de los promedios 945.44

