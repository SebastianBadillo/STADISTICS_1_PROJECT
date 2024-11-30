#Ruta donde se encuentra el CSV  
setwd("C:/Users/aleja/Downloads")

#Nombre del CSV
datos <- read.csv("datos_P1144S2.csv")

head(datos)         # Muestra las primeras filas
colnames(datos)     # Lista los nombres de las columnas


###ANALISIS PARA CSV DEL TIEMPO DE COMIDAS DE LAS PERSONAS


##ANALISIS DE ESTADÍSTICAS BÁSICAS

summary(datos$Tiempo_Minutos)
sd(datos$Tiempo_Minutos)
range(datos$Tiempo_Minutos)

##En promedio la gente gasta menos de una hora en comer



##Gráfico de barras acerca de los tiempos que la gente se demora comiendo

# Crear categorías
datos$categoria <- cut(datos$Tiempo_Minutos, 
                       breaks = c(0, 15, 30, 45, Inf), 
                       labels = c("Menos de 15 min", "15-30 min", "30-45 min", "Más de 45 min"))

# Graficar
barplot(table(datos$categoria), 
        main = "Categorías de Tiempo de Comida", 
        ylab = "Número de personas", 
        col = c("#C2FFC7", "#9EDF9C", "#62825D", "#526E48"))


##Boxplot para identificar outliers
boxplot(datos$Tiempo_Minutos, 
        main = "Boxplot del Tiempo Dedicado a Comer", 
        ylab = "Tiempo (minutos)", 
        col = "#3498DB")  # Azul celeste

##Boxplot en el que se ajusta el rango del eje Y

boxplot(datos$Tiempo_Minutos, 
        main = "Boxplot del Tiempo Dedicado a Comer", 
        ylab = "Tiempo (minutos)", 
        col = "#3498DB", 
        ylim = c(0, 100))  # Ajusta el valor según tus datos para mejorar la visualización



#Histograma Distribución del Tiempo

hist(datos$Tiempo_Minutos, 
     main = "Distribución del Tiempo dedicado a comer", 
     xlab = "Tiempo de comida (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "skyblue", 
     border = "black")




####ANALISIS PARA EL CSV DEL TIEMPO DE SUEÑO
setwd("C:/Users/aleja/Downloads")


datos_sueño <- read.csv("datos_P1144S1.csv")

summary(datos_sueño$Tiempo_Minutos)
sd(datos_sueño$Tiempo_Minutos)
range(datos_sueño$Tiempo_Minutos)


#Histograma de distribución del tiempo de sueño
hist(datos_sueño$Tiempo_Minutos, 
     main = "Distribución del Tiempo dedicado a dormir", 
     xlab = "Tiempo de sueño (minutos)", 
     ylab = "Frecuencia(# Personas)", 
     col = "skyblue", 
     border = "black")


# Calcular el histograma para las frecuencias (cantidad de personas)
histograma <- hist(datos_sueño$Tiempo_Minutos, 
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


##INTENTO DE AGRUPAR DATOS


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
    main = "Distribución del Tiempo de Sueño por Rangos de 60 Minutos\n(excluyendo < 1000 personas)", 
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
