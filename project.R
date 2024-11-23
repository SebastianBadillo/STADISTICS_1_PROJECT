datos <- read.csv("Datos_Estudiantes_Taller1_2024-2.csv")
print(datos$Edades)
Edades <- datos$Edades
Alturas <- datos$Alturas
Pesos <- datos$Pesos

####################################################################
####################################################################

# Hallar medidas de centralización (datos sueltos y agrupados en intervalos): media aritmética, moda y mediana.

######## SIN AGRUPAR

#### Edades 
# Media aritmética
media_edades <- mean(Edades)
# Moda 
moda_edades <- as.numeric(names(sort(table(Edades), decreasing = TRUE)[1]))
# mediana
mediana_edades <- median(Edades)

#### Alturas
# Media aritmética
media_alturas <- mean(Alturas)
# Moda 
moda_alturas <- as.numeric(names(sort(table(Alturas), decreasing = TRUE)[1]))
# mediana
mediana_alturas <- median(Alturas)

#### Pesos
# Media aritmética
media_pesos <- mean(Pesos)
# Moda 
moda_pesos <- as.numeric(names(sort(table(Pesos), decreasing = TRUE)[1]))
# mediana
mediana_pesos <- median(Pesos)

###########     AGRUPADOS
###### Edades
intervalos_edades <- cut(Edades, breaks = seq(17, 39, 3)) # usa 17 y 39 para ajuste de intervalos
tabla_frecuencia_edades <- table(intervalos_edades)
marcas_clase_edades <- (seq(17,35,3) + seq(20,38,3))/2
frecuencias_edades <- as.numeric(tabla_frecuencia_edades)
### Media agrupada 
media_agrupada_edades <- sum(marcas_clase_edades * frecuencias)/ sum(frecuencias)

###### Moda
intervalo_modal_edades <-which.max(frecuencias_edades)
# Obtener valores necesarios para la fórmula de la moda
L_inferior_intervalo_modal_edades <- seq(17, 35, by = 3)[intervalo_modal_edades]  # Límite inferior del intervalo modal
f_m_edades <- frecuencias[intervalo_modal_edades]       # Frecuencia del intervalo modal
amplitud_edades <- 5                                    # Amplitud del intervalo

# Frecuencias de los intervalos anterior y siguiente al modal
f_m_1 <- ifelse(intervalo_modal_edades == 1, 0, frecuencias[intervalo_modal_edades - 1])  # Frecuencia anterior
f_m_2 <- ifelse(intervalo_modal_edades == length(frecuencias), 0, frecuencias[intervalo_modal_edades + 1])  # Frecuencia siguiente

moda_agrupada_edades <- L_inferior_intervalo_modal_edades + ((f_m_edades - f_m_1) / ((f_m_edades - f_m_1) + (f_m_edades - f_m_2))) * amplitud_edades

##### Mediana
frecuencia_acumulada_edades <- cumsum(frecuencias_edades)
N <- sum(frecuencias_edades)
# Determinar el punto medio dependiendo de si N es par o impar
punto_medio_mediana_edades <- ifelse(N %% 2 == 0, N / 2, (N + 1) / 2)

intervalo_mediana_edades <- which(frecuencia_acumulada_edades >= punto_medio_mediana_edades)[1]

# Obtener los valores necesarios para la fórmula de la mediana
Limite_inferior_mediana_edades <- seq(17, 35, by = 3)[intervalo_mediana_edades]  # Límite inferior del intervalo mediano
F_anterior <- ifelse(intervalo_mediana_edades == 1, 0, frecuencia_acumulada[intervalo_mediana - 1]) # Frecuencia acumulada anterior
f_mediana <- frecuencias[intervalo_mediana_edades]  # Frecuencia del intervalo mediano
c <- 5  # Amplitud del intervalo

# Calcular la mediana agrupada
mediana_agrupada <- Limite_inferior_mediana_edades + ((punto_medio - F_anterior) / f_mediana) * c


