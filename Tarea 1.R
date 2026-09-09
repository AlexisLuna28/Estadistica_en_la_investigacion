# Ejercicio 3.1
# URL de los datos
url <- paste0("https://raw.githubusercontent.com/mgtagle/",
              "Estadistica_Investigacion_2026/refs/heads/main/", 
              "Datos/Crudos/cuadro1.csv")

# Importar los datos directamente desde GitHub
cuadro1 <- read.csv(url, header = TRUE)
str(cuadro1)

#Histograma de la variable diametro
hist(cuadro1$Diametro)

# Hipotesis nula: No existen diferencias en los diametros de los arboles
# Hipotesis alternativa: Los arboles si presentan diametros diferentes

# Prueba de normalidad
shapiro.test(cuadro1$Diametro)

# Media de la variable diametro
mean(cuadro1$Diametro)

# Desviacion estandar de la variable diametro
sd(cuadro1$Diametro)

# Prueba de una sola muestra

boxplot(cuadro1$Diametro)

# Prueba de t de una sola muestra 
t.test(cuadro1$Diametro, mu = 0.95)


# 1.- Formule la hipotesis nula (H0) y la hipotesis alternativa (H1)
# R= H0: No se presentan diferencias significativas en el diametro
# de los arboles de bosque templado
# R= H1: Se presenta diferencias significativas en el diámetro 
# de los arboles de bosque templado
  
# 2.- La pregunta planteada requiere una prueba de una cola o de dos colas)-¿
# R: Se requiere una prueba de dos colas, ya que solo se esta indicando que 
# se requiere determinar si hay diferencias significativas en el diametro (16.5)
# no especifica si es mayor o menor.
  
# 3.- Calcule la media y la desviación estándar de la variable Diametro

# 4.- Realizar la prueba de t: 
  
# 5.- Identificar en los resultados lo siguiente:
# Estadistico de t:  R= 32.526
# Grados de Libertad: R= 49
# Valor de p: R= <2.2e-16
# Media de la muestra: R= 15.794
# Intervalo de confianza del 95%: R= 14.97689 - 16.71111
  
# 6.- Con α =0,05, indique si se rechaza o no se rechaza H0. 
# R= Se rechaza la hipotesis nula (H0) ya que si existen 
# diferencias sifnigicativas en la variable diametro
  
# 7.- Redacte una conclusión en términos del diámetro medio de 
# los árboles del rodal
# R= 


#####

# Ejercicio 3.2 

# Importar datos a R
costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 
            81.94, 80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62,
            81.4, 74.89, 82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28,
            78.44, 79.01, 80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7,
            82.45, 77.29, 75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

# Media del peso neto medio de los costales
mean(costal)

#Desviacion estandar del peso neto medio de los costales
sd(costal)

# Prueba de t
t.test(costal, mu= 80, alternative = "less")

### Actividades

# 1.- Explique con sus propias palabras qué representan H0 y H1.
# Ho: No hay diferencia significativa en el peso medio de los costales, 
# el cual es igual a 80 kg.

# H1: Hay diferencia significativa en el peso medio de los costales, 
# resultando menor a 80 kg.

# 2.- ¿Por qué corresponde utilizar una prueba de una cola?  
# R= Se utiliza una prueba de una cola, ya que se especifica que se quiere 
# verificar que el peso neto medio de los costales es menor a 80 kg

# 3.- Calcule la media y desviación estándar de los 44 costales.
# media: R= 78.91068
# desviacion estandar: R= 3.056023

# 4.- Realice la prueba: 

# 5.- Identifique: 
# estadístico t: R= -2.3644
# grados de libertad: R= 43 
# valor de p: R= 0.01132 
# media observada: R= 78.91068

# 6.- Compare el valor de p con α = 0,05.

# ¿Se rechaza o no se rechaza H0? R= Se rechaza la hipotesis nula, 
# ya que el peso neto medio de los costales si es menor a 80 kg, 
# de esta forma se acepta la hipotesis alternativa

# 7.- Redacte una conclusión en el contexto del problema,indicando si existe 
# evidencia estadística suficiente para afirmar que los costales contienen,
# en promedio, menos de 80 kg.

