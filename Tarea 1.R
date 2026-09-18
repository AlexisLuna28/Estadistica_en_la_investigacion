# Nombre: Jorge Alexis Luna Robles
# Matricula:  2134500
# Fecha: 08/09/2026

# Ejercicio 3.1

# URL de los datos
url <- paste0("https://raw.githubusercontent.com/mgtagle/",
              "Estadistica_Investigacion_2026/refs/heads/main/", 
              "Datos/Crudos/cuadro1.csv")

# Importar los datos directamente desde GitHub
cuadro1 <- read.csv(url, header = TRUE)

# Revisar los datos
head(cuadro1)
str(cuadro1)

# Formule la hipótesis nula (H0) y la hipótesis alternativa (H1).

# H0: No se presentan diferencias significativas en los diámetros medios
# de los árboles de un rodal de bosque templado
# H1: Se presentan diferencias significativas en los diámetros medios 
# de los árboles de un rodal de bosque templado

#Histograma de la variable diámetro
hist(cuadro1$Diametro, main = "Histograma de la\n variable diámetro ",
     xlab = "Diámetro",
     ylab = "Frecuencia",
     ylim = c(0,15),
     xlim = c(5,25),
     col = "yellow")

# Prueba de normalidad
shapiro.test(cuadro1$Diametro)

# Media de la variable diámetro
mean(cuadro1$Diametro)

# Desviación estándar de la variable diámetro
sd(cuadro1$Diametro)

# Prueba de t

boxplot(cuadro1$Diametro)
t.test(cuadro1$Diametro, mu = 16.5, alternative = "two.sided")

#### Respuestas del ejercicio 3.1
# 1.- Formule la hipótesis nula (H0) y la hipótesis alternativa (H1)
# R= H0: No se presentan diferencias significativas en los diámetros medios
# de los árboles de un rodal de bosque templado
# R= H1: Se presentan diferencias significativas en los diámetros medios 
# de los árboles de un rodal de bosque templado
  
# 2.- La pregunta planteada requiere una prueba de una cola o de dos colas?
# R: Se requiere una prueba de dos colas, ya que solo se está indicando que 
# se requiere determinar si hay diferencias significativas en el diámetro (16.5)
# no especifica si es mayor o menor.
  
# 3.- Calcule la media y la desviación estándar de la variable Diametro
# R= Media: 15.794       Desviación estándar: 3.227017

# 4.- Realizar la prueba de t: se aplicó mediante la función de t.test
  
# 5.- Identificar en los resultados lo siguiente:
# Estadístico de t:  R= -1.547
# Grados de Libertad: R= 49
# Valor de p: R= 0.1283
# Media de la muestra: R= 15.794
# Intervalo de confianza del 95%: R= 14.87689 - 16.71111
  
# 6.- Con α = 0,05, indique si se rechaza o no se rechaza H0. 
# R= No se rechaza la hipótesis nula (H0) ya que no existen 
# diferencias significativas en la variable diámetro
  
# 7.- Redacte una conclusión en términos del diámetro medio de 
# los árboles del rodal
# R= De acuerdo con los resultados obtenidos en la prueba de t, el p-value 0.1283 
# es mayor al nivel de significancia de α=0.05, por lo que no se rechaza 
# la hipótesis nula. Si bien el diámetro medio de los árboles de la muestra 
# fue de 15.794, es un valor que no difiere significativamente del diámetro 
# medio de referencia de 16.5 establecido a partir de los registros históricos.

# Preguntas de reflexión 
# ¿El intervalo de confianza del 95% 
# contiene el valor de referencia de 16.5 cm? 
# R= Si, el valor de referencia de 16.5 se encuentra entre 
# los intervalos de confianza 14.87689 - 16.71111

# ¿Cómo se relaciona este resultado con la decisión 
# obtenida mediante el valor de p?
# R= Se relaciona mediante a la consistencia mutua de que si el valor de p
# es mayor a 0.05, el valor de referencia debe estar dentro de los intervalos 
# de confianza.

#####

# Ejercicio 3.2 

# Importar datos a R
costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 
            81.94, 80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62,
            81.4, 74.89, 82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28,
            78.44, 79.01, 80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7,
            82.45, 77.29, 75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

# Histograma 
hist(costal, main = "Histograma del peso\n de los costales ",
     xlab = "Peso (kg)",
     ylab = "Frecuencia",
     ylim = c(0,15),
     xlim = c(68,90),
     col = "red")

# Prueba de normalidad
shapiro.test(costal)

# Media del peso neto medio de los costales
mean(costal)

# Desviación estándar del peso neto medio de los costales
sd(costal)

# Prueba de t
boxplot(costal)
t.test(costal, mu= 80, alternative = "less")

### Respuestas ejercicio 3.2

# 1.- Explique con sus propias palabras qué representan H0 y H1.
# Ho: No hay diferencia significativa en el peso medio de los costales, 
# el cual es igual a 80 kg.

# H1: Hay diferencia significativa en el peso medio de los costales, 
# resultando menor a 80 kg.

# 2.- ¿Por qué corresponde utilizar una prueba de una cola?  
# R= Se utiliza una prueba de una cola, ya que se especifica que se busca 
# verificar que el peso neto medio de los costales es menor a 80 kg

# 3.- Calcule la media y desviación estándar de los 44 costales.
# R= Media: 78.91068    Desviación estándar: 3.056023

# 4.- Realice la prueba: se aplicó mediante la funcion t.test

# 5.- Identifique: 
# Estadístico t: R= -2.3644
# Grados de libertad: R= 43 
# Valor de p: R= 0.01132 
# Media observada: R= 78.91068

# 6.- Compare el valor de p con α = 0,05.
# ¿Se rechaza o no se rechaza H0? R= Se rechaza la hipótesis nula, 
# ya que el peso neto medio de los costales si es menor a 80 kg.

# 7.- Redacte una conclusión en el contexto del problema, indicando si existe 
# evidencia estadística suficiente para afirmar que los costales contienen,
# en promedio, menos de 80 kg.
# R= Si existe evidencia estadística suficiente para afirmar que los costales 
# contienen, en promedio menos de 80 kg. Dado que el p value (0.0113) indica 
# rechazar la hipótesis nula en favor de la alternativa, demostrando que 
# el peso medio neto real de los costales de 78.91 es significativamente 
# menor al contenido declarado por la empresa de 80 kg, lo cual le da la razón 
# al consumidor a su sospecha de que el contenido era menor.

