#Jorge Alexis Luna Robles
# Matricula: 2134500
# Fecha: 18/08/2026

# Importar datos ----
# Funcion read.csv importa datos de excl a R

IE <- read.csv("Datos/Crudos/vivero.csv", header = T)
IE$Tratamiento <- as.factor(IE$Tratamiento)

IE$planta
IE$IE
IE$Tratamiento

# Revisar solo una porcion de datos 
head(IE)  #Primeras filas de la BD IE
tail(IE)  #Ultimas filas de la BD IE

# Graficar ----

hist(IE$IE,
     main = " ",
     xlab = "Indice",
     ylab = "Frecuencia",
     ylim = c(0,12),
     col = "blue")

# Normalidad datos ----

shapiro.test(IE$IE)
mean(IE$IE)
sd(IE$IE)
var(IE$IE)
fivenum(IE$IE)

boxplot(IE$IE ~ IE$Tratamiento,
              col = "red",
              xlab = "Tratamientos",
              ylab = "Indice")

bartlett.test(IE$IE ~ IE$Tratamiento)
