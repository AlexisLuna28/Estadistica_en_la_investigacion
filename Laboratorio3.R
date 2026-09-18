# Nombre: Jorge Alexis Luna Robles
# Matricula: 2134500
# Fecha 07/09/2026

# Laboratorio Semana 3

# Parte 1: Importar datos Importar desde archivos csv

trees <- read.csv("DBH_1.csv", header=TRUE)
head(trees)

# Ingresar datos directo en la consola
dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1, 
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3, 
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

# Accesar datos de internet

url <- "https://www.agricultura.gob.mx/sites/default/files/sagarpa/document/2025/07/11/2245/14072025-actividades-inspeccion-movilizacion.csv"

inspeccion <- read.csv(url)

head(inspeccion)

# Otra forma de ingresar los url (acomodar url largos)

prof_url_2 <- paste0("https://www.agricultura.gob.mx/sites/default/",
  "files/sagarpa/document/2025/07/11/2245/",
  "14072025-actividades-inspeccion-movilizacion.csv")

senasica <- read.csv(prof_url_2)

# Ingresar datos desde Dropbox

library(repmis) 
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

head(conjunto) # Muestra las primeras 6 filas de la BD

# Descarga de datos desde GitHub
library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/", 
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read_csv(file)
head(inventario)


# Parte 2: Operaciones con la base de datos

mean(trees$dbh) # El signo de $ informa que necesitamos la columna dbh
sd(trees$dbh)

# Selección mediante restricciones
# Restricciones mas empleadas
# igual o mayor(>=),mayor que(>),igual que(==) 
# igual o menor(<=),menor que(<),no igual(!=)

# ¿Cuántos individuos tienen un diámetro menor(<) a 10 cm?
# Indica la sumatoria de los individuos en el objeto tree con un dbh < a 10
sum(trees$dbh <10)

# Saber cuales son los individuos que son inferiores al diámetro (dbh < 10 cm)
which(trees$dbh < 10)

# Excluir los diametros que se encuentran en la parcela 2
# El objeto resultante se puede grabar como trees.13

trees.13 <- trees[!(trees$parcela=="2"),]
trees.13

# Seleccion de una sola submuestra
# Se obtiene mediante la funcion subset
# Diámetros iguales o menores a 10 cm 

trees.1 <- subset(trees, dbh <= 10)
head(trees.1)

# Ahora tenemos dos bases de datos: trees y trees.1

mean(trees$dbh)
mean(trees.1$dbh)

# Representación gráfica de las dos bases de datos: trees y trees.1
hist(trees$dbh,
     main = "Muestra original trees",
     xlab = "DBH",
     ylab = "Frecuencia")

hist(trees.1$dbh,
     main = "dbh < 10 cm. trees.1",
     xlab = "DBH",
     ylab = "Frecuencia")

# Parte 3 Representación gráfica Histogramas
mamiferos<-read.csv("https://www.openintro.org/data/csv/mammals.csv")

# Variable total_sleep para generar el histograma

hist(mamiferos$total_sleep)

# mejorar código para generar un histograma mas presentable
hist(mamiferos$total_sleep, #Datos
     xlim = c(0,20), ylim = c(0,14), # Cambiar los limites de x & y
     main = "Total de horas sueño de las 39 especies", # Cambiar el titulo
     xlab = "Horas sueño", # Cambiar eje de las x
     ylab = "Frecuencia", # Cambiar eje de las0 y
     las = 1, # Cambiar orientacion de y
     col = "navajowhite") # Cambiar color de las barras

# Barplot o gráfico de barras
data("chickwts")
head(chickwts[c(1:2,42:43,62:64),])

# Acomodar datos en columnas
feeds <- table(chickwts$feed)
feeds

barplot(feeds)

# Ordenar de forma decreciente las barras
barplot(feeds[order(feeds, decreasing = TRUE)])

barplot(feeds[order(feeds)], 
    col = "#ffdb4d", main = "Frecuencias por tipo de\n alimentacion",
     xlab = "Número de pollos", las=1, horiz= TRUE, cex.names = 0.7, cex.axis =0.7 )

