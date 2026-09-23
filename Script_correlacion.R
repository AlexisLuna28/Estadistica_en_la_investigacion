# importar datos

datos <- read.csv("bosque.csv", header = T)


# Distribucion de pares de datos 
plot(datos$diametro, datos$altura,
     pch = 19, col= "red",
     xlab = "Variable independiente (dbh)",
     ylab = "Variable dependiente (h)")

# Pruebas de normalidad y boxplot de las variables

shapiro.test(datos$diametro)
boxplot(datos$diametro)
hist(datos$diametro)

shapiro.test(datos$altura)
boxplot(datos$altura)
hist(datos$altura)

datos$dbh_log <- log10(datos$diametro)
datos$dbh_sqr <- sqrt(datos$diametro)

hist(datos$dbh_log)
shapiro.test(datos$dbh_log)
shapiro.test(datos$dbh_sqr)
hist(datos$dbh_sqr)

# Prueba de correlación no paramétrica

cor.test(datos$diametro, datos$altura, method = "pearson")

# Datos de Geiser

# correlación si te esperas 10 minutos habrá alguna erupción
# Tiempo entre una erupcion y otra y su duración

# Faithful geyser

geyser <- faithful
View(faithful)

fivenum(geyser$eruptions)
fivenum(geyser$waiting)
mean(geyser$eruptions)
mean(geyser$waiting)

plot(geyser$waiting, geyser$eruptions,
     pch = 19, col = "red",
     xlab = "Tiempo de espera (min)",
     ylab = "Erupción (min)")

cor.test(geyser$eruptions, geyser$waiting)
cor.test(geyser$waiting, geyser$eruptions)

g.lm <- lm(geyser$eruptions ~ geyser$waiting)
summary(g.lm)

plot(geyser$waiting, geyser$eruptions,
     pch = 19, col = "lightgreen",
     xlab = "Tiempo de espera (min)",
     ylab = "Erupción (min)")
abline(g.lm, col = "red",
       lwd = 2)
g.lm$coefficients
g.lm$residuals

anova(g.lm)

geyser$yp <- g.lm$fitted.values
geyser$res <- g.lm$residuals
sum(geyser$res)

geyser$modelo <- -1.874016 + 0.07563*geyser$waiting

tiem <- c(45, 55, 68)
-1.874016 + 0.07563*tiem
