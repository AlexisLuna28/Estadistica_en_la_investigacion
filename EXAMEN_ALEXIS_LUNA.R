# Jorge Alexis Luna Robles
# matricula: 2134500
# fecha 27/10/2026
datos <- data.frame(
  Parcela = sprintf("P%02d", 1:30),
  Tratamiento = factor(
    rep(c("Testigo", "Ligero", "Moderado"), each = 10),
    levels = c("Testigo", "Ligero", "Moderado")), 
  Area_basal_inicial = c(32, 26, 39, 29, 43, 35, 28, 41, 34, 37,
                         31, 42, 27, 38, 33, 45, 29, 40, 36, 44,
                         30, 37, 46, 28, 41, 34, 43, 32, 39, 35), 
  Incremento = c(
        1.32, 1.16, 1.61, 1.42, 1.59,
        1.27, 1.35, 1.54, 1.43, 1.62,
        1.48, 1.72, 1.39, 1.82, 1.61,
        1.53, 1.88, 1.67, 1.59, 1.76,
        1.57, 1.93, 1.81, 2.08, 1.69,
        1.84, 2.16, 1.73, 1.98, 1.88))

Testigo <- subset(datos, Tratamiento == "Testigo")
dos_grupos <- droplevels(
  subset(datos, Tratamiento %in% c("Testigo", "Moderado")))

str(datos)

table(datos$Tratamiento)

# Actividad A. Estadística descriptiva

tapply(datos$Incremento, datos$Tratamiento, mean)
tapply(datos$Incremento, datos$Tratamiento, median)
tapply(datos$Incremento, datos$Tratamiento, sd)
tapply(datos$Incremento, datos$Tratamiento, var)
tapply(datos$Incremento, datos$Tratamiento, min)
tapply(datos$Incremento, datos$Tratamiento, max)
CV_pt <- tapply(
  datos$Incremento, datos$Tratamiento,
  function(x) 100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
print(CV_pt)

# RESPUESTAS
# Mayor Media : MODERADO
# Mayor dispersión absoluta:  MODERADO
# Mayor dispersión relativa Testigo

# Actividad B. Representación Gráfica

op <- par(no.readonly = TRUE)
par(mar = c(5, 7, 3, 2))

boxplot(
  Incremento ~ Tratamiento,
  data = datos,
  col = c("purple", "green", "blue"),
  xlab = "Tratamiento",
  ylab = expression(
    "Incremento de área basal (" *
      plain(m)^2 ~ plain(ha)^{-1} ~ "año"^{-1} * ")"),
  outline = FALSE)

set.seed(123)

stripchart(
  Incremento ~ Tratamiento,
  data = datos,
  vertical = TRUE,
  method = "jitter",
  jitter = 0.10,
  pch = 16,
  col = "black",
  add = TRUE)

modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = Testigo)

plot(
  Testigo$Area_basal_inicial, Testigo$Incremento,
  pch = 16, col = "green",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "red", lwd = 2)

# RESPUESTAS
# Punto 1 cajas y puntos
# Las medianas se aumentan desde el Testigo hasta el Moderado
# La dispersion absoluta es mayor en el moderado y 
# la variabilidad relativa en el Testigo.
# Se presenta superposicion de los intervalos entre los tratamientos 
# testigo con ligero, y ligero con moderado.
# No se presentan valores atipicos ya que las lineas alcanzan 
# el punto mas alto y mas bajo.

# Punto 2 
# La direccion es positiva entre el area basal y el incremento
# La forma es aproximadamente lineal
# Los puntos muestran una dispersion alrededor de la linea, por lo tanto, 
# a medida que aumenta el area basal tiende a aumentar el incremento
# de area basal

# Actividad C Correlación de Pearson
# Hipótesis nula (H_0): ρ=0. No existe una correlación lineal 
# significativa entre el área basal inicial y el incremento 
# del área basal en el tratamiento testigo.

# Hipótesis alternativa (H_1): ρ≠0. Existe una correlación lineal 
# significativa entre el área basal inicial y el incremento 
# del área basal en el tratamiento testigo.

resultado_cor <- cor.test(
  Testigo$Area_basal_inicial, Testigo$Incremento,
  method = "pearson", alternative = "two.sided")
resultado_cor

# Respuestas 
# valor de p: 0.005954
# valor t: 3.7103
# grados de libertad: 8
# Intervalos de confianza 0.3317795 y 0.9494747
# Direccion: correlacion positiva, cuando aumenta el area basal tambien
# aumenta el incremento
# Intensidad: la  intensidad es fuerte, ya que el coeficiente 
# de pearson es r= 0.7952751

# Actividad D Regresión lineal simple
modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = Testigo)
summary(modelo_reg)

confint(modelo_reg)

plot(
  Testigo$Area_basal_inicial, Testigo$Incremento,
  pch = 16, col = "blue",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "red", lwd = 2)

# Estimación dentro del intervalo observado
predict(
  modelo_reg,
  newdata = data.frame(Area_basal_inicial = 36),
  interval = "confidence", level = 0.95)
# Comparación de dos muestras independientes
resultado_t <- t.test(
  Incremento ~ Tratamiento, data = dos_grupos,
  alternative = "two.sided", var.equal = TRUE,
  conf.level = 0.95
)
resultado_t

# Respuestas
# valor de p: 1.807e-05
# media testigo: 1.431, media moderado: 1.867
# intervalos de confianza -0.5947243   -0.2772757

# Comparación de tres muestras independientes
modelo_anova <- aov(Incremento ~ Tratamiento, data = datos)
summary(modelo_anova)

# Respuestas
# valor de f: 17.61
# grados de libertad: 2
# tratamiento medias 0.4753 y media residuales: 0.0270

# Evaluación de supuestos
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))  
plot(fitted(modelo_reg), resid(modelo_reg), pch = 16,
     xlab = "Valores ajustados", ylab = "Residuos",  
     main = "Regresión: residuos vs ajustados") 
abline(h = 0, lty = 2, col = "red")  
qqnorm(resid(modelo_reg), pch = 16, main = "Regresión: Q-Q de residuos") 
qqline(resid(modelo_reg), col = "red")  
plot(fitted(modelo_anova), resid(modelo_anova), pch = 16,     
     xlab = "Valores ajustados", ylab = "Residuos",      
     main = "ANOVA: residuos vs ajustados") 

abline(h = 0, lty = 2, col = "red")  
qqnorm(resid(modelo_anova), pch = 16, main = "ANOVA: Q-Q de residuos") 
qqline(resid(modelo_anova), col = "red") 
 
