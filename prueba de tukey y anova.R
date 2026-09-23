# ANOVA

paraje <- read.csv("Datos_Rascon_Anova.csv", header = T)

# Los parajes serian el tratamiento

boxplot(paraje$DAP ~ paraje$Paraje)
range(paraje$EDAD)
range(paraje$DAP)

# Función tapply sirve para sacar la media o var de la variable en cada paraje

tapply(paraje$DAP, paraje$Paraje, mean)
tapply(paraje$DAP, paraje$Paraje, var)
tapply(paraje$DAP, paraje$Paraje, sd)

# Aplicar normalidad y homogeneidad

# HO: No hay diferencias
# H1: al menos en uno de los parajes hay diferencias

# aov
pa.aov <- aov(paraje$DAP ~ paraje$Paraje)
summary(pa.aov)

TukeyHSD(pa.aov)
plot(TukeyHSD(pa.aov))
