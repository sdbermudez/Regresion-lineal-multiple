library(dplyr)
library(knitr)
library(readr)
library(ggplot2)
library(patchwork) # PAra varias gráficos en el mismo renglón

#Se llevó a cabo un conjunto de ensayos experimentales con un 
#horno para determinar una forma de predecir el tiempo de cocción, y, 
#a diferentes niveles de ancho del horno, x1,y a diferentes temperaturas, x2.
#Se registraron los siguientes datos:
yp <-c(6.40, 15.05, 18.75, 30.25, 44.85, 48.85, 51.55, 61.50, 100.44, 111.42)
x1 <-c(1.32, 2.69, 3.56, 4.41, 5.35, 6.20, 7.12, 8.87, 9.80, 10.65)
x2 <-c(1.15, 3.40, 4.10, 8.75, 14.82, 15.15, 15.32, 18.18, 35.19, 40.40)
datos<-data.frame(yp, x1, x2)
kable(datos, caption = "Factores que influyen en el tiempo de coccion segun 
      diferentes niveles de ancho del horno y diferentes temperaturas")

g1 <- ggplot(data = datos, mapping = aes(x = x1, y = yp)) +
  geom_point(color = "forestgreen", size = 2) +
  labs(title  =  'yp ~ x1', x  =  'x1') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

g2 <- ggplot(data = datos, mapping = aes(x = x2, y = yp)) +
  geom_point(color = "orange", size = 2) +
  labs(title  =  'yp ~ x2', x  =  'x2') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g1+g2

data_independiente <- data.frame(x1,x2)
cor(data_independiente,method = "pearson")


modelo <- lm(formula = yp ~ ., data = datos)
summary(modelo)


modelo2 <- lm(formula = yp ~ x2, data = datos)
summary(modelo2)

anova(modelo, modelo2)

modelo_nuevo <- lm(formula = yp ~ x1 + x2 -1, data = datos)
summary(modelo_nuevo)

# Get the model residuals
model_residuals = modelo_nuevo$residuals
# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals)
