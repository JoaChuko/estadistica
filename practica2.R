rm(list=ls())

# Creo una muestra
set.seed(123)  # Para hacer que los resultados sean reproducibles
muestra_exponencial <- rexp(100, rate = 1)  # Cambia el tamaño de la muestra y el valor de rate según tus necesidades
datos <- data.frame(Valores = muestra_exponencial)

# Funcion empirica
funcion_empirica <- ecdf(muestra_exponencial)

# Creo el grafico
library(ggplot2)


ggplot(datos, aes(x = Valores)) +
  geom_function(fun=funcion_empirica, color ="blue", size = 1) +
  geom_function(fun = pexp, args = list(rate = 1), color = "red", size = 1) +
  labs(title = "Distribución Exponencial",
       x = "Valores",
       y = "Probabilidad Acumulada") +
  theme_minimal()

# Estimadores

Est= 1/(mean(muestra_exponencial))

dens_est = density(muestra_exponencial)
distribucion = function(t){
  integrate(dens_est, -Inf, t)
}

library(ggplot2)

ggplot(datos, aes(x = Valores)) + 
  geom_function(fun = distribucion, color= "green", size=1) +
  geom_function(fun = funcion_empirica, color = "blue", size = 1) +
  geom_function(fun = pexp, args = list(rate = 1), color = "red", size = 1) +
  labs(title = "Distribución Exponencial",
       x = "Valores",
       y = "Probabilidad Acumulada") +
  theme_minimal()
  
#chequear como hacer la empirica y como aproximar la densidad
